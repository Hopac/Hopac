// Copyright (C) by Housemarque, Inc.

namespace Hopac {
  using Microsoft.FSharp.Core;
  using System;
  using System.Runtime.CompilerServices;
  using System.Threading;
  using Hopac.Core;

  /// <summary>Represents a promise to produce a result at some point in the
  /// future.</summary>
  public class Promise<T> : Alt<T> {
    internal T Value;
    internal volatile int State;
    internal Cont<T> Readers;

    internal const int Locked = ~Empty;
    internal const int Empty = 0;
    internal const int HasValue = 1;
    internal const int HasExn = 2;

    [MethodImpl(AggressiveInlining.Flag)]
    internal Promise() { }

    /// Internal implementation detail.
    [MethodImpl(AggressiveInlining.Flag)]
    public Promise(T value) {
      this.State = HasValue;
      this.Value = value;
    }

    /// Internal implementation detail.
    [MethodImpl(AggressiveInlining.Flag)]
    public Promise(Exception e) {
      this.Readers = new Fail<T>(e); // We assume failures are infrequent.
      this.State = HasExn;
    }

    /// Internal implementation detail.
    public bool Full {
      [MethodImpl(AggressiveInlining.Flag)]
      get { return Empty < State; }
    }

    [MethodImpl(AggressiveInlining.Flag)]
    internal T Get() {
      if (HasValue == State)
        return Value;
      throw (this.Readers as Fail<T>).exn;
    }

    internal override void DoJob(ref Worker wr, Cont<T> aK) {
    Spin:
      var state = this.State;
      if (state > Empty) goto Completed;
      if (state < Empty) goto Spin;
      if (state != Interlocked.CompareExchange(ref this.State, ~state, state)) goto Spin;

      WaitQueue.AddTaker(ref this.Readers, aK);
      this.State = Empty;
      return;

    Completed:
      if (state == HasValue)
        Cont.Do(aK, ref wr, this.Value);
      else
        aK.DoHandle(ref wr, (this.Readers as Fail<T>).exn);
    }

    internal override void TryAlt(ref Worker wr, int i, Cont<T> aK, Else aE) {
      var pkSelf = aE.pk;
    Spin:
      var state = this.State;
      if (state > Empty) goto Completed;
      if (state < Empty) goto Spin;
      if (state != Interlocked.CompareExchange(ref this.State, ~state, state)) goto Spin;

      WaitQueue.AddTaker(ref this.Readers, i, pkSelf, aK);
      this.State = Empty;
      aE.TryElse(ref wr, i + 1);
      return;

    Completed:
      var stSelf = Pick.TryPick(pkSelf);
      if (stSelf > 0) goto AlreadyPicked;
      if (stSelf < 0) goto Completed;

      Pick.SetNacks(ref wr, i, pkSelf);

      if (state == HasValue)
        Cont.Do(aK, ref wr, this.Value);
      else
        aK.DoHandle(ref wr, (this.Readers as Fail<T>).exn);
    AlreadyPicked:
      return;
    }

    internal sealed class Fulfill : Cont<T> {
      private readonly Promise<T> tP;
      private Cont<Unit> procFin;

      [MethodImpl(AggressiveInlining.Flag)]
      internal Fulfill(Promise<T> tP) {
        this.tP = tP;
      }

      internal override Proc GetProc(ref Worker wr) {
        return Handler.GetProc(ref wr, ref this.procFin);
      }

      [MethodImpl(AggressiveInlining.Flag)]
      internal void Do(ref Worker wr, T t) {
        var tP = this.tP;
        tP.Value = t;
      Spin:
        var state = tP.State;
        if (state != Empty) goto Spin;
        if (Empty != Interlocked.CompareExchange(ref tP.State, HasValue, state)) goto Spin;

        WaitQueue.PickReaders(ref tP.Readers, tP.Value, ref wr);
      }

      internal override void DoWork(ref Worker wr) {
        Do(ref wr, this.Value);
      }

      internal override void DoCont(ref Worker wr, T v) {
        Do(ref wr, v);
      }

      internal override void DoHandle(ref Worker wr, Exception e) {
        var tP = this.tP;
      Spin:
        var state = tP.State;
        if (state != Empty) goto Spin;
        if (Empty != Interlocked.CompareExchange(ref tP.State, Locked, state)) goto Spin;

        var readers = tP.Readers;
        tP.Readers = new Fail<T>(e);
        tP.State = HasExn;

        WaitQueue.FailReaders(readers, e, ref wr);
      }
    }
  }
}
