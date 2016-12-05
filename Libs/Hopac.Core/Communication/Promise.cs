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

    internal const int Delayed = 0;
    internal const int Running = 1;
    internal const int HasValue = 2;
    internal const int HasExn = 3;
    internal const int MakeLocked = 4;  // Greater than any state.

    /// <summary>Creates a promise that will never be fulfilled.</summary>
    [MethodImpl(AggressiveInlining.Flag)]
    public Promise() { this.State = Running; }

    /// <summary>Creates a promise whose value is computed lazily with the given
    /// job when an attempt is made to read the promise.  Although the job is
    /// not started immediately, the effect is that the delayed job will be run
    /// as a separate job, which means it is possible to communicate with it as
    /// long the delayed job is started before trying to communicate with
    /// it.</summary>
    [MethodImpl(AggressiveInlining.Flag)]
    public Promise(Job<T> tJ) {
      this.Readers = new Fulfill(tJ);
    }

    /// <summary>Creates a promise with the given value.</summary>
    [MethodImpl(AggressiveInlining.Flag)]
    public Promise(T value) {
      this.Value = value;
      this.State = HasValue;
    }

    /// <summary>Creates a promise with the given failure exception.</summary>
    [MethodImpl(AggressiveInlining.Flag)]
    public Promise(Exception e) {
      this.Readers = new Fail<T>(e); // We assume failures are infrequent.
      this.State = HasExn;
    }

    /// Internal implementation detail.
    public bool Full {
      [MethodImpl(AggressiveInlining.Flag)]
      get { return HasValue <= State; }
    }

    [MethodImpl(AggressiveInlining.Flag)]
    internal T Get() {
      if (HasValue == State)
        return Value;
      throw (this.Readers as Fail<T>).exn;
    }

    [MethodImpl(AggressiveInlining.Flag)]
    internal void UnsafeAddReader(Cont<T> tK) {
      WaitQueue.AddTaker(ref this.Readers, tK);
    }

    internal override void DoJob(ref Worker wr, Cont<T> aK) {
    Spin:
      var state = this.State;
    Reconsider:
      if (state > Running) goto Completed;
      if (state < Delayed) goto Spin;
      var check = state;
      state = Interlocked.CompareExchange(ref this.State, state-MakeLocked, state);
      if (Delayed == state) goto Delayed;
      if (state != check) goto Reconsider;

      WaitQueue.AddTaker(ref this.Readers, aK);
      this.State = Running;
      return;

    Delayed:
      var readers = this.Readers;
      this.Readers = null;
      this.State = Running;

      var fulfill = readers as Fulfill;
      fulfill.tP = this;
      fulfill.reader = aK;
      var tJ = fulfill.tJ;
      fulfill.tJ = null;
      Job.Do(tJ, ref wr, fulfill);
      return;

    Completed:
      if (state == HasValue)
        Cont.Do(aK, ref wr, this.Value);
      else
        aK.DoHandle(ref wr, (this.Readers as Fail<T>).exn);
    }

    internal override void TryAlt(ref Worker wr, int i, Cont<T> aK, Else aE) {
    Spin:
      var state = this.State;
    Reconsider:
      if (state > Running) goto Completed;
      if (state < Delayed) goto Spin;
      var check = state;
      state = Interlocked.CompareExchange(ref this.State, state-MakeLocked, state);
      if (Delayed == state) goto Delayed;
      if (state != check) goto Reconsider;

      WaitQueue.AddTaker(ref this.Readers, i, aE.pk, aK);
      this.State = Running;
      aE.TryElse(ref wr, i + 1);
      return;

    Delayed:
      var readers = this.Readers;

      var taker = new Taker<T>();
      this.Readers = taker;
      taker.Next = taker;
      taker.Pick = aE.pk;
      taker.Me = i;
      taker.Cont = aK;
      this.State = Running;

      var fulfill = readers as Fulfill;
      fulfill.tP = this;

      Worker.PushNew(ref wr, fulfill);
      aE.TryElse(ref wr, i + i);
      return;

    Completed:
      var pkSelf = aE.pk;
    TryPick:
      var stSelf = Pick.TryPick(pkSelf);
      if (stSelf > 0) goto AlreadyPicked;
      if (stSelf < 0) goto TryPick;

      Pick.SetNacks(ref wr, i, pkSelf);

      if (state == HasValue)
        Cont.Do(aK, ref wr, this.Value);
      else
        aK.DoHandle(ref wr, (this.Readers as Fail<T>).exn);
    AlreadyPicked:
      return;
    }

    internal sealed class Fulfill : Cont<T> {
      internal Promise<T> tP;
      internal Job<T> tJ;

      private Cont<Unit> procFin;

      internal Cont<T> reader;
      internal int me;
      internal Pick pk;

      [MethodImpl(AggressiveInlining.Flag)]
      internal Fulfill(Promise<T> tP) {
        this.tP = tP;
      }

      [MethodImpl(AggressiveInlining.Flag)]
      internal Fulfill(Job<T> tJ) {
        this.tJ = tJ;
      }

      [MethodImpl(AggressiveInlining.Flag)]
      internal Fulfill(Promise<T> tP, Job<T> tJ) {
        this.tP = tP;
        this.tJ = tJ;
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
        if (state != Running) goto Spin;
        if (Running != Interlocked.CompareExchange(ref tP.State, HasValue, state)) goto Spin;

        WaitQueue.PickReaders(ref tP.Readers, tP.Value, ref wr);

        var pk = this.pk;
        if (null == pk) goto MaybeGotReader;
      TryPick:
        var st = Pick.TryPick(pk);
        if (st > 0) goto Done;
        if (st < 0) goto TryPick;

        Pick.SetNacks(ref wr, me, pk);

      MaybeGotReader:
        var reader = this.reader;
        if (null == reader) goto Done;
        wr.Handler = reader;
        reader.DoCont(ref wr, tP.Value);
      Done:
        return;
      }

      internal override void DoWork(ref Worker wr) {
        var tJ = this.tJ;
        if (null == tJ) {
          Do(ref wr, this.Value);
        } else {
          this.tJ = null;
          tJ.DoJob(ref wr, this);
        }
      }

      internal override void DoCont(ref Worker wr, T v) {
        Do(ref wr, v);
      }

      internal override void DoHandle(ref Worker wr, Exception e) {
        var tP = this.tP as Promise<T>;
      Spin:
        var state = tP.State;
        if (state != Running) goto Spin;
        if (Running != Interlocked.CompareExchange(ref tP.State, state-MakeLocked, state)) goto Spin;

        var readers = tP.Readers;
        tP.Readers = new Fail<T>(e);
        tP.State = HasExn;

        WaitQueue.FailReaders(readers, e, ref wr);

        var reader = this.reader;
        if (null == reader) return;
        wr.Handler = reader;
        reader.DoHandle(ref wr, e);
      }
    }
  }
}
