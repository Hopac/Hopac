// Copyright (C) by Housemarque, Inc.

namespace Hopac {
  using Microsoft.FSharp.Core;
  using Hopac.Core;
  using System.Runtime.CompilerServices;
  using System.Threading;
  using System;

  /// <summary>Represents a write once variable.</summary>
  public class IVar<T> : Alt<T> {
    internal T Value;
    internal volatile int State;
    internal Cont<T> Readers;

    internal const int Locked = -1;
    internal const int Empty = 0;
    internal const int HasValue = 1;
    internal const int HasExn = 2;

    ///
    public IVar() { }
    ///
    public IVar(T t) {
      this.Value = t;
      this.State = HasValue;
    }
    ///
    public IVar(Exception e) {
      this.Readers = new Fail<T>(e);
      this.State = HasExn;
    }

    ///
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
      if (state > Empty) goto GotValueOrExn;
      if (state < Empty) goto Spin;
      if (Empty != Interlocked.CompareExchange(ref this.State, Locked, Empty)) goto Spin;

      WaitQueue.AddTaker(ref this.Readers, aK);
      this.State = Empty;
      return;

    GotValueOrExn:
      if (state == HasValue)
        Cont.Do(aK, ref wr, this.Value);
      else
        aK.DoHandle(ref wr, (this.Readers as Fail<T>).exn);
    }

    internal override void TryAlt(ref Worker wr, int i, Cont<T> aK, Else aE) {
      var pkSelf = aE.pk;
    Spin:
      var state = this.State;
      if (state > Empty) goto TryPick;
      if (state < Empty) goto Spin;
      if (Empty != Interlocked.CompareExchange(ref this.State, Locked, Empty)) goto Spin;

      WaitQueue.AddTaker(ref this.Readers, i, pkSelf, aK);
      this.State = Empty;
      aE.TryElse(ref wr, i + 1);
      return;

    TryPick:
      var st = Pick.TryPick(pkSelf);
      if (st > 0) goto AlreadyPicked;
      if (st < 0) goto TryPick;

      Pick.SetNacks(ref wr, i, pkSelf);

      if (state == HasValue)
        Cont.Do(aK, ref wr, this.Value);
      else
        aK.DoHandle(ref wr, (this.Readers as Fail<T>).exn);
    AlreadyPicked:
      return;
    }
  }

  namespace Core {
    /// Internal implementation detail.
    public class IVarFill<T> : Job<Unit> {
      internal readonly IVar<T> IV;
      internal readonly T X;

      /// Internal implementation detail.
      [MethodImpl(AggressiveInlining.Flag)]
      public IVarFill(IVar<T> iv, T x) {
        this.IV = iv;
        this.X = x;
      }

      [MethodImpl(AggressiveInlining.Flag)]
      internal static void ProcessReaders(IVar<T> iv, ref Worker wr) {
        var readers = iv.Readers;
        if (null == readers) return;
        iv.Readers = null;
        int me = 0;
        Work cursor = readers;
      TryReader:
        var reader = cursor as Cont<T>;
        cursor = cursor.Next;
        var pk = reader.GetPick(ref me);
        if (null == pk)
          goto GotReader;

      TryPick:
        var st = Pick.TryPick(pk);
        if (st > 0) goto TryNextReader;
        if (st < 0) goto TryPick;

        Pick.SetNacks(ref wr, me, pk);
      GotReader:
        reader.Value = iv.Value;
        Worker.Push(ref wr, reader);

      TryNextReader:
        if (cursor != readers)
          goto TryReader;
      }

      internal override void DoJob(ref Worker wr, Cont<Unit> uK) {
        var iv = this.IV;
        iv.Value = this.X; // This assumes correct usage of IVar.
      Spin:
        var state = iv.State;
        if (state < IVar<T>.Empty) goto Spin;
        if (state != Interlocked.CompareExchange(ref iv.State, IVar<T>.HasValue, state)) goto Spin;

        if (state > IVar<T>.Empty) {
          uK.DoHandle(ref wr, new Exception("IVar full"));
        } else {
          ProcessReaders(iv, ref wr);
          Work.Do(uK, ref wr);
        }
      }
    }

    /// Internal implementation detail.
    public sealed class IVarTryFill<T> : IVarFill<T> {

      /// Internal implementation detail.
      [MethodImpl(AggressiveInlining.Flag)]
      public IVarTryFill(IVar<T> iv, T x) : base (iv, x) { }

      internal override void DoJob(ref Worker wr, Cont<Unit> uK) {
        var iv = this.IV;
      Spin:
        var state = iv.State;
        if (state < IVar<T>.Empty) goto Spin;
        if (state > IVar<T>.Empty) goto Done;
        if (state != Interlocked.CompareExchange(ref iv.State, IVar<T>.Locked, state)) goto Spin;

        iv.Value = this.X;
        iv.State = IVar<T>.HasValue;

        ProcessReaders(iv, ref wr);
      Done:
        Work.Do(uK, ref wr);
      }
    }

    /// Internal implementation detail.
    public sealed class IVarFillFailure<X> : Job<Unit> {
      private readonly IVar<X> xI;
      private readonly Exception e;

      /// Internal implementation detail.
      [MethodImpl(AggressiveInlining.Flag)]
      public IVarFillFailure(IVar<X> xI, Exception e) {
        this.xI = xI;
        this.e = e;
      }

      internal override void DoJob(ref Worker wr, Cont<Unit> uK) {
        var xI = this.xI;
      Spin:
        var state = xI.State;
        if (state < IVar<X>.Empty) goto Spin;
        if (state != Interlocked.CompareExchange(ref xI.State, IVar<X>.Locked, state)) goto Spin;

        if (state > IVar<X>.Empty) goto IVarFull;

        var readers = xI.Readers;
        xI.Readers = new Fail<X>(this.e);
        xI.State = IVar<X>.HasExn;

        if (null == readers) goto Empty;
        int me = 0;
        Work cursor = readers;
      TryReader:
        var reader = cursor as Cont<X>;
        cursor = cursor.Next;
        var pk = reader.GetPick(ref me);
        if (null == pk) goto GotReader;

      TryPick:
        var st = Pick.TryPick(pk);
        if (st > 0) goto TryNextReader;
        if (st < 0) goto TryPick;

        Pick.SetNacks(ref wr, me, pk);
      GotReader:
        Worker.Push(ref wr, new FailWork(this.e, reader));

      TryNextReader:
        if (cursor != readers) goto TryReader;

      Empty:
        Work.Do(uK, ref wr);
        return;

      IVarFull:
        uK.DoHandle(ref wr, new Exception("IVar full"));
      }
    }
  }
}