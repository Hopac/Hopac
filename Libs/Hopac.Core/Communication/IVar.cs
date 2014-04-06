// Copyright (C) by Housemarque, Inc.

namespace Hopac {
  using Microsoft.FSharp.Core;
  using Hopac.Core;
  using System.Runtime.CompilerServices;
  using System.Threading;
  using System;

  /// <summary>Represents a synchronized write once variable.</summary>
  public class IVar<T> : Alt<T> {
    internal T Value;
    internal volatile int State;
    internal Cont<T> Readers;

    internal const int Locked = -1;
    internal const int Empty = 0;
    internal const int HasValue = 1;
    internal const int HasExn = 2;

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
        Handler.DoHandle(aK, ref wr, (this.Readers as Fail<T>).exn);
    }

    internal override void TryAlt(ref Worker wr, int i, Pick pkSelf, Cont<T> aK, Else<T> aE) {
    Spin:
      var state = this.State;
      if (state > Empty) goto TryPick;
      if (state < Empty) goto Spin;
      if (Empty != Interlocked.CompareExchange(ref this.State, Locked, Empty)) goto Spin;

      WaitQueue.AddTaker(ref this.Readers, i, pkSelf, aK);
      this.State = Empty;
      aE.TryElse(ref wr, i + 1, pkSelf, aK);
      return;

    TryPick:
      var st = Pick.TryPick(pkSelf);
      if (st > 0) goto AlreadyPicked;
      if (st < 0) goto TryPick;

      Pick.SetNacks(ref wr, i, pkSelf);

      if (state == HasValue)
        Cont.Do(aK, ref wr, this.Value);
      else
        Handler.DoHandle(aK, ref wr, (this.Readers as Fail<T>).exn);
    AlreadyPicked:
      return;
    }
  }

  namespace Core {
    /// Internal implementation detail.
    public sealed class IVarFill<T> : Job<Unit> {
      private readonly IVar<T> IV;
      private readonly T X;

      /// Internal implementation detail.
      [MethodImpl(AggressiveInlining.Flag)]
      public IVarFill(IVar<T> iv, T x) {
        this.IV = iv;
        this.X = x;
      }

      internal override void DoJob(ref Worker wr, Cont<Unit> uK) {
        var iv = this.IV;
        iv.Value = this.X; // This assumes correct usage of IVar.
      Spin:
        var state = iv.State;
        if (state < IVar<T>.Empty) goto Spin;
        if (state != Interlocked.CompareExchange(ref iv.State, IVar<T>.HasValue, state)) goto Spin;

        if (state > IVar<T>.Empty) goto IVarFull;

        var readers = iv.Readers;
        if (null == readers) goto Empty;
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

      Empty:
        Work.Do(uK, ref wr);
        return;

      IVarFull:
        Handler.DoHandle(uK, ref wr, new Exception("IVar full"));
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
        Handler.DoHandle(uK, ref wr, new Exception("IVar full"));
      }
    }
  }
}