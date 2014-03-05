// Copyright (C) by Housemarque, Inc.

namespace Hopac {
  using Microsoft.FSharp.Core;
  using Hopac.Core;
  using System.Runtime.CompilerServices;
  using System.Threading;
  using System;

  /// <summary>A synchronized write once variable.</summary>
  public class IVar<T> : Alt<T> {
    internal T Value;
    internal volatile int State;
    internal Cont<T> Takers; // Takers -> Readers

    internal const int Locked = -1;
    internal const int Empty = 0;
    internal const int HasValue = 1;

    internal override void DoJob(ref Worker wr, Cont<T> aK) {
    Spin:
      var state = this.State;
      if (state > Empty) goto GotValue;
      if (state < Empty) goto Spin;
      if (Empty != Interlocked.CompareExchange(ref this.State, Locked, Empty)) goto Spin;

      WaitQueue.AddTaker(ref this.Takers, aK);
      this.State = Empty;
      return;

    GotValue:
      Cont.Do(aK, ref wr, this.Value);
      return;
    }

    internal override void TryAlt(ref Worker wr, int i, Pick pkSelf, Cont<T> aK, Else<T> aE) {
    Spin:
      var state = this.State;
      if (state > Empty) goto TryPick;
      if (state < Empty) goto Spin;
      if (Empty != Interlocked.CompareExchange(ref this.State, Locked, Empty)) goto Spin;

      WaitQueue.AddTaker(ref this.Takers, i, pkSelf, aK);
      this.State = Empty;
      aE.TryElse(ref wr, i + 1, pkSelf, aK);
      return;

    TryPick:
      var st = Pick.TryPick(pkSelf);
      if (st > 0) goto AlreadyPicked;
      if (st < 0) goto TryPick;

      Pick.SetNacks(ref wr, i, pkSelf);

      Cont.Do(aK, ref wr, this.Value);
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
        if (state < IVar<T>.Empty)
          goto Spin;
        if (state != Interlocked.CompareExchange(ref iv.State, IVar<T>.HasValue, state))
          goto Spin;

        if (state > IVar<T>.Empty)
          goto IVarFull;

        var takers = iv.Takers;
        if (null == takers)
          goto Empty;
        iv.Takers = null;
        int me = 0;
        Work cursor = takers;
      TryTaker:
        var taker = cursor as Cont<T>;
        cursor = cursor.Next;
        var pk = taker.GetPick(ref me);
        if (null == pk)
          goto GotTaker;

      TryPick:
        var st = Pick.TryPick(pk);
        if (st > 0) goto TryNextTaker;
        if (st < 0) goto TryPick;

        Pick.SetNacks(ref wr, me, pk);
      GotTaker:
        taker.Value = iv.Value;
        Worker.Push(ref wr, taker);

      TryNextTaker:
        if (cursor != takers)
          goto TryTaker;

      Empty:
        Work.Do(uK, ref wr);
        return;

      IVarFull:
        uK.DoHandle(ref wr, new Exception("IVar full"));
      }
    }
  }
}