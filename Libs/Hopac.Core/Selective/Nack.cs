// Copyright (C) by Housemarque, Inc.

namespace Hopac.Core {
  using Microsoft.FSharp.Core;
  using System.Diagnostics;
  using System.Runtime.CompilerServices;
  using System.Threading;
  using System;

  /// <summary>Internal implementation detail.</summary>
  public class Nack : Alt<Unit> {
    internal Nack Next;
    internal volatile int State;
    /// <summary>Internal implementation detail.</summary>
    public int I0;
    /// <summary>Internal implementation detail.</summary>
    public int I1;
    internal Cont<Unit> Takers;

    internal const int Locked = -1;
    internal const int Initial = 0;
    internal const int Signaled = 1;

    [MethodImpl(AggressiveInlining.Flag)]
    internal Nack(Nack next, int i0) {
      this.Next = next;
      this.I0 = i0;
      this.I1 = Int32.MaxValue;
    }

    /// <summary>Internal implementation detail.</summary>
    internal override void DoJob(ref Worker wr, Cont<Unit> uK) {
    Spin:
      var state = this.State;
      if (state > Initial) goto Signaled;
      if (state < Initial) goto Spin;
      if (Initial != Interlocked.CompareExchange(ref this.State, Locked, Initial)) goto Spin;

      WaitQueue.AddTaker(ref this.Takers, uK);
      this.State = Initial;
      return;

    Signaled:
      uK.DoCont(ref wr, null);
    }

    /// <summary>Internal implementation detail.</summary>
    internal override void TryAlt(ref Worker wr, int i, Pick pkSelf, Cont<Unit> uK) {
    Spin:
      var state = this.State;
      if (state > Initial) goto TryPick;
      if (state < Initial) goto Spin;
      if (Initial != Interlocked.CompareExchange(ref this.State, Locked, Initial)) goto Spin;

      WaitQueue.AddTaker(ref this.Takers, i, pkSelf, uK);
      this.State = Initial;
      uK.TryNext(ref wr, i + 1, pkSelf);
      return;

    TryPick:
      var st = Pick.TryPick(pkSelf);
      if (st > 0) goto AlreadyPicked;
      if (st < 0) goto TryPick;

      Pick.SetNacks(ref wr, i, pkSelf);

      uK.DoCont(ref wr, null);
    AlreadyPicked:
      return;
    }

    [MethodImpl(AggressiveInlining.Flag)]
    internal static void Signal(ref Worker wr, Nack nk) {
    Spin:
      var state = nk.State;
      if (state > Initial) goto JustExit; // XXX Can this happen?
      if (state < Initial) goto Spin;
      if (Initial != Interlocked.CompareExchange(ref nk.State, Signaled, Initial)) goto Spin;

      var takers = nk.Takers;
      if (null == takers)
        goto JustExit;
      nk.Takers = null;
      var me = 0;
      Work cursor = takers;

    TryTaker:
      var taker = cursor as Cont<Unit>;
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
      taker.Next = null;
      Worker.Push(ref wr, taker);
    TryNextTaker:
      if (cursor != takers) goto TryTaker;
    JustExit:
      return;
    }
  }
}
