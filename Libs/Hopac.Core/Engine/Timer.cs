// Copyright (C) by Housemarque, Inc.

namespace Hopac.Core {
  using Microsoft.FSharp.Core;
  using System.Diagnostics;
  using System.Runtime.CompilerServices;
  using System.Threading;
  using System;

  internal class Timer : Job<int> {
    internal WorkTimed TopTimed;
    internal SpinlockTTAS Lock;
    internal Scheduler Scheduler;

    internal Timer(Scheduler sr) {
      this.Scheduler = sr;
      sr.IdleHandler = this;
    }

    internal override void DoJob(ref Worker wr, Cont<int> iK) {
    TryNextTimed:
      var waitTicks = Timeout.Infinite;
      var tt = this.TopTimed;
      if (null == tt)
        goto Return;

      var tickCount = Environment.TickCount;
      waitTicks = tt.Ticks - tickCount;
      if (waitTicks > 0)
        goto Return;

      this.Lock.Enter();
      tt = this.TopTimed;
      if (null == tt)
        goto ExitAndReturnWithInfinite;

      waitTicks = tt.Ticks - tickCount;
      if (waitTicks > 0)
        goto ExitAndReturn;

      DropTimed();
      this.Lock.Exit();

      var pk = tt.Pick;
      if (null == pk)
        goto GotIt;

    TryPick:
      var st = Pick.TryPick(pk);
      if (st > 0) goto TryNextTimed;
      if (st < 0) goto TryPick;

      Pick.SetNacks(ref wr, tt.Me, pk);

    GotIt:
      Worker.Push(ref wr, tt);
      iK.DoCont(ref wr, 0);
      return;

    ExitAndReturnWithInfinite:
      waitTicks = Timeout.Infinite;
    ExitAndReturn:
      this.Lock.Exit();
    Return:
      iK.DoCont(ref wr, waitTicks);
    }

    [MethodImpl(AggressiveInlining.Flag)]
    internal void SynchronizedPushTimed(WorkTimed x) {
      this.Lock.Enter();
      var o = TopTimed;
      var n = Merge(TopTimed, x);
      TopTimed = n;
      if (o != n)
        Scheduler.Signal(this.Scheduler);
      this.Lock.Exit();
    }

    [MethodImpl(AggressiveInlining.Flag)]
    private void DropTimed() {
      var top = TopTimed;
      var n = Merge(top.Next as WorkTimed, top.Right);
      TopTimed = n;
      if (null != n)
        Scheduler.Signal(this.Scheduler);
    }

    private static WorkTimed Merge(WorkTimed x, WorkTimed y) {
      if (IsAbandoned(x) > 0) x = Merge(x.Next as WorkTimed, x.Right);
      if (IsAbandoned(y) > 0) y = Merge(y.Next as WorkTimed, y.Right);

      if (null == x) return y;
      if (null == y) return x;

      if (x.Ticks - y.Ticks > 0) {
        var t = x; x = y; y = t;
      }

      x.Right = Merge(x.Right, y);

      if (null == x.Next) {
        x.Next = x.Right;
        x.Right = null;
      } else if (null != x.Right) {
        var x_Next = x.Next as WorkTimed;
        if (x_Next.Rank < x.Right.Rank) {
          var t = x_Next;
          x.Next = x.Right;
          x.Right = t;
        }
        x.Rank = x.Right.Rank + 1;
      }
      return x;
    }

    [MethodImpl(AggressiveInlining.Flag)]
    private static int IsAbandoned(WorkTimed x) {
      if (null == x)
        return 0;
      var pk = x.Pick;
      if (null == pk)
        return 0;
      return pk.State;
    }
  }

  internal abstract class WorkTimed : Work {
    internal WorkTimed Right;
    internal int Ticks;
    internal int Rank;
    internal Pick Pick;
    internal int Me;

    [MethodImpl(AggressiveInlining.Flag)]
    internal WorkTimed(int ticks) {
      this.Ticks = ticks;
    }

    [MethodImpl(AggressiveInlining.Flag)]
    internal WorkTimed(int ticks, int me, Pick pk) {
      this.Ticks = ticks;
      this.Me = me;
      this.Pick = pk;
    }
  }
}
