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
    internal uint unique = 0;
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
      waitTicks = (int)(tt.Ticks >> 32) - tickCount;
      if (waitTicks > 0)
        goto Return;

      this.Lock.Enter();
      tt = this.TopTimed;
      if (null == tt)
        goto ExitAndReturnWithInfinite;

      waitTicks = (int)(tt.Ticks >> 32) - tickCount;
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
      var tag = this.unique;
      x.Ticks |= tag; // XXX: Kludge to make sure Ticks are unique.
      this.unique = tag + 1;
      var h = this.TopTimed;
      if (null == h) {
        this.TopTimed = x;
        x.Up = x;
        x.Next = x;
        Scheduler.Signal(this.Scheduler);
      } else if (x.Ticks - h.Ticks < 0) {
        x.Next = h.Up;
        h.Up = x;
        x.Up = x;
        this.TopTimed = x;
        Scheduler.Signal(this.Scheduler);
      } else if (x.Ticks - h.Up.Ticks > 0) {
        x.Up = h.Up;
        h.Up = x;
        x.Next = x;
      } else {
        var y = h.Up;
        Work z = y;
        while (x.Ticks - y.Up.Ticks < 0) {
          y = y.Up;
          var t = y.Next;
          y.Next = z;
          z = t;
        }
        x.Up = y.Up;
        x.Next = z;
        y.Up = x;
        h.Up = x;
      }
      this.Lock.Exit();
    }

    [MethodImpl(AggressiveInlining.Flag)]
    private void DropTimed() {
      // XXX This implementation requires keys to be unique - must analyze and fix this properly to allow duplicates.
      var h = this.TopTimed;
      if (null == h)
        return;
      var y1 = h.Up;
      var y2 = h.Next as WorkTimed;
      if (y1.Ticks - y2.Ticks < 0) {
        var t = y1;
        y1 = y2;
        y2 = t;
      }
      if (y1.Ticks == h.Ticks) {
        this.TopTimed = null;
        return;
      }
      WorkTimed x;
      var h3 = y1;
      y1 = y1.Up;
      h3.Up = h3;
      while (true) {
        if (y1.Ticks - y2.Ticks < 0) {
          var t = y1;
          y1 = y2;
          y2 = t;
        }
        if (y1.Ticks == h.Ticks) {
          this.TopTimed = h3;
          return;
        }
        x = y1;
        y1 = y1.Up;
        x.Up = x.Next as WorkTimed;
        x.Next = h3.Up;
        h3.Up = x;
        h3 = x;
      }
    }
  }

  internal abstract class WorkTimed : Work {
    internal WorkTimed Up;
    internal long Ticks;
    internal Pick Pick;
    internal int Me;

    [MethodImpl(AggressiveInlining.Flag)]
    internal WorkTimed(int ticks) {
      this.Ticks = ticks;
    }

    [MethodImpl(AggressiveInlining.Flag)]
    internal WorkTimed(int ticks, int me, Pick pk) {
      this.Ticks = ((long)ticks) << 32;
      this.Me = me;
      this.Pick = pk;
    }
  }
}
