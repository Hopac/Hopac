// Copyright (C) by Housemarque, Inc.

namespace Hopac.Core {
  using Microsoft.FSharp.Core;
  using System.Diagnostics;
  using System.Runtime.CompilerServices;
  using System.Threading;
  using System;

  internal static class Scheduler {
    internal static volatile int Waiters;
    internal static volatile int Lock;
    internal static int TimedCount;
    internal static Work WorkStack;
    internal static WorkTimed TopTimed;
    internal static WorkerEvent[] Events;

    // We use manual initialization here, because automatic initialization
    // generates code with slightly more overhead.

    [MethodImpl(AggressiveInlining.Flag)]
    internal static object NullWhenNeedsInit() {
      return Volatile.Read(ref Events);
    }

    internal static void DoInit() {
      Debug.Assert(SpinlockTTAS.Locked == SpinlockTTAS.GetState(ref Lock) &&
                   null == NullWhenNeedsInit());
      var numWorkers = Environment.ProcessorCount;
      Waiters = -1;
      Events = new WorkerEvent[numWorkers];
      var threads = new Thread[numWorkers];
      for (int i = 0; i < numWorkers; ++i) {
        Events[i] = new WorkerEvent(i);
        var index = i;
        var thread = new Thread(() => Worker.Run(index));
        threads[i] = thread;
        thread.IsBackground = true;
      }
      for (int i=0; i < numWorkers; ++i)
        threads[i].Start();
    }

    [MethodImpl(AggressiveInlining.Flag)]
    internal static void Push(Work work, Work last) {
      SpinlockTTAS.Enter(ref Lock);
      last.Next = WorkStack;
      WorkStack = work;
      var waiters = Waiters;
      if (0 <= waiters)
        Events[waiters].Set();
      SpinlockTTAS.Exit(ref Lock);
    }

    [MethodImpl(AggressiveInlining.Flag)]
    internal static void PushAll(Work work) {
      if (null == work)
        return;

      var last = work;
    FindLast:
      var next = last.Next;
      if (null == next)
        goto FoundLast;
      last = next;
      goto FindLast;

    FoundLast:
      Push(work, last);
    }

    static uint unique = 0;

    [MethodImpl(AggressiveInlining.Flag)]
    internal static void SynchronizedPushTimed(ref Worker wr, WorkTimed x) {
      SpinlockTTAS.Enter(ref Scheduler.Lock);
      var tag = unique;
      x.Ticks |= tag; // XXX: Kludge to make sure Ticks are unique.
      unique = tag + 1;
      var h = TopTimed;
      if (null == h) {
        TopTimed = x;
        x.Up = x;
        x.Next = x;
        var waiter = Waiters;
        if (0 <= Waiters)
          Events[waiter].Set();
      } else if (x.Ticks - h.Ticks < 0) {
        x.Next = h.Up;
        h.Up = x;
        x.Up = x;
        TopTimed = x;
        var waiter = Waiters;
        if (0 <= Waiters)
          Events[waiter].Set();
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
      SpinlockTTAS.Exit(ref Scheduler.Lock);
    }

    [MethodImpl(AggressiveInlining.Flag)]
    internal static void DropTimed() {
      // XXX This implementation requires keys to be unique - must analyze and fix this properly to allow duplicates.
      var h = TopTimed;
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
        TopTimed = null;
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
          TopTimed = h3;
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
}
