// Copyright (C) by Housemarque, Inc.

namespace Hopac.Core {
  using System;
  using System.Diagnostics;
  using System.Runtime.CompilerServices;
  using System.Threading;

  internal class WorkerEvent : ManualResetEventSlim {
    internal int Next;
    internal int Me;

    internal WorkerEvent(int me) {
      this.Next = -1;
      this.Me = me;
    }
  }

  unsafe internal struct Worker {
    internal Work WorkStack;
    internal Handler Handler;
#if TRAMPOLINE
    internal void *StackLimit;
#endif
#if ENABLE_MCS
    internal SpinlockMCS.Node Node;
#endif

#if TRAMPOLINE
    [MethodImpl(AggressiveInlining.Flag)]
    internal void Init(void *StackLimit, int bytes) {
      this.StackLimit = (byte *)StackLimit - bytes;
#else
    [MethodImpl(AggressiveInlining.Flag)]
    internal void Init() {
#endif
#if ENABLE_MCS
      Node.Init();
#endif
    }

    [MethodImpl(AggressiveInlining.Flag)]
    internal static void Push(ref Worker wr, Work work) {
      work.Next = null;
      PushNew(ref wr, work, work);
    }

    [MethodImpl(AggressiveInlining.Flag)]
    internal static void Push(ref Worker wr, Work work, Work last) {
      last.Next = null;
      PushNew(ref wr, work, last);
    }

    [MethodImpl(AggressiveInlining.Flag)]
    internal static void PushNew(ref Worker wr, Work work) {
      PushNew(ref wr, work, work);
    }

    [MethodImpl(AggressiveInlining.Flag)]
    internal static void PushNew(ref Worker wr, Work work, Work last) {
      Debug.Assert(null == last.Next);
      var older = wr.WorkStack;
      wr.WorkStack = work;
      if (null != older)
        goto CheckScheduler;
      goto Exit;
    CheckScheduler:
      if (0 <= Scheduler.Waiters) // null == Scheduler.WorkStack)
        goto MaybePushToScheduler;
    Contention:
      last.Next = older;
    Exit:
      return;

    MaybePushToScheduler:
      if (SpinlockTTAS.Open != SpinlockTTAS.GetState(ref Scheduler.Lock))
        goto Contention;
      if (SpinlockTTAS.Open != SpinlockTTAS.TryEnter(ref Scheduler.Lock))
        goto Contention;
      if (null == Scheduler.WorkStack)
        goto PushToScheduler;
      SpinlockTTAS.Exit(ref Scheduler.Lock);
      last.Next = older;
      return;

    PushToScheduler:
      Scheduler.WorkStack = older;
      int waiters = Scheduler.Waiters;
      if (0 <= waiters)
        Scheduler.Events[waiters].Set();
      SpinlockTTAS.Exit(ref Scheduler.Lock);
    }

    [MethodImpl(AggressiveInlining.Flag)]
    internal static void RunOnThisThread(Work work) {
      var wr = new Worker();
#if TRAMPOLINE
      wr.Init(&wr.StackLimit, 1000);
#else
      wr.Init();
#endif
      try {
        wr.Handler = work;
        work.DoWork(ref wr);
      } catch (Exception e) {
        wr.WorkStack = new FailWork(wr.WorkStack, e, wr.Handler);
      }

      Scheduler.PushAll(wr.WorkStack);
    }

    [MethodImpl(AggressiveInlining.Flag)]
    internal static void RunOnThisThread<T>(Job<T> tJ, Cont<T> tK) {
      var wr = new Worker();
#if TRAMPOLINE
      wr.Init(&wr.StackLimit, 1000);
#else
      wr.Init();
#endif
      try {
        wr.Handler = tK;
        tJ.DoJob(ref wr, tK);
      } catch (Exception e) {
        wr.WorkStack = new FailWork(wr.WorkStack, e, wr.Handler);
      }

      Scheduler.PushAll(wr.WorkStack);
    }
    
    internal static void Run(int me) {
      var wr = new Worker();
#if TRAMPOLINE
      wr.Init(&wr.StackLimit, 4000);
#else
      wr.Init();
#endif

      var mine = Scheduler.Events[me];

      while (true) {
        try {
          Work work = wr.WorkStack;
          if (null == work)
            goto EnterScheduler;

        WorkerLoop:
          wr.WorkStack = work.Next;
        Do:
          wr.Handler = work;
          work.DoWork(ref wr);
          work = wr.WorkStack;
          if (null != work)
            goto WorkerLoop;

        EnterScheduler:
          SpinlockTTAS.Enter(ref Scheduler.Lock);
        TryScheduler:
          work = Scheduler.WorkStack;
          if (null == work)
            goto TryTimed;

          {
            var next = work.Next;
            Scheduler.WorkStack = next;
            if (null == next)
              goto NoWake;
            var waiter = Scheduler.Waiters;
            if (0 <= waiter)
              Scheduler.Events[waiter].Set();
          }
        NoWake:
          SpinlockTTAS.Exit(ref Scheduler.Lock);
          goto Do;

        TryTimed:
          var waitTicks = Timeout.Infinite;
          {
            var tt = Scheduler.TopTimed;
            if (null == tt)
              goto JustWait;

            waitTicks = (int)(tt.Ticks >> 32) - Environment.TickCount;
            if (waitTicks > 0)
              goto JustWait;

            Scheduler.DropTimed();
            SpinlockTTAS.Exit(ref Scheduler.Lock);

            var pk = tt.Pick;
            if (null == pk)
              goto GotIt;
          TryPick:
            var st = Pick.TryPick(pk);
            if (st > 0)
              goto EnterScheduler;
            if (st < 0)
              goto TryPick;

            Pick.SetNacks(ref wr, tt.Me, pk);

          GotIt:
            work = tt;
            goto Do;
          }
        JustWait:
          var n = Scheduler.Waiters;
          if (n < 0) {
            mine.Next = -1;
            Scheduler.Waiters = me;
          } else {
            WorkerEvent other = Scheduler.Events[n];
            mine.Next = other.Next;
            other.Next = me;
          }

          mine.Reset();
          SpinlockTTAS.Exit(ref Scheduler.Lock);
          mine.Wait(waitTicks);
          SpinlockTTAS.Enter(ref Scheduler.Lock);

          if (Scheduler.Waiters == me) {
            Scheduler.Waiters = mine.Next;
          } else {
            WorkerEvent ev = Scheduler.Events[Scheduler.Waiters];
            if (ev.Next != me) {
              do {
                ev = Scheduler.Events[ev.Next];
              } while (ev.Next != me);
            }
            ev.Next = mine.Next;
          }

          goto TryScheduler;
        } catch (Exception e) {
          wr.WorkStack = new FailWork(wr.WorkStack, e, wr.Handler);
        }
      }
    }
  }
}
