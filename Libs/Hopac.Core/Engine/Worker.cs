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
    internal Scheduler Scheduler;

#if TRAMPOLINE
    [MethodImpl(AggressiveInlining.Flag)]
    internal void Init(Scheduler sr, void *StackLimit, int bytes) {
      this.StackLimit = (byte *)StackLimit - bytes;
#else
    [MethodImpl(AggressiveInlining.Flag)]
    internal void Init(Scheduler sr) {
#endif
#if ENABLE_MCS
      Node.Init();
#endif
      this.Scheduler = sr;
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
      var sr = wr.Scheduler;
      if (0 <= sr.Waiters) // null == Scheduler.WorkStack)
        goto MaybePushToScheduler;
    Contention:
      last.Next = older;
    Exit:
      return;

    MaybePushToScheduler:
      if (SpinlockTTAS.Open != SpinlockTTAS.GetState(ref sr.Lock))
        goto Contention;
      if (SpinlockTTAS.Open != SpinlockTTAS.TryEnter(ref sr.Lock))
        goto Contention;
      if (null == sr.WorkStack)
        goto PushToScheduler;
      SpinlockTTAS.Exit(ref sr.Lock);
      last.Next = older;
      return;

    PushToScheduler:
      sr.WorkStack = older;
      int waiters = sr.Waiters;
      if (0 <= waiters)
        sr.Events[waiters].Set();
      SpinlockTTAS.Exit(ref sr.Lock);
    }

    [MethodImpl(AggressiveInlining.Flag)]
    internal static void RunOnThisThread(Scheduler sr, Work work) {
      var wr = new Worker();
#if TRAMPOLINE
      wr.Init(sr, &wr.StackLimit, 1000);
#else
      wr.Init(sr);
#endif
      try {
        wr.Handler = work;
        work.DoWork(ref wr);
      } catch (Exception e) {
        wr.WorkStack = new FailWork(wr.WorkStack, e, wr.Handler);
      }

      Scheduler.PushAll(sr, wr.WorkStack);
    }

    [MethodImpl(AggressiveInlining.Flag)]
    internal static void RunOnThisThread<T>(Scheduler sr, Job<T> tJ, Cont<T> tK) {
      var wr = new Worker();
#if TRAMPOLINE
      wr.Init(sr, &wr.StackLimit, 1000);
#else
      wr.Init(sr);
#endif
      try {
        wr.Handler = tK;
        tJ.DoJob(ref wr, tK);
      } catch (Exception e) {
        wr.WorkStack = new FailWork(wr.WorkStack, e, wr.Handler);
      }

      Scheduler.PushAll(sr, wr.WorkStack);
    }
    
    internal static void Run(Scheduler sr, int me) {
      var wr = new Worker();
#if TRAMPOLINE
      wr.Init(sr, &wr.StackLimit, 4000);
#else
      wr.Init(sr);
#endif
      var iK = new IdleCont();

      var mine = sr.Events[me];

      while (null != sr) {
        try {
        Restart:
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
          work = sr.WorkStack;
          if (null == work)
            goto TryIdle;
          sr.Lock.Enter();
        EnterSchedulerLocked:
          work = sr.WorkStack;
          if (null == work)
            goto ExitAndTryIdle;

        SchedulerGotSome:
          var next = work.Next;
          sr.WorkStack = next;
          if (null != next) {
            var waiter = sr.Waiters;
            if (0 <= waiter)
              sr.Events[waiter].Set();
          }
          sr.Lock.Exit();
          goto Do;

        ExitAndTryIdle:
          sr.Lock.Exit();

        TryIdle:
          iK.Value = Timeout.Infinite;

          var iJ = sr.IdleHandler;
          if (null != iJ) {
            wr.Handler = iK;
            iJ.DoJob(ref wr, iK);
          }

          if (0 == iK.Value)
            goto Restart;

          sr.Lock.Enter();
          work = sr.WorkStack;
          if (null != work)
            goto SchedulerGotSome;

          var n = sr.Waiters;
          if (n < 0) {
            mine.Next = -1;
            sr.Waiters = me;
          } else {
            WorkerEvent other = sr.Events[n];
            mine.Next = other.Next;
            other.Next = me;
          }

          mine.Reset();
          sr.Lock.Exit();
          mine.Wait(iK.Value);
          sr.Lock.Enter();
          if (sr.Waiters == me) {
            sr.Waiters = mine.Next;
          } else {
            WorkerEvent ev = sr.Events[sr.Waiters];
            if (ev.Next != me) {
              do {
                ev = sr.Events[ev.Next];
              } while (ev.Next != me);
            }
            ev.Next = mine.Next;
          }

          goto EnterSchedulerLocked;
        } catch (ThreadAbortException) {
          Scheduler.Signal(sr);
          sr = null;
        } catch (Exception e) {
          wr.WorkStack = new FailWork(wr.WorkStack, e, wr.Handler);
        }
      }
    }

    internal class IdleCont : Cont<int> {
      internal override void DoHandle(ref Worker wr, Exception e) {
        Handler.DoHandle(null, ref wr, e);
      }

      internal override void DoWork(ref Worker wr) { }

      internal override void DoCont(ref Worker wr, int value) {
        this.Value = value;
      }
    }
  }
}
