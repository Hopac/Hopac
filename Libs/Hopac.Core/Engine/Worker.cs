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
    internal byte *StackLimit;
    [ThreadStatic]
    internal static byte *ThreadStackLimit;
#endif
    internal Scheduler Scheduler;
    internal WorkerEvent Event;
    internal ulong RandomLo;
    internal ulong RandomHi;

    [ThreadStatic]
    internal static bool IsWorkerThread;

    [MethodImpl(AggressiveInlining.Flag)]
    internal void Init(Scheduler sr, int bytes) {
      var sp = Unsafe.GetStackPtr();
#if TRAMPOLINE
      var limit = ThreadStackLimit;
      if (null == limit)
        ThreadStackLimit = limit = sp - bytes;
      this.StackLimit = limit;
#endif
      this.RandomLo = (ulong)sp; // Quick and dirty (also never zero)
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
      if (null != older) {
        var sr = wr.Scheduler;
        if (null == sr.WorkStack) {
          Scheduler.PushAll(sr, older);
        } else {
          last.Next = older;
        }
      }
    }

    [MethodImpl(AggressiveInlining.Flag)]
    internal static void RunOnThisThread(Scheduler sr, Work work) {
      var wr = new Worker();
      wr.Init(sr, 8000);

      Scheduler.Inc(sr);

      try {
        wr.Handler = work;
        work.DoWork(ref wr);
      } catch (Exception e) {
        wr.WorkStack = new FailWork(wr.WorkStack, e, wr.Handler);
      }

      Scheduler.PushAllAndDec(sr, wr.WorkStack);
    }

    [MethodImpl(AggressiveInlining.Flag)]
    internal static void RunOnThisThread<T>(Scheduler sr, Job<T> tJ, Cont<T> tK) {
      var wr = new Worker();
      wr.Init(sr, 8000);

      Scheduler.Inc(sr);

      try {
        wr.Handler = tK;
        tJ.DoJob(ref wr, tK);
      } catch (Exception e) {
        wr.WorkStack = new FailWork(wr.WorkStack, e, wr.Handler);
      }

      Scheduler.PushAllAndDec(sr, wr.WorkStack);
    }
    
    internal static void Run(Scheduler sr, int me) {
      IsWorkerThread = true;

      var wr = new Worker();
      wr.Init(sr, 16000);
      wr.RandomHi = (ulong)DateTime.UtcNow.Ticks;

      var iK = new IdleCont();

      var wdm = (1L << 32) / sr.Events.Length;

      wr.Event = sr.Events[me];

      while (null != sr) {
        try {
        Restart:
          Work work = wr.WorkStack;
          if (null == work)
            goto EnterScheduler;

        WorkerLoop:
          wr.Handler = work;
          {
            var next = work.Next;
            if (null != next && null == sr.WorkStack) {
              Scheduler.PushAll(sr, next);
              next = null;
            }
            wr.WorkStack = next;
          }
          work.DoWork(ref wr);
          work = wr.WorkStack;
          if (null != work)
            goto WorkerLoop;

          wr.Handler = null;

        EnterScheduler:
          work = sr.WorkStack;
          if (null == work)
            goto TryIdle;

          Scheduler.Enter(sr);
        EnteredScheduler:
          work = sr.WorkStack;
          if (null == work)
            goto ExitAndTryIdle;

        SchedulerGotSome: {
            var last = work;
            int numWorkStack = sr.NumWorkStack;
            int n = (int)((numWorkStack - 1L) * wdm >> 32) + 1;
            sr.NumWorkStack = numWorkStack-n;
            n -= 1;
            while (n > 0) {
              last = last.Next;
              n -= 1;
            }
            var next = last.Next;
            last.Next = null;
            sr.WorkStack = next;
            if (null != next)
              Scheduler.UnsafeSignal(sr);
            Scheduler.Exit(sr);
            goto WorkerLoop;
          }

        ExitAndTryIdle:
          Scheduler.Exit(sr);

        TryIdle:
          iK.Value = Timeout.Infinite;

          var iJ = sr.IdleHandler;
          if (null != iJ) {
            wr.Handler = iK;
            iJ.DoJob(ref wr, iK);
          }

          if (0 == iK.Value)
            goto Restart;

          Scheduler.Enter(sr);
          work = sr.WorkStack;
          if (null != work)
            goto SchedulerGotSome;

          Scheduler.UnsafeWait(sr, iK.Value, wr.Event);
          goto EnteredScheduler;
        } catch (KillException) {
          Scheduler.Kill(sr);
          Scheduler.Dec(sr);
          sr = null;
        } catch (Exception e) {
          wr.WorkStack = new FailWork(wr.WorkStack, e, wr.Handler);
        }
      }
    }

    internal sealed class IdleCont : Cont<int> {
      internal override Proc GetProc(ref Worker wr) {
        throw new NotImplementedException();  // XXX: Idle job is not a process?
      }

      internal override void DoHandle(ref Worker wr, Exception e) {
        Handler.DoHandleNull(ref wr, e);
      }

      internal override void DoWork(ref Worker wr) { }

      internal override void DoCont(ref Worker wr, int value) {
        this.Value = value;
      }
    }
  }
}
