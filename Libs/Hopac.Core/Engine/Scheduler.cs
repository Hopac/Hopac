// Copyright (C) by Housemarque, Inc.

namespace Hopac {
  using Hopac.Core;
  using Microsoft.FSharp.Core;
  using System.Diagnostics;
  using System.Runtime.CompilerServices;
  using System.Threading;
  using System;

  /// <summary>Represents a scheduler that manages a number of worker
  /// threads.</summary>
  public class Scheduler {
    internal Work WorkStack;
    internal SpinlockTTAS Lock;
    internal int NumWorkStack;
    internal int WaiterStack;
    internal int NumActive;
    internal WorkerEvent[] Events;
    internal FSharpFunc<Exception, Job<Unit>> TopLevelHandler;
    internal Job<int> IdleHandler;

    internal Scheduler(int numWorkers,
                       FSharpFunc<Exception, Job<Unit>> topLevelHandler,
                       Job<int> idleHandler) {
      StaticData.Init();
      TopLevelHandler = topLevelHandler;
      IdleHandler = idleHandler;
      WaiterStack = -1;
      NumActive = numWorkers;
      Events = new WorkerEvent[numWorkers];
      var threads = new Thread[numWorkers];
      for (int i = 0; i < numWorkers; ++i) {
        Events[i] = new WorkerEvent(i);
        var index = i;
        var thread = new Thread(() => Worker.Run(this, index));
        threads[i] = thread;
        thread.IsBackground = true;
      }
      for (int i=0; i < numWorkers; ++i)
        threads[i].Start();
    }

    /// <summary>Returns true if the scheduler is completely idle, meaning that
    /// all worker threads are waiting, at the moment.  Note that calling this
    /// from a worker thread, even from an idle handler, always returns
    /// false.</summary>
    [MethodImpl(AggressiveInlining.Flag)]
    public static bool IsIdle(Scheduler sr) {
      return 0 == sr.NumActive;
    }

    /// <summary>Kills the worker threads of the scheduler one-by-one.  This
    /// should only be used with a local scheduler that is known to be
    /// idle.</summary>
    public static void Kill(Scheduler sr) {
      var work = new AbortWork();
      PushAll(sr, work);
    }

    [MethodImpl(AggressiveInlining.Flag)]
    internal static void Enter(Scheduler sr) {
      sr.Lock.Enter();
    }

    [MethodImpl(AggressiveInlining.Flag)]
    internal static void Exit(Scheduler sr) {
      sr.Lock.Exit();
    }

    [MethodImpl(AggressiveInlining.Flag)]
    internal static void UnsafeSignal(Scheduler sr) {
      var waiter = sr.WaiterStack;
      if (0 <= waiter) {
        var ev = sr.Events[waiter];
        sr.WaiterStack = ev.Next;
        sr.NumActive += 1;
        ev.Set();
      }
    }

    [MethodImpl(AggressiveInlining.Flag)]
    internal static void Signal(Scheduler sr) {
      if (0 <= sr.WaiterStack) {
        Enter(sr);
        UnsafeSignal(sr);
        Exit(sr);
      }
    }

    [MethodImpl(AggressiveInlining.Flag)]
    internal static void UnsafeWait(Scheduler sr, int ms, WorkerEvent ev) {
      ev.Next = sr.WaiterStack;
      sr.WaiterStack = ev.Me;
      sr.NumActive -= 1;
      Exit(sr);
      ev.Wait(ms);
      ev.Reset();
    }

    [MethodImpl(AggressiveInlining.Flag)]
    internal static void Push(Scheduler sr, Work work, Work last, int n) {
      Enter(sr);
      last.Next = sr.WorkStack;
      sr.WorkStack = work;
      sr.NumWorkStack += n;
      UnsafeSignal(sr);
      Exit(sr);
    }

    [MethodImpl(AggressiveInlining.Flag)]
    internal static void PushAll(Scheduler sr, Work work) {
      if (null == work)
        return;

      var n = 1;
      var last = work;
    FindLast:
      var next = last.Next;
      if (null == next)
        goto FoundLast;
      n += 1;
      last = next;
      goto FindLast;

    FoundLast:
      Push(sr, work, last, n);
    }
  }

  internal class KillException : Exception { }

  internal class AbortWork : Work {
    internal override void DoHandle(ref Worker wr, Exception e) {
      throw new KillException();
    }

    internal override void DoWork(ref Worker wr) {
      throw new KillException();
    }
  }
}
