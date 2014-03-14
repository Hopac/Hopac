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
    internal int Waiters;
    internal WorkerEvent[] Events;
    internal FSharpFunc<Exception, Job<Unit>> TopLevelHandler;
    internal Job<int> IdleHandler;

    internal Scheduler(int numWorkers,
                       FSharpFunc<Exception, Job<Unit>> topLevelHandler,
                       Job<int> idleHandler) {
      TopLevelHandler = topLevelHandler;
      IdleHandler = idleHandler;
      Waiters = -1;
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
      var waiter = sr.Waiters;
      if (0 <= waiter) {
        var ev = sr.Events[waiter];
        sr.Waiters = ev.Next;
        ev.Set();
      }
    }

    [MethodImpl(AggressiveInlining.Flag)]
    internal static void Signal(Scheduler sr) {
      if (0 <= sr.Waiters) {
        Enter(sr);
        UnsafeSignal(sr);
        Exit(sr);
      }
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
}
