// Copyright (C) by Housemarque, Inc.

namespace Hopac {
  using Hopac.Core;
  using Microsoft.FSharp.Core;
  using System.Diagnostics;
  using System.Runtime.CompilerServices;
  using System.Threading;
  using System;

  /// <summary>Internal implementation detail.</summary>
  public class Scheduler {
    internal Work WorkStack;
    internal SpinlockTTAS Lock;
    internal volatile int Waiters;
    internal WorkerEvent[] Events;
    internal FSharpFunc<Exception, Job<Unit>> TopLevelHandler;
    internal Job<int> IdleHandler;

    [MethodImpl(AggressiveInlining.Flag)]
    internal static void Signal(Scheduler sr) {
      var waiter = sr.Waiters;
      if (0 <= waiter) {
        sr.Lock.Enter();
        waiter = sr.Waiters;
        if (0 <= waiter)
          sr.Events[waiter].Set();
        sr.Lock.Exit();
      }
    }

    internal Scheduler(int numWorkers) {
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
    internal static void Push(Scheduler sr, Work work, Work last) {
      sr.Lock.Enter();
      last.Next = sr.WorkStack;
      sr.WorkStack = work;
      var waiters = sr.Waiters;
      if (0 <= waiters)
        sr.Events[waiters].Set();
      sr.Lock.Exit();
    }

    [MethodImpl(AggressiveInlining.Flag)]
    internal static void PushAll(Scheduler sr, Work work) {
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
      Push(sr, work, last);
    }
  }
}
