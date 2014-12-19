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
    internal int NumPulseWaiters;
    internal WorkerEvent[] Events;
    internal FSharpFunc<Exception, Job<Unit>> TopLevelHandler;
    internal Job<int> IdleHandler;

    internal Scheduler() { }

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
    internal static void Inc(Scheduler sr) {
      Enter(sr);
      sr.NumActive += 1;
      Exit(sr);
    }

    [MethodImpl(AggressiveInlining.Flag)]
    internal static void UnsafeDec(Scheduler sr) {
      var numActive = sr.NumActive - 1;
      sr.NumActive = numActive;
      if (0 == numActive && 0 != sr.NumPulseWaiters) {
        Monitor.Enter(sr);
        Monitor.PulseAll(sr);
        Monitor.Exit(sr);
      }
    }

    [MethodImpl(AggressiveInlining.Flag)]
    internal static void Dec(Scheduler sr) {
      Enter(sr);
      UnsafeDec(sr);
      Exit(sr);
    }

    [MethodImpl(AggressiveInlining.Flag)]
    internal static void UnsafeWait(Scheduler sr, int ms, WorkerEvent ev) {
      ev.Next = sr.WaiterStack;
      sr.WaiterStack = ev.Me;
      UnsafeDec(sr);
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

    [MethodImpl(AggressiveInlining.Flag)]
    internal static void PushAndDec(Scheduler sr, Work work, Work last, int n) {
      Enter(sr);
      last.Next = sr.WorkStack;
      sr.WorkStack = work;
      sr.NumWorkStack += n;
      sr.NumActive -= 1;
      UnsafeSignal(sr);
      Exit(sr);
    }

    [MethodImpl(AggressiveInlining.Flag)]
    internal static void PushAllAndDec(Scheduler sr, Work work) {
      if (null == work) {
        Dec(sr);
      } else {
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
        PushAndDec(sr, work, last, n);
      }
    }
  }

  internal class KillException : Exception { }

  internal class AbortWork : Work {
    internal override Proc GetProc(ref Worker wr) {
      throw new KillException();
    }

    internal override void DoHandle(ref Worker wr, Exception e) {
      throw new KillException();
    }

    internal override void DoWork(ref Worker wr) {
      throw new KillException();
    }
  }
}
