// Copyright (C) by Housemarque, Inc.

namespace Hopac.Core {
  using System.Diagnostics;
  using System.Runtime.CompilerServices;
  using System.Threading;
  using System;

  internal struct WorkQueueLock {
    internal Work Tail;

    internal void Enter(ref Worker wr, Work work) {
      work.Next = null;

      var owner = Interlocked.Exchange(ref Tail, work);

      if (owner != null) {
        owner.Next = work;
      } else {
      Release:
        work.DoWork(ref wr);
        var prev = work;
        work = Volatile.Read(ref prev.Next);
        if (null != work)
          goto Release;

        if (prev == Interlocked.CompareExchange(ref Tail, null, prev))
          return;

      Spin:
        work = Volatile.Read(ref prev.Next);
        if (null != work)
          goto Release;
        goto Spin;
      }
    }

    internal void ExitAndEnter(ref Worker wr, Work owner) {
      var work = owner.Next;
      if (null != work) {
        owner.Next = null;
        var last = Interlocked.Exchange(ref Tail, owner);

      Release:
        work.DoWork(ref wr);
        var prev = work;
      Spin:
        work = Volatile.Read(ref prev.Next);
        if (null != work)
          goto Release;
        if (prev != last)
          goto Spin;
      }
    }
  }
}
