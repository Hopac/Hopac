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

      var prev = Interlocked.Exchange(ref Tail, work);

      if (prev != null) {
        prev.Next = work;
      } else {
      Release:
        work.DoWork(ref wr);
        var last = work;
        work = work.Next;
        if (null != work)
          goto Release;

        if (last == Interlocked.CompareExchange(ref Tail, null, last))
          return;

      Spin:
        work = last.Next;
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
        if (work == last)
          return;
        var prev = work;
      Spin:
        work = prev.Next;
        if (null != work)
          goto Release;
        goto Spin;
      }
    }
  }
}
