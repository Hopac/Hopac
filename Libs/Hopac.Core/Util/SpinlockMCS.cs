// Copyright (C) by Housemarque, Inc.

#if ENABLE_MCS

namespace Hopac.Core {
  using System;
  using System.Diagnostics;
  using System.Runtime.CompilerServices;
  using System.Runtime.InteropServices;
  using System.Threading;

  /// This is a variation of the MCS spinlock.  This is slow in case of low
  /// contention, but should be fast in case of high contention.
  internal struct SpinlockMCS {
    private long state;

    [StructLayout(LayoutKind.Sequential)]
    internal struct Node {
      internal long Next;
      internal long Self;

      [MethodImpl(AggressiveInlining.Flag)]
      unsafe internal void Init() {
        fixed (long* self = &Next) {
          Self = (long)self;
        }
      }
    }

    [MethodImpl(AggressiveInlining.Flag)]
    unsafe internal void Enter(ref Worker wr) {
      var self = wr.Node.Self;
      var pred = Interlocked.Exchange(ref this.state, self);
      if (0 != pred) {
        *((long*)pred) = self;
        while (0 != Volatile.Read(ref wr.Node.Self)) { }
        wr.Node.Self = self;
      }
    }

    [MethodImpl(AggressiveInlining.Flag)]
    unsafe internal void Exit(ref Worker wr) {
      var next = wr.Node.Next;
      if (0 == next) {
        var self = wr.Node.Self;
        if (self == Interlocked.CompareExchange(ref this.state, 0, self))
          return;

        do { next = Volatile.Read(ref wr.Node.Next); } while (0 == next);
      }
      ((long*)next)[1] = 0;
      wr.Node.Next = 0;
    }
  }
}

#endif
