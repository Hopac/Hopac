// Copyright (C) by Housemarque, Inc.

namespace Hopac.Core {
  using System.Diagnostics;
  using System.Runtime.CompilerServices;
  using System.Threading;

  /// <summary>Provides a low overhead spinlock that is about as fast as
  /// possible in case of low contention, but also becomes slow in case of
  /// contention.  On every change of owner this spinlock implementation
  /// requires Omega(n) cache line transfers, where n is the number of threads
  /// waiting for the lock, and is thus inherently unscalable.</summary>
  internal struct SpinlockTTAS {
    private volatile int state;

    internal const int Open   =  0;
    internal const int Locked = -1;

    [MethodImpl(AggressiveInlining.Flag)]
    internal void Enter() {
    Spin:
      // First spin using shared reads.  This improves performance a bit.
      var st = state;
      if (st < Open)
        goto Spin;
      if (Interlocked.Exchange(ref state, ~st) < Open)
        goto Spin;
    }

    [MethodImpl(AggressiveInlining.Flag)]
    internal void Exit() {
      Debug.Assert(state == Locked); // Must be locked!
      state = Open;
    }
  }
}