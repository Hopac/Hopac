// Copyright (C) by Housemarque, Inc.

namespace Hopac.Core {
  using Microsoft.FSharp.Core;
  using System.Diagnostics;
  using System.Runtime.CompilerServices;
  using System.Threading;
  using System;

  internal class WorkTimed : Work {
    internal WorkTimed Up;
    internal readonly long Ticks;
    internal Pick Pick;
    internal int Me;

    private static int unique = 0; // XXX Kludge to eliminate duplicate keys.  Should fix the bottom up skew heap instead.

    [MethodImpl(AggressiveInlining.Flag)]
    internal WorkTimed(int ticks) {
      this.Ticks = ticks;
    }

    [MethodImpl(AggressiveInlining.Flag)]
    internal WorkTimed(int ticks, int me, Pick pk) {
      this.Ticks = (((long)ticks) << 32) | (uint)Interlocked.Increment(ref unique);
      this.Me = me;
      this.Pick = pk;
    }

    internal override void DoHandle(ref Worker wr, Exception e) {
      throw new NotImplementedException();
    }

    internal override void DoWork(ref Worker wr) {
      throw new NotImplementedException();
    }
  }
}
