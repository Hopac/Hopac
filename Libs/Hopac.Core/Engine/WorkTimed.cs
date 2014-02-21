// Copyright (C) by Housemarque, Inc.

namespace Hopac.Core {
  using Microsoft.FSharp.Core;
  using System.Diagnostics;
  using System.Runtime.CompilerServices;
  using System.Threading;
  using System;

  internal class WorkTimed : Work {
    internal WorkTimed Up;
    internal int Ticks;
    internal int Me;
    internal Pick Pick;

    [MethodImpl(AggressiveInlining.Flag)]
    internal WorkTimed(int ticks) {
      this.Ticks = ticks;
    }

    [MethodImpl(AggressiveInlining.Flag)]
    internal WorkTimed(int ticks, int me, Pick pk) {
      this.Ticks = ticks;
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
