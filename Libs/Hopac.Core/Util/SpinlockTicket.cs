// Copyright (C) by Housemarque, Inc.

namespace Hopac.Core {
  using System.Diagnostics;
  using System.Runtime.CompilerServices;
  using System.Threading;

  internal struct SpinlockTicket {
    internal volatile int Next;
    internal volatile int Turn;

    [MethodImpl(AggressiveInlining.Flag)]
    internal void Enter() {
      var ticket = Interlocked.Increment(ref Next) - 1;
      while (Turn != ticket) ;
    }

    [MethodImpl(AggressiveInlining.Flag)]
    internal void Exit() {
      Turn += 1;
    }
  }
}
