// Copyright (C) by Housemarque, Inc.

namespace Hopac.Core {
  using Microsoft.FSharp.Core;
  using System.Diagnostics;
  using System.Runtime.CompilerServices;
  using System.Threading;
  using System;

  internal sealed class Nack : Promise<Unit> {
    internal Nack Next;
    internal int I0;
    internal int I1;

    [MethodImpl(AggressiveInlining.Flag)]
    internal Nack(Nack next, int i0) {
      this.Next = next;
      this.I0 = i0;
      this.I1 = Int32.MaxValue;
    }

    [MethodImpl(AggressiveInlining.Flag)]
    internal static void Signal(ref Worker wr, Nack nk) {
    Spin:
      var state = nk.State;
      if (state < Delayed) goto Spin;
      if (Running != Interlocked.CompareExchange(ref nk.State, state+1, state)) goto Spin;

      WaitQueue.PickReaders(ref nk.Readers, null, ref wr);
    }
  }
}
