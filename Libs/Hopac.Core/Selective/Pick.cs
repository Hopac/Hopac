// Copyright (C) by Housemarque, Inc.

namespace Hopac.Core {
  using System;
  using System.Runtime.CompilerServices;
  using System.Threading;

  /// <summary>Internal implementation detail.</summary>
  internal class Pick {
    /// <summary>Internal implementation detail.</summary>
    internal Nack Nacks;
    internal volatile int State;  // 0 = available, -1 = claimed, 1 = picked

    [MethodImpl(AggressiveInlining.Flag)]
    internal static int TryClaim(Pick pk) {
      return Interlocked.CompareExchange(ref pk.State, -1, 0);
    }

    [MethodImpl(AggressiveInlining.Flag)]
    internal static void Unclaim(Pick pk) {
      pk.State = 0;
    }

    [MethodImpl(AggressiveInlining.Flag)]
    internal static void PickClaimed(Pick pk) {
      pk.State = 1;
    }

    [MethodImpl(AggressiveInlining.Flag)]
    internal static int TryPick(Pick pk) {
      return Interlocked.CompareExchange(ref pk.State, 1, 0);
    }

    /// <summary>Internal implementation detail.</summary>
    [MethodImpl(AggressiveInlining.Flag)]
    public static Nack AddNack(Pick pk, int i0) {
    TryClaim:
      var st = TryClaim(pk);
      if (st > 0) goto AlreadyPicked;
      if (st < 0) goto TryClaim;

      var nk = new Nack(pk.Nacks, i0);
      pk.Nacks = nk;

      Unclaim(pk);
      return nk;

    AlreadyPicked:
      return null;
    }

    [MethodImpl(AggressiveInlining.Flag)]
    internal static void SetNacks(ref Worker wr, int i, Pick pk) {
      Nack nk = pk.Nacks;
      if (null != nk) {
        SetNacks(ref wr, i, nk);
      }
    }

    private static void SetNacks(ref Worker wr, int i, Nack nk) {
      do {
        if (i < nk.I0 || nk.I1 <= i)
          Nack.Signal(ref wr, nk);
        nk = nk.Next;
      } while (null != nk);
    }
  }
}
