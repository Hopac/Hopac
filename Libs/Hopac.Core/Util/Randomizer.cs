// Copyright (C) by Vesa Karvonen

namespace Hopac.Core {
  using System;
  using System.Runtime.CompilerServices;

  internal class Randomizer {
    [MethodImpl(AggressiveInlining.Flag)]
    internal static ulong Next(ref ulong slo, ref ulong shi) {
      // xorshift128+
      var s1 = slo;
      var s0 = shi;
      var s1_23 = s1 << 23;
      var s0_26 = s0 >> 26;
      s1 ^= s1_23; // a
      slo = s0;
      var s1_17 = s1 >> 17;
      return (shi = ((s1 ^ s0) ^ (s1_17 ^ s0_26))) + s0; // b, c
    }

    [MethodImpl(AggressiveInlining.Flag)]
    internal static int NextInRange(ref ulong slo, ref ulong shi, int lo, int hi) {
      var s = (uint)Next(ref slo, ref shi);
      return (int)(((long)s * (long)(hi - lo)) >> 32) + lo;
    }
  }
}
