// Copyright (C) by Vesa Karvonen

namespace Hopac.Core {
  using System;

  internal static class Randomizer {
    internal static uint Next(ref uint state) {
      var was = state;
      state = was * 1664525 + 1013904223; // Numerical Recipes
      return was;
    }

    internal static int NextInRange(ref uint state, int lo, int hi) {
      var r = Next(ref state);
      return (int)(((long)r * (long)(hi - lo)) >> 32) + lo;
    }
  }
}
