// Copyright (C) by Vesa Karvonen

namespace Hopac.Core {
  using System;
  using System.Runtime.CompilerServices;
  internal static class Primitive {
    [MethodImpl(AggressiveInlining.Flag)]
    internal static bool IsNull<T>(T x) {
      return null == x;
    }
  }
}
