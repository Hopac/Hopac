// Copyright (C) by Vesa Karvonen

namespace Hopac.Core {
  using Hopac.Core;
  using System.Runtime.CompilerServices;

  internal unsafe static class Unsafe {
    [MethodImpl(AggressiveInlining.Flag)]
    internal static byte *GetStackPtr() {
      byte dummy;
      return &dummy;
    }
  }
}
