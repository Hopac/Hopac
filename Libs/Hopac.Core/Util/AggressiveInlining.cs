// Copyright (C) by Housemarque, Inc.

namespace Hopac.Core {
  using System.Runtime.CompilerServices;

  internal class AggressiveInlining {
#if AGGRESSIVE_INLINING
    internal const MethodImplOptions Flag = MethodImplOptions.AggressiveInlining;
#else
    internal const MethodImplOptions Flag = (MethodImplOptions)0;
#endif
  }
}
