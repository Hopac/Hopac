// Copyright (C) by Housemarque, Inc.

namespace Hopac {
  using Hopac.Core;

  /// <summary>A first class synchronous operation.</summary>
  public abstract class Alt<T> : Job<T> {
    /// Internal implementation detail.
    internal abstract void TryAlt(ref Worker wr, int i, Pick pk, Cont<T> xK, Else<T> xE);
  }
}
