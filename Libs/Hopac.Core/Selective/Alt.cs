// Copyright (C) by Housemarque, Inc.

namespace Hopac {
  using Hopac.Core;

  /// <summary>Represents a first class synchronous operation.</summary>
  public abstract class Alt<T> : Job<T> {
    internal abstract void TryAlt(ref Worker wr, int i, Cont<T> xK, Else xE);
  }
}
