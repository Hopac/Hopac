// Copyright (C) by Housemarque, Inc.

namespace Hopac {
  using Microsoft.FSharp.Core;
  using Hopac.Core;
  using System.Runtime.CompilerServices;
  using System;

  /// <summary>Represents a lightweight thread of execution.</summary>
  public abstract class Job<T> {
    internal abstract void DoJob(ref Worker wr, Cont<T> aK);
  }
}
