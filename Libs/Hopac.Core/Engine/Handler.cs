// Copyright (C) by Housemarque, Inc.

namespace Hopac.Core {
  using System.Runtime.CompilerServices;
  using System;

  /// <summary>Exception handling continuation.</summary>
  internal abstract class Handler {
    internal abstract void DoHandle(ref Worker wr, Exception e);
  }

  internal abstract class Handler_State<T> : Handler {
    internal T State;

    [MethodImpl(AggressiveInlining.Flag)]
    internal Handler_State() { }

    [MethodImpl(AggressiveInlining.Flag)]
    internal Handler_State(T state) { this.State = state; }
  }
}
