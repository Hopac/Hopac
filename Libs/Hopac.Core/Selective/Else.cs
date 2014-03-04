// Copyright (C) by Housemarque, Inc.

namespace Hopac.Core {
  using System;
  using System.Runtime.CompilerServices;

  internal abstract class Else<T> {
    internal abstract void TryElse(ref Worker wr, int i, Pick pk, Cont<T> tK);
  }

  internal abstract class Else_State<T, S> : Else<T> {
    internal S State;

    [MethodImpl(AggressiveInlining.Flag)]
    internal Else_State() { }

    [MethodImpl(AggressiveInlining.Flag)]
    internal Else_State(S s) { State = s; }
  }
}
