// Copyright (C) by Housemarque, Inc.

namespace Hopac.Core {
  using System;
  using System.Runtime.CompilerServices;

  internal abstract class Else {
    internal Pick pk;
    internal Else(Pick pk) { this.pk = pk; }
    internal abstract void TryElse(ref Worker wr, int i);
  }

  internal abstract class Else_State<S> : Else {
    internal S State;

    [MethodImpl(AggressiveInlining.Flag)]
    internal Else_State(Pick pk, S s) : base(pk) { State = s; }
  }
}
