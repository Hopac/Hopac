// Copyright (C) by Housemarque, Inc.

namespace Hopac.Core {
  using System;
  using System.Runtime.CompilerServices;

  internal abstract class Else {
    internal Pick pk;
    [MethodImpl(AggressiveInlining.Flag)]
    internal Else() { }
    [MethodImpl(AggressiveInlining.Flag)]
    internal Else(Pick pk) { this.pk = pk; }
    [MethodImpl(AggressiveInlining.Flag)]
    internal Else Init(Pick pk) { this.pk = pk; return this; }
    internal abstract void TryElse(ref Worker wr, int i);
  }

  internal abstract class Else_State<S1> : Else {
    internal S1 State1;

    [MethodImpl(AggressiveInlining.Flag)]
    internal Else Init(Pick pk, S1 s1) { this.pk = pk; this.State1 = s1; return this; }
  }
}
