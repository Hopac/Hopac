// Copyright (C) by Vesa Karvonen

namespace Hopac {
  using Microsoft.FSharp.Core;
  using System;
  using System.Runtime.CompilerServices;
  using System.Threading;
  using Hopac.Core;

  ///
  public class Once<X> : Alt<X> {
    private X value;
    private volatile int state;

    ///
    public Once(X value) { this.value = value; }

    internal override void DoJob(ref Worker wr, Cont<X> xK) {
    Spin:
      var st = this.state;
      if (st > 0) goto Done;
      if (st < 0) goto Spin;
      if (0 != Interlocked.CompareExchange(ref this.state, 1, st)) goto Spin;

      var x = this.value;
      this.value = default(X);
      Cont.Do(xK, ref wr, x);
    Done:
      return;
    }

    internal override void TryAlt(ref Worker wr, int i, Cont<X> xK, Else xE) {
    Spin:
      var st = this.state;
      if (st > 0) goto Done;
      if (st < 0) goto Spin;
      if (0 != Interlocked.CompareExchange(ref this.state, -1, st)) goto Spin;

      var pk = xE.pk;
    TryPick:
      var stPk = Pick.TryPick(pk);
      if (stPk > 0) goto AlreadyPicked;
      if (stPk < 0) goto TryPick;

      this.state = 1;
      Pick.SetNacks(ref wr, i, pk);

      var x = this.value;
      this.value = default(X);
      Cont.Do(xK, ref wr, x);
      return;

    AlreadyPicked:
      st = 0;
    Done:
      xE.TryElse(ref wr, i);
    }
  }
}
