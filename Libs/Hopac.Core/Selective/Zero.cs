// Copyright (C) by Housemarque, Inc.

namespace Hopac.Core {
  using Microsoft.FSharp.Core;

  internal sealed class Zero : Alt<Unit> {
    internal override void DoJob(ref Worker wr, Cont<Unit> aK) { }

    internal override void TryAlt(ref Worker wr, int i, Pick pk, Cont<Unit> uK, Else<Unit> uE) {
      uE.TryElse(ref wr, i, pk, uK);
    }
  }
}
