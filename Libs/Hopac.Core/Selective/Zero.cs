// Copyright (C) by Housemarque, Inc.

namespace Hopac.Core {
  ///
  public sealed class Never<X> : Promise<X> {
    internal override void DoJob(ref Worker wr, Cont<X> aK) { }

    internal override void TryAlt(ref Worker wr, int i, Cont<X> xK, Else xE) {
      xE.TryElse(ref wr, i);
    }
  }
}
