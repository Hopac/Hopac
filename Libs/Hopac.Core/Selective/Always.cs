// Copyright (C) by Housemarque, Inc.

namespace Hopac.Core {
  using System.Runtime.CompilerServices;

  /// Internal implementation detail.
  public class Always<T> : Alt<T> {
    private readonly T value;

    /// Internal implementation detail.
    [MethodImpl(AggressiveInlining.Flag)]
    public Always(T value) {
      this.value = value;
    }

    /// Internal implementation detail.
    internal override void DoJob(ref Worker wr, Cont<T> xK) {
      xK.DoCont(ref wr, this.value);
    }

    /// Internal implementation detail.
    internal override void TryAlt(ref Worker wr, int i, Pick pkSelf, Cont<T> aK) {
    TryPick:
      var stSelf = Pick.TryPick(pkSelf);
      if (stSelf > 0) goto AlreadyPicked;
      if (stSelf < 0) goto TryPick;

      Pick.SetNacks(ref wr, i, pkSelf);

      aK.DoCont(ref wr, this.value);

    AlreadyPicked:
      return;
    }
  }
}
