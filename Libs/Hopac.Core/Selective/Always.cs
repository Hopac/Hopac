// Copyright (C) by Housemarque, Inc.

namespace Hopac.Core {
  using Microsoft.FSharp.Core;
  using System.Runtime.CompilerServices;

  /// Internal implementation detail.
  public sealed class Always<T> : Alt<T> {
    private readonly T value;

    /// Internal implementation detail.
    [MethodImpl(AggressiveInlining.Flag)]
    public Always(T value) {
      this.value = value;
    }

    internal override void DoJob(ref Worker wr, Cont<T> xK) {
      Cont.Do(xK, ref wr, this.value);
    }

    internal override void TryAlt(ref Worker wr, int i, Cont<T> xK, Else xE) {
      var pkSelf = xE.pk;
    TryPick:
      var stSelf = Pick.TryPick(pkSelf);
      if (stSelf > 0) goto AlreadyPicked;
      if (stSelf < 0) goto TryPick;

      Pick.SetNacks(ref wr, i, pkSelf);

      Cont.Do(xK, ref wr, this.value);

    AlreadyPicked:
      return;
    }
  }

  internal abstract class AlwaysUnitBase : Alt<Unit> {
    internal override void TryAlt(ref Worker wr, int i, Cont<Unit> uK, Else uE) {
      var pkSelf = uE.pk;
    TryPick:
      var stSelf = Pick.TryPick(pkSelf);
      if (stSelf > 0) goto AlreadyPicked;
      if (stSelf < 0) goto TryPick;

      Pick.SetNacks(ref wr, i, pkSelf);

      Work.Do(uK, ref wr);

    AlreadyPicked:
      return;
    }
  }

  internal sealed class AlwaysUnitTC : AlwaysUnitBase {
    internal override void DoJob(ref Worker wr, Cont<Unit> uK) {
      uK.DoWork(ref wr);
    }
  }

  internal sealed class AlwaysUnitNonTC : AlwaysUnitBase {
    internal override void DoJob(ref Worker wr, Cont<Unit> uK) {
      Work.Do(uK, ref wr);
    }
  }
}
