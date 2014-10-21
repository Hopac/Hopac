// Copyright (C) by Housemarque, Inc.

namespace Hopac {
  using System;
  using Hopac.Core;

  /// <summary>Represents a first class synchronous operation.</summary>
  public abstract class Alt<T> : Job<T> {
    internal abstract void TryAlt(ref Worker wr, int i, Cont<T> xK, Else xE);
  }

  namespace Core {
    ///
    public abstract class AltBind<X, Y> : Alt<Y> {
      internal Alt<X> xA;

      ///
      public AltBind(Alt<X> xA) { this.xA = xA; }

      ///
      public abstract Job<Y> Do(X x);

      internal override void DoJob(ref Worker wr, Cont<Y> yK) {
        xA.DoJob(ref wr, new ContBind(this, yK));
      }

      internal sealed class ContBind : Cont<X> {
        internal AltBind<X, Y> yA;
        internal Cont<Y> yK;

        internal ContBind(AltBind<X, Y> yA, Cont<Y> yK) {
          this.yA = yA;
          this.yK = yK;
        }

        internal override Proc GetProc(ref Worker wr) {
          return Handler.GetProc(ref wr, ref yK);
        }

        internal override void DoHandle(ref Worker wr, Exception e) {
          Handler.DoHandle(yK, ref wr, e);
        }

        internal override void DoWork(ref Worker wr) {
          yA.Do(this.Value).DoJob(ref wr, yK);
        }

        internal override void DoCont(ref Worker wr, X x) {
          yA.Do(x).DoJob(ref wr, yK);
        }
      }

      internal override void TryAlt(ref Worker wr, int i, Cont<Y> yK, Else yE) {
        xA.TryAlt(ref wr, i, new ContBind(this, yK), yE);
      }
    }

    ///
    public abstract class AltMap<X, Y> : Alt<Y> {
      internal Alt<X> xA;

      ///
      public AltMap(Alt<X> xA) { this.xA = xA; }

      ///
      public abstract Y Do(X x);

      internal override void DoJob(ref Worker wr, Cont<Y> yK) {
        xA.DoJob(ref wr, new ContMap(this, yK));
      }

      internal sealed class ContMap : Cont<X> {
        internal AltMap<X, Y> yA;
        internal Cont<Y> yK;

        internal ContMap(AltMap<X, Y> yA, Cont<Y> yK) {
          this.yA = yA;
          this.yK = yK;
        }

        internal override Proc GetProc(ref Worker wr) {
          return Handler.GetProc(ref wr, ref yK);
        }

        internal override void DoHandle(ref Worker wr, Exception e) {
          Handler.DoHandle(yK, ref wr, e);
        }

        internal override void DoWork(ref Worker wr) {
          yK.DoCont(ref wr, yA.Do(this.Value));
        }

        internal override void DoCont(ref Worker wr, X x) {
          yK.DoCont(ref wr, yA.Do(x));
        }
      }

      internal override void TryAlt(ref Worker wr, int i, Cont<Y> yK, Else yE) {
        xA.TryAlt(ref wr, i, new ContMap(this, yK), yE);
      }
    }

    ///
    public abstract class AltDelay<X> : Alt<X> {

      ///
      public abstract Alt<X> Do();

      internal override void DoJob(ref Worker wr, Cont<X> xK) {
        Do().DoJob(ref wr, xK);
      }

      internal override void TryAlt(ref Worker wr, int i, Cont<X> xK, Else xE) {
        var pk = xE.pk;
        if (0 == Pick.Claim(pk)) {
          Alt<X> xA;
          try {
            xA = Do();
            Pick.Unclaim(pk);
          } catch (Exception e) {
            Pick.PickClaimedAndSetNacks(ref wr, i, pk);
            xA = new AltFail<X>(e);
          }
          xA.TryAlt(ref wr, i, xK, xE);
        }
      }
    }

    internal sealed class AltFail<X> : Alt<X> {
      internal Exception e;
      internal AltFail(Exception e) { this.e = e; }
      internal override void DoJob(ref Worker wr, Cont<X> xK) {
        Handler.DoHandle(xK, ref wr, e);
      }
      internal override void TryAlt(ref Worker wr, int i, Cont<X> xK, Else xE) {
        Handler.DoHandle(xK, ref wr, e);
      }
    }
  }
}
