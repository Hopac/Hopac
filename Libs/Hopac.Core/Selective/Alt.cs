// Copyright (C) by Housemarque, Inc.

namespace Hopac {
  using System;
  using System.Runtime.CompilerServices;
  using Microsoft.FSharp.Core;
  using Hopac.Core;

  /// <summary>Represents a first class synchronous operation.</summary>
  public abstract class Alt<T> : Job<T> {
    internal abstract void TryAlt(ref Worker wr, int i, Cont<T> xK, Else xE);
  }

  namespace Core {
    ///
    public abstract class AltAfter<X, Y> : Alt<Y> {
      internal Alt<X> xA;
      ///
      [MethodImpl(AggressiveInlining.Flag)]
      public Alt<Y> InternalInit(Alt<X> xA) { this.xA = xA; return this; }
      ///
      public abstract JobContCont<X, Y> Do();
      internal override void DoJob(ref Worker wr, Cont<Y> yK) {
        var xK = Do();
        xK.yK = yK;
        xA.DoJob(ref wr, xK);
      }
      internal override void TryAlt(ref Worker wr, int i, Cont<Y> yK, Else yE) {
        var xK = Do();
        xK.yK = yK;
        xA.TryAlt(ref wr, i, xK, yE);
      }
    }

    ///
    public abstract class AltPrepareFun<X> : Alt<X> {
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

    ///
    public abstract class AltPrepareJob<X, xA> : Alt<X> where xA : Alt<X> {
      ///
      public abstract Job<xA> Do();
      internal override void DoJob(ref Worker wr, Cont<X> xK) {
        Do().DoJob(ref wr, new PrepareJobCont<X, xA>(xK));
      }
      internal override void TryAlt(ref Worker wr, int i, Cont<X> xK, Else xE) {
        var pk = xE.pk;
        if (0 == Pick.Claim(pk)) {
          Job<xA> xAJ;
          try {
            xAJ = Do();
          } catch (Exception e) {
            Pick.PickClaimedAndSetNacks(ref wr, i, pk);
            xAJ = new AltFail<xA>(e);
          }
          xAJ.DoJob(ref wr, new PrepareAltCont<X, xA>(i, xK, xE));
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

    ///
    public abstract class AltRandom<X> : Alt<X> {
      ///
      public abstract Alt<X> Do(ulong random);
      internal override void DoJob(ref Worker wr, Cont<X> xK) {
        Do(Randomizer.Next(ref wr.RandomLo, ref wr.RandomHi)).DoJob(ref wr, xK);
      }
      internal override void TryAlt(ref Worker wr, int i, Cont<X> xK, Else xE) {
        var pk = xE.pk;
        if (0 == Pick.Claim(pk)) {
          Alt<X> xA;
          try {
            xA = Do(Randomizer.Next(ref wr.RandomLo, ref wr.RandomHi));
            Pick.Unclaim(pk);
          } catch (Exception e) {
            Pick.PickClaimedAndSetNacks(ref wr, i, pk);
            xA = new AltFail<X>(e);
          }
          xA.TryAlt(ref wr, i, xK, xE);
        }
      }
    }

    internal class PrepareJobCont<X, xA> : Cont<xA> where xA : Alt<X> {
      private Cont<X> xK;
      internal PrepareJobCont(Cont<X> xK) { this.xK = xK; }
      internal override Proc GetProc(ref Worker wr) {
        return Handler.GetProc(ref wr, ref xK);
      }
      internal override void DoHandle(ref Worker wr, Exception e) {
        Handler.DoHandle(xK, ref wr, e);
      }
      internal override void DoWork(ref Worker wr) {
        Value.DoJob(ref wr, xK);
      }
      internal override void DoCont(ref Worker wr, xA value) {
        value.DoJob(ref wr, xK);
      }
    }

    internal class PrepareAltCont<X, xA> : Cont<xA> where xA : Alt<X> {
      private int i;
      private Cont<X> xK;
      private Else xE;
      internal PrepareAltCont(int i, Cont<X> xK, Else xE) {
        this.i = i;
        this.xK = xK;
        this.xE = xE;
      }
      internal override Proc GetProc(ref Worker wr) {
        return Handler.GetProc (ref wr, ref xK);
      }
      internal override void DoHandle(ref Worker wr, Exception e) {
        Pick.PickClaimedAndSetNacks(ref wr, i, xE.pk);
        var xK = this.xK;
        wr.Handler = xK;
        Handler.DoHandle(xK, ref wr, e);
      }
      internal override void DoWork(ref Worker wr) {
        var xE = this.xE;
        Pick.Unclaim(xE.pk);
        var xK = this.xK;
        wr.Handler = xK;
        Value.TryAlt(ref wr, i, xK, xE);
      }
      internal override void DoCont(ref Worker wr, xA xA) {
        var xE = this.xE;
        Pick.Unclaim(xE.pk);
        var xK = this.xK;
        wr.Handler = xK;
        xA.TryAlt(ref wr, i, xK, xE);
      }
    }

    internal class WithNackElse : Else {
      private Nack nk;
      private Else xE;
      internal WithNackElse(Nack nk, Else xE) : base(xE.pk) {
        this.nk = nk;
        this.xE = xE;
      }
      internal override void TryElse(ref Worker wr, int i) {
        nk.I1 = i;
        xE.TryElse(ref wr, i);
      }
    }

    internal class WithNackCont<X, xA> : Cont<xA> where xA : Alt<X> {
      private Cont<X> xK;
      private Else xE;
      internal WithNackCont(Cont<X> xK, Else xE) {
        this.xK = xK;
        this.xE = xE;
      }
      internal override Proc GetProc(ref Worker wr) {
        return Handler.GetProc(ref wr, ref xK);
      }
      internal override void DoHandle(ref Worker wr, Exception e) {
        var pk = xE.pk;
        Pick.PickClaimedAndSetNacks(ref wr, pk.Nacks.I0, pk);
        var xK = this.xK;
        wr.Handler = xK;
        Handler.DoHandle(xK, ref wr, e);
      }
      internal override void DoWork(ref Worker wr) {
        var xE = this.xE;
        var pk = xE.pk;
        var nk = pk.Nacks;
        Pick.Unclaim(pk);
        var xK = this.xK;
        wr.Handler = xK;
        Value.TryAlt(ref wr, nk.I0, xK, new WithNackElse(nk, xE));
      }
      internal override void DoCont(ref Worker wr, xA value) {
        var xE = this.xE;
        var pk = xE.pk;
        var nk = pk.Nacks;
        Pick.Unclaim(pk);
        var xK = this.xK;
        wr.Handler = xK;
        value.TryAlt(ref wr, nk.I0, xK, new WithNackElse(nk, xE));
      }
    }

    ///
    public abstract class AltWithNackJob<X, xA> : Alt<X> where xA : Alt<X> {
      ///
      public abstract Job<xA> Do(Promise<Unit> nack);
      internal override void DoJob(ref Worker wr, Cont<X> xK) {
        Do(StaticData.zero).DoJob(ref wr, new PrepareJobCont<X, xA>(xK));
      }
      internal override void TryAlt(ref Worker wr, int i, Cont<X> xK, Else xE) {
        var nk = Pick.ClaimAndAddNack(xE.pk, i);
        if (null != nk) {
          var handler = new WithNackCont<X, xA>(xK, xE);
          wr.Handler = handler;
          Do(nk).DoJob(ref wr, handler);
        }
      }
    }

    ///
    public abstract class AltWithNackFun<X> : Alt<X> {
      ///
      public abstract Alt<X> Do(Promise<Unit> nack);
      internal override void DoJob(ref Worker wr, Cont<X> xK) {
        Do(StaticData.zero).DoJob(ref wr, xK);
      }
      internal override void TryAlt(ref Worker wr, int i, Cont<X> xK, Else xE) {
        var pk = xE.pk;
        var nk = Pick.ClaimAndAddNack(pk, i);
        if (null != nk) {
          Alt<X> xA;
          try {
            xA = Do(nk);
            Pick.Unclaim(pk);
          } catch (Exception e) {
            Pick.PickClaimedAndSetNacks(ref wr, i, pk);
            xA = new AltFail<X>(e);
          }
          xA.TryAlt(ref wr, i, xK, new WithNackElse(nk, xE));
        }
      }
    }
  }
}
