// Copyright (C) by Housemarque, Inc.

namespace Hopac.Core {
  using System;
  using System.Threading;
  using Microsoft.FSharp.Core;

  internal abstract class WorkAsyncCallback : Cont<Unit> {
    internal abstract void Ready(IAsyncResult iar);
  }

  internal class JobCallback<X> : WorkAsyncCallback {
    internal Scheduler sr;
    internal FromBeginEnd<X> xJ;
    internal Cont<X> xK;
    internal IAsyncResult iar;
    internal JobCallback(Scheduler sr, FromBeginEnd<X> xJ, Cont<X> xK) {
      this.sr = sr;
      this.xJ = xJ;
      this.xK = xK;
    }
    internal override void Ready(IAsyncResult iar) {
      this.iar = iar;
      Worker.RunOnThisThread(sr, this);
    }
    internal override Proc GetProc(ref Worker wr) {
      return xK.GetProc(ref wr);
    }
    internal override void DoHandle(ref Worker wr, Exception e) {
      xK.DoHandle(ref wr, e);
    }
    internal override void DoWork(ref Worker wr) {
      xK.DoCont(ref wr, xJ.DoEnd(iar));
    }
    internal override void DoCont(ref Worker wr, Unit value) {
      this.DoWork(ref wr);
    }
  }

  internal class AltCallback<X> : JobCallback<X> {
    internal Pick pk;
    internal volatile int me;
    internal AltCallback(Scheduler sr, FromBeginEnd<X> xJ, Cont<X> xK, Pick pk, int me) : base(sr, xJ, xK) {
      this.pk = pk;
      this.me = me;
    }
    internal override void DoWork(ref Worker wr) {
      var me = this.me;
      if (me < 0 && me == Interlocked.CompareExchange(ref this.me, 0, me))
        return;
      if (0 != Pick.PickAndSetNacks(pk, ref wr, me))
        xJ.DoCancel(iar);
      else
        xK.DoCont(ref wr, xJ.DoEnd(iar));
    }
  }

  ///
  public abstract class FromBeginEnd<X> : Alt<X> {
    ///
    public abstract IAsyncResult DoBegin(AsyncCallback acb, object state);
    ///
    public abstract X DoEnd(IAsyncResult iar);
    ///
    public virtual void DoCancel(IAsyncResult iar) {}
    internal override void DoJob(ref Worker wr, Cont<X> xK) {
      DoBegin(StaticData.workAsyncCallback, new JobCallback<X>(wr.Scheduler, this, xK));
    }
    internal override void TryAlt(ref Worker wr, int i, Cont<X> xK, Else xE) {
      var pk = xE.pk;
      var cb = new AltCallback<X>(wr.Scheduler, this, xK, pk, -1);
      var nk = Pick.ClaimAndAddNack(pk, i);
      if (null != nk) {
        try {
          cb.iar = DoBegin(StaticData.workAsyncCallback, cb);
        } catch (Exception e) {
          Pick.PickClaimedAndSetNacks(ref wr, i, pk);
          Handler.DoHandle(xK, ref wr, e);
          return;
        }
        var m = cb.me;
        if (0 <= m || m != Interlocked.CompareExchange(ref cb.me, i, m)) {
          Pick.PickClaimedAndSetNacks(ref wr, i, pk);
          Cont.Do(xK, ref wr, DoEnd(cb.iar));
        } else {
          nk.UnsafeAddReader(cb);
          Pick.Unclaim(pk);
          var j = i + 1;
          nk.I1 = j;
          xE.TryElse(ref wr, j);
        }
      }
    }
  }
}
