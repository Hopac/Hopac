// Copyright (C) by Housemarque, Inc.

namespace Hopac.Core {
  using System;

  internal abstract class WorkAsyncCallback : Work {
    internal abstract void Ready(IAsyncResult iar);
  }

  ///
  public abstract class JobFromBeginEnd<X> : Job<X> {
    ///
    public abstract IAsyncResult DoBegin(AsyncCallback acb, object state);
    ///
    public abstract X DoEnd(IAsyncResult iar);

    private class Callback : WorkAsyncCallback {
      private Scheduler sr;
      private JobFromBeginEnd<X> xJ;
      private Cont<X> xK;
      private IAsyncResult iar;
      internal Callback(Scheduler sr, JobFromBeginEnd<X> xJ, Cont<X> xK) {
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
    }
    internal override void DoJob(ref Worker wr, Cont<X> xK) {
      DoBegin(StaticData.workAsyncCallback, new Callback(wr.Scheduler, this, xK));
    }
  }
}
