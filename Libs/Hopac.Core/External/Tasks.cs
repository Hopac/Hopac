// Copyright (C) by Housemarque, Inc.

namespace Hopac.Core {
  using Microsoft.FSharp.Core;
  using System;
  using System.Runtime.CompilerServices;
  using System.Threading.Tasks;

  ///
  public abstract class BindTaskWithResult<X, Y> : Job<Y> {
    private Task<X> xT;
    ///
    public BindTaskWithResult(Task<X> xT) { this.xT = xT; }
    ///
    public abstract Job<Y> Do(X x);
    private sealed class State : Work {
      private Scheduler sr;
      private BindTaskWithResult<X, Y> yJ;
      private Cont<Y> yK;
      internal State(Scheduler sr, BindTaskWithResult<X, Y> yJ, Cont<Y> yK) {
        this.sr = sr;
        this.yJ = yJ;
        this.yK = yK;
      }
      internal override Proc GetProc(ref Worker wr) {
        return Handler.GetProc(ref wr, ref yK);
      }
      internal override void DoHandle(ref Worker wr, Exception e) {
        Handler.DoHandle(yK, ref wr, e);
      }
      internal override void DoWork(ref Worker wr) {
        var yJ = this.yJ;
        yJ.Do(yJ.xT.Result).DoJob(ref wr, yK);
      }
      internal void Ready() {
        Worker.RunOnThisThread(sr, this);
      }
    }
    internal override void DoJob(ref Worker wr, Cont<Y> yK) {
      var xT = this.xT;
      if (TaskStatus.RanToCompletion == xT.Status)
        Job.Do(Do(xT.Result), ref wr, yK);
      else
        xT.GetAwaiter().UnsafeOnCompleted(new State(wr.Scheduler, this, yK).Ready);
    }
  }

  ///
  public abstract class BindTask<Y> : Job<Y> {
    private Task uT;
    ///
    public BindTask(Task uT) { this.uT = uT; }
    ///
    public abstract Job<Y> Do();
    private sealed class State : Work {
      private Scheduler sr;
      private BindTask<Y> yJ;
      private Cont<Y> yK;
      internal State(Scheduler sr, BindTask<Y> yJ, Cont<Y> yK) {
        this.sr = sr;
        this.yJ = yJ;
        this.yK = yK;
      }
      internal override Proc GetProc(ref Worker wr) {
        return Handler.GetProc(ref wr, ref yK);
      }
      internal override void DoHandle(ref Worker wr, Exception e) {
        Handler.DoHandle(yK, ref wr, e);
      }
      internal override void DoWork(ref Worker wr) {
        var yJ = this.yJ;
        var uT = yJ.uT;
        if (TaskStatus.RanToCompletion == uT.Status)
          yJ.Do().DoJob(ref wr, yK);
        else
          Handler.DoHandle(yK, ref wr, uT.Exception);
      }
      internal void Ready() {
        Worker.RunOnThisThread(sr, this);
      }
    }
    internal override void DoJob(ref Worker wr, Cont<Y> yK) {
      var uT = this.uT;
      if (TaskStatus.RanToCompletion == uT.Status)
        Job.Do(Do(), ref wr, yK);
      else
        uT.GetAwaiter().UnsafeOnCompleted(new State(wr.Scheduler, this, yK).Ready);
    }
  }

  ///
  public sealed class AwaitTaskWithResult<X> : Job<X> {
    private Task<X> xT;
    ///
    [MethodImpl(AggressiveInlining.Flag)]
    public AwaitTaskWithResult(Task<X> xT) { this.xT = xT; }
    private sealed class State : Work {
      private Task<X> xT;
      private Cont<X> xK;
      private Scheduler sr;
      [MethodImpl(AggressiveInlining.Flag)]
      public State(Task<X> xT, Cont<X> xK, Scheduler sr) {
        this.xT = xT;
        this.xK = xK;
        this.sr = sr;
      }
      internal override Proc GetProc(ref Worker wr) {
        return xK.GetProc(ref wr);
      }
      internal override void DoHandle(ref Worker wr, Exception e) {
        xK.DoHandle(ref wr, e);
      }
      internal override void DoWork(ref Worker wr) {
        xK.DoCont(ref wr, xT.Result);
      }
      public void Ready() {
        Worker.RunOnThisThread(this.sr, this);
      }
    }
    internal override void DoJob(ref Worker wr, Cont<X> xK) {
      var xT = this.xT;
      if (TaskStatus.RanToCompletion == xT.Status)
        Cont.Do(xK, ref wr, xT.Result);
      else
        xT.GetAwaiter().UnsafeOnCompleted(new State(xT, xK, wr.Scheduler).Ready);
    }
  }

  ///
  public sealed class AwaitTask : Job<Unit> {
    private Task uT;
    ///
    [MethodImpl(AggressiveInlining.Flag)]
    public AwaitTask(Task uT) { this.uT = uT; }
    private sealed class State : Work {
      private Task uT;
      private Cont<Unit> uK;
      private Scheduler sr;
      [MethodImpl(AggressiveInlining.Flag)]
      public State(Task uT, Cont<Unit> uK, Scheduler sr) {
        this.uT = uT;
        this.uK = uK;
        this.sr = sr;
      }
      internal override Proc GetProc(ref Worker wr) {
        return uK.GetProc(ref wr);
      }
      internal override void DoHandle(ref Worker wr, Exception e) {
        uK.DoHandle(ref wr, e);
      }
      internal override void DoWork(ref Worker wr) {
        var uT = this.uT;
        if (TaskStatus.RanToCompletion == uT.Status)
          uK.DoWork(ref wr);
        else
          uK.DoHandle(ref wr, uT.Exception);
      }
      internal void Ready() {
        Worker.RunOnThisThread(this.sr, this);
      }
    }
    internal override void DoJob(ref Worker wr, Cont<Unit> uK) {
      var uT = this.uT;
      if (TaskStatus.RanToCompletion == uT.Status)
        Work.Do(uK, ref wr);
      else
        uT.GetAwaiter().UnsafeOnCompleted(new State(uT, uK, wr.Scheduler).Ready);
    }
  }
}
