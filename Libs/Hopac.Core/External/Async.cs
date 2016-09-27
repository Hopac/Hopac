// Copyright (C) by Vesa Karvonen

namespace Hopac.Core {
  using Microsoft.FSharp.Core;
  using System;
  using System.Runtime.CompilerServices;
  using System.Threading;
  using System.Threading.Tasks;

  ///
  public abstract class BindAsyncCont<X, Y> : Work {
    internal Cont<Y> yK;
    internal Scheduler sr;
    internal int state;
    internal X x;
    internal Exception e;
    ///
    public void InternalInit(Cont<Y> yK) { this.yK = yK; }
    internal override Proc GetProc(ref Worker wr) {
      return Handler.GetProc(ref wr, ref yK);
    }
    internal override void DoHandle(ref Worker wr, Exception e) {
      Handler.DoHandle(yK, ref wr, e);
    }
    internal override void DoWork(ref Worker wr) {
      var e = this.e;
      if (null != e)
        Handler.DoHandle(yK, ref wr, e);
      else
        Do(this.x).DoJob(ref wr, yK);
    }
    ///
    public abstract Job<Y> Do(X x);
    ///
    public void Success(X x) {
      this.x = x;
      var sr = this.sr;
      if (null != sr)
        goto Continue;
      if (0 == Interlocked.CompareExchange(ref this.state, 1, 0))
        return;
      sr = this.sr;
    Continue:
      Worker.ContinueOnThisThread(sr, this);
    }
    ///
    public void Failure(Exception e) {
      this.e = e;
      var sr = this.sr;
      if (null != sr)
        goto Continue;
      if (0 == Interlocked.CompareExchange(ref this.state, 1, 0))
        return;
      sr = this.sr;
    Continue:
      Worker.ContinueOnThisThread(sr, this);
    }
  }

  ///
  public abstract class BindAsync<X, Y> : Job<Y> {
    ///
    public abstract BindAsyncCont<X, Y> Start(Cont<Y> yK);
    internal override void DoJob(ref Worker wr, Cont<Y> yK) {
      var bac = this.Start(yK);
      if (0 != bac.state)
        goto Continue;
      bac.sr = wr.Scheduler;
      if (0 == Interlocked.CompareExchange(ref bac.state, 1, 0))
        return;
    Continue:
      Work.Do(bac, ref wr);
    }
  }

  ///
  public sealed class FromAsyncCont<X> {
    internal Cont<X> xK;
    internal Scheduler sr;
    internal int state;
    internal FromAsyncCont(Cont<X> xK) {
      this.xK = xK;
    }
    ///
    public void Success(X x) {
      var xK = this.xK;
      xK.Value = x;
      var sr = this.sr;
      if (null != sr)
        goto Continue;
      if (0 == Interlocked.CompareExchange(ref this.state, 1, 0))
        return;
      sr = this.sr;
    Continue:
      Worker.ContinueOnThisThread(sr, xK);
    }
    ///
    public void Failure(Exception e) {
      var eK = new FailCont<X>(xK, e);
      var sr = this.sr;
      if (null != sr)
        goto Continue;
      this.xK = eK;
      if (0 == Interlocked.CompareExchange(ref this.state, 1, 0))
        return;
      sr = this.sr;
    Continue:
      Worker.ContinueOnThisThread(sr, eK);
    }
  }

  ///
  public abstract class FromAsync<X> : Job<X> {
    ///
    public abstract void Start(FromAsyncCont<X> xK);
    internal override void DoJob(ref Worker wr, Cont<X> xK) {
      var fac = new FromAsyncCont<X>(xK);
      this.Start(fac);
      if (0 != fac.state)
        goto Continue;
      fac.sr = wr.Scheduler;
      if (0 == Interlocked.CompareExchange(ref fac.state, 1, 0))
        return;
    Continue:
      Work.Do(fac.xK, ref wr);
    }
  }
}
