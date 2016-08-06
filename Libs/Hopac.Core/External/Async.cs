// Copyright (C) by Vesa Karvonen

namespace Hopac.Core {
  using Microsoft.FSharp.Core;
  using System;
  using System.Runtime.CompilerServices;
  using System.Threading;
  using System.Threading.Tasks;

  ///
  public interface IBindAsyncCont<X> {
    ///
    void Success(X x);
    ///
    void Failure(Exception e);
  }

  ///
  internal sealed class BindAsyncCont<X, Y> : Work, IBindAsyncCont<X> {
    private Scheduler sr;
    private BindAsync<X, Y> ba;
    private Cont<Y> yK;
    private X x;
    internal BindAsyncCont(Scheduler sr, BindAsync<X, Y> ba, Cont<Y> yK) {
      this.sr = sr;
      this.ba = ba;
      this.yK = yK;
    }
    internal override Proc GetProc(ref Worker wr) {
      return Handler.GetProc(ref wr, ref yK);
    }
    internal override void DoHandle(ref Worker wr, Exception e) {
      Handler.DoHandle(yK, ref wr, e);
    }
    internal override void DoWork(ref Worker wr) {
      ba.Do(this.x).DoJob(ref wr, yK);
    }
    ///
    public void Success(X x) {
      this.x = x;
      Worker.RunOnThisThread(sr, this);
    }
    ///
    public void Failure(Exception e) {
      Worker.RunOnThisThread(sr, new FailWork(e, yK));
    }
  }

  ///
  public abstract class BindAsync<X, Y> : Job<Y> {
    ///
    public abstract Job<Y> Do(X x);
    ///
    public abstract void Start(IBindAsyncCont<X> xK);
    internal override void DoJob(ref Worker wr, Cont<Y> yK) {
      this.Start(new BindAsyncCont<X, Y>(wr.Scheduler, this, yK));
    }
  }

  ///
  public sealed class FromAsyncCont<X> {
    private Scheduler sr;
    private Cont<X> xK;
    internal FromAsyncCont(Scheduler sr, Cont<X> xK) {
      this.sr = sr;
      this.xK = xK;
    }
    ///
    public void Success(X x) {
      var xK = this.xK;
      xK.Value = x;
      Worker.RunOnThisThread(sr, xK);
    }
    ///
    public void Failure(Exception e) {
      Worker.RunOnThisThread(sr, new FailWork(e, xK));
    }
  }

  ///
  public abstract class FromAsync<X> : Job<X> {
    ///
    public abstract void Start(FromAsyncCont<X> xK);
    internal override void DoJob(ref Worker wr, Cont<X> xK) {
      this.Start(new FromAsyncCont<X>(wr.Scheduler, xK));
    }
  }
}
