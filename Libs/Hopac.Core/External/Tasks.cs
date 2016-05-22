// Copyright (C) by Housemarque, Inc.

namespace Hopac.Core {
  using Microsoft.FSharp.Core;
  using System;
  using System.Runtime.CompilerServices;
  using System.Threading;
  using System.Threading.Tasks;

  ///
  public abstract class BindTaskWithResult<X, Y> : Job<Y> {
    private Task<X> xT;
    ///
    [MethodImpl(AggressiveInlining.Flag)]
    public Job<Y> InternalInit(Task<X> xT) { this.xT = xT; return this; }
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
        xT.ConfigureAwait(false).GetAwaiter().UnsafeOnCompleted(new State(wr.Scheduler, this, yK).Ready);
    }
  }

  ///
  public abstract class BindTask<Y> : Job<Y> {
    private Task uT;
    ///
    [MethodImpl(AggressiveInlining.Flag)]
    public Job<Y> InternalInit(Task uT) { this.uT = uT; return this; }
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
        uT.ConfigureAwait(false).GetAwaiter().UnsafeOnCompleted(new State(wr.Scheduler, this, yK).Ready);
    }
  }

  ///
  public abstract class TaskWithResultToJob<X> : Job<X> {
    ///
    public abstract Task<X> Start();
    ///
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
      var xT = this.Start();
      if (TaskStatus.RanToCompletion == xT.Status)
        Cont.Do(xK, ref wr, xT.Result);
      else
        xT.ConfigureAwait(false).GetAwaiter().UnsafeOnCompleted(new State(xT, xK, wr.Scheduler).Ready);
    }
  }

  ///
  public abstract class TaskToJob : Job<Unit> {
    ///
    public abstract Task Start();
    ///
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
      var uT = this.Start();
      if (TaskStatus.RanToCompletion == uT.Status)
        Work.Do(uK, ref wr);
      else
        uT.ConfigureAwait(false).GetAwaiter().UnsafeOnCompleted(new State(uT, uK, wr.Scheduler).Ready);
    }
  }

  ///
  public abstract class TaskWithResultToAlt<X> : Alt<X> {
    ///
    public abstract Task<X> Start(CancellationToken t);
    private sealed class State : Cont<Unit> {
      private CancellationTokenSource cts;
      private Pick pk;
      private int me;
      private Task<X> xT;
      private Cont<X> xK;
      private Scheduler sr;
      [MethodImpl(AggressiveInlining.Flag)]
      public State(CancellationTokenSource cts, Pick pk, int me, Task<X> xT, Cont<X> xK, Scheduler sr) {
        this.cts = cts;
        this.pk = pk;
        this.me = me;
        this.xT = xT;
        this.xK = xK;
        this.sr = sr;
      }
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
        var pk = this.pk;
        if (null == pk)
          goto DoCont;
        var cts = this.cts;
        if (null == cts)
          goto Done;
        this.cts = null;
        var picked = Pick.PickAndSetNacks(pk, ref wr, this.me);
        if (0 != picked)
          cts.Cancel();
        cts.Dispose();
        if (0 != picked)
          goto Done;
      DoCont:
        xK.DoCont(ref wr, xT.Result);
      Done:
        return;
      }
      internal override void DoCont(ref Worker wr, Unit value) {
        this.DoWork(ref wr);
      }
      public void Ready() {
        Worker.RunOnThisThread(this.sr, this);
      }
    }
    internal override void DoJob(ref Worker wr, Cont<X> xK) {
      var xT = Start(new CancellationToken(false));
      if (TaskStatus.RanToCompletion == xT.Status)
        Cont.Do(xK, ref wr, xT.Result);
      else
        xT.ConfigureAwait(false).GetAwaiter().UnsafeOnCompleted(new State(xT, xK, wr.Scheduler).Ready);
    }
    internal override void TryAlt(ref Worker wr, int i, Cont<X> xK, Else xE) {
      var pk = xE.pk;
      var nk = Pick.ClaimAndAddNack(pk, i);
      if (null != nk) {
        var cts = new CancellationTokenSource();
        Task<X> xT;
        try {
          xT = this.Start(cts.Token);
        } catch (Exception e) {
          Pick.PickClaimedAndSetNacks(ref wr, i, pk);
          cts.Dispose();
          Handler.DoHandle(xK, ref wr, e);
          return;
        }
        if (TaskStatus.RanToCompletion == xT.Status) {
          Pick.PickClaimedAndSetNacks(ref wr, i, pk);
          cts.Dispose();
          Cont.Do(xK, ref wr, xT.Result);
        } else {
          var state = new State(cts, pk, i, xT, xK, wr.Scheduler);
          nk.UnsafeAddReader(state);
          Pick.Unclaim(pk);
          var j = i + 1;
          nk.I1 = j;
          xT.ConfigureAwait(false).GetAwaiter().UnsafeOnCompleted(state.Ready);
          xE.TryElse(ref wr, j);
        }
      }
    }
  }
}
