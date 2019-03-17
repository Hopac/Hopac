// Copyright (C) by Housemarque, Inc.

namespace Hopac.Core {
  using Microsoft.FSharp.Core;
  using System;
  using System.Runtime.CompilerServices;
  using System.Threading;
  using System.Threading.Tasks;

  ///
  public abstract class BindTask<X, Y> : Job<Y> {
    private Task<X> xT;
    ///
    [MethodImpl(AggressiveInlining.Flag)]
    public Job<Y> InternalInit(Task<X> xT) { this.xT = xT; return this; }
    ///
    public abstract Job<Y> Do(X x);
    private sealed class State : Work {
      private Scheduler sr;
      private BindTask<X, Y> yJ;
      private Cont<Y> yK;
      internal State(Scheduler sr, BindTask<X, Y> yJ, Cont<Y> yK) {
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
        var xT = yJ.xT;
        if (TaskStatus.Faulted == xT.Status)
          Handler.DoHandle(yK, ref wr, xT.Exception);
        else
          yJ.Do(yJ.xT.Result).DoJob(ref wr, yK);
      }
      internal void Ready() {
        Worker.ContinueOnThisThread(sr, this);
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
    sealed class State : Work {
      readonly Scheduler sr;
      readonly BindTask<Y> yJ;
      Cont<Y> yK;

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
        if (TaskStatus.Faulted == uT.Status)
          Handler.DoHandle(yK, ref wr, uT.Exception);
        else
          yJ.Do().DoJob(ref wr, yK);
      }
      internal void Ready() {
        Worker.ContinueOnThisThread(sr, this);
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

  internal sealed class TaskToJobAwaiter<X> : Work {
    readonly Task<X> xT;
    readonly Cont<X> xK;
    readonly Scheduler sr;

    [MethodImpl(AggressiveInlining.Flag)]
    public TaskToJobAwaiter(Task<X> xT, Cont<X> xK, Scheduler sr) {
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
      var xT = this.xT;
      if (xT.IsCompletedSuccessfully)
        xK.DoCont(ref wr, xT.Result);
      else
        xK.DoHandle(ref wr, xT.Exception);
    }
    public void Ready() {
      Worker.ContinueOnThisThread(this.sr, this);
    }
  }

  ///
  public abstract class TaskToJob<X> : Job<X> {
    ///
    public abstract Task<X> Start();
    internal override void DoJob(ref Worker wr, Cont<X> xK) {
      var xT = this.Start();
      if (TaskStatus.RanToCompletion == xT.Status)
        Cont.Do(xK, ref wr, xT.Result);
      else
        xT.ConfigureAwait(false).GetAwaiter().UnsafeOnCompleted(new TaskToJobAwaiter<X>(xT, xK, wr.Scheduler).Ready);
    }
  }

  internal sealed class ValueTaskToJobAwaiter<X> : Work {
    readonly ConfiguredValueTaskAwaitable<X>.ConfiguredValueTaskAwaiter xA;
    readonly Cont<X> xK;
    readonly Scheduler sr;

    public ValueTaskToJobAwaiter(ConfiguredValueTaskAwaitable<X>.ConfiguredValueTaskAwaiter xA, Cont<X> xK, Scheduler sr) {
      this.xA = xA;
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
      var xA = this.xA;
      try {
        xK.DoCont(ref wr, xA.GetResult());
      }
      catch (Exception e) {
        xK.DoHandle(ref wr, e);
      }
    }

    public void Ready() {
      Worker.ContinueOnThisThread(sr, this);
    }
  }

  public abstract class ValueTaskToJob<X> : Job<X> {
    public abstract ValueTask<X> Start();

    internal override void DoJob(ref Worker wr, Cont<X> xK) {
      var xVT = this.Start();
      if (xVT.IsCompletedSuccessfully)
        Cont.Do(xK, ref wr, xVT.Result);
      else {
        var a = xVT.ConfigureAwait(false).GetAwaiter();
        a.UnsafeOnCompleted(new ValueTaskToJobAwaiter<X>(a, xK, wr.Scheduler).Ready);
      }
    }
  }

  internal sealed class TaskToJobAwaiter : Work {
    readonly Task uT;
    readonly Cont<Unit> uK;
    readonly Scheduler sr;

    [MethodImpl(AggressiveInlining.Flag)]
    public TaskToJobAwaiter(Task uT, Cont<Unit> uK, Scheduler sr) {
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
      if (uT.IsCompletedSuccessfully) {
        uK.DoWork(ref wr);
      }
      else
        uK.DoHandle(ref wr, uT.Exception);
    }
    internal void Ready() {
      Worker.ContinueOnThisThread(sr, this);
    }
  }

  ///
  public abstract class TaskToJob : Job<Unit> {
    ///
    public abstract Task Start();
    internal override void DoJob(ref Worker wr, Cont<Unit> uK) {
      var uT = this.Start();
      if (TaskStatus.RanToCompletion == uT.Status)
        Work.Do(uK, ref wr);
      else
        uT.ConfigureAwait(false).GetAwaiter().UnsafeOnCompleted(new TaskToJobAwaiter(uT, uK, wr.Scheduler).Ready);
    }
  }
  
  internal sealed class ValueTaskToJobAwaiter : Work {
    readonly ConfiguredValueTaskAwaitable.ConfiguredValueTaskAwaiter xA;
    readonly Cont<Unit> uK;
    readonly Scheduler sr;

    [MethodImpl(AggressiveInlining.Flag)]
    public ValueTaskToJobAwaiter(ConfiguredValueTaskAwaitable.ConfiguredValueTaskAwaiter xA, Cont<Unit> uK, Scheduler sr) {
      this.xA = xA;
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
      var xA = this.xA;
      try {
        xA.GetResult();
        uK.DoWork(ref wr);
      }
      catch (Exception e) {
        uK.DoHandle(ref wr, e);
      }
    }

    internal void Ready() {
      Worker.ContinueOnThisThread(sr, this);
    }
  }

  public abstract class ValueTaskToJob : Job<Unit> {
    ///
    public abstract ValueTask Start();
    internal override void DoJob(ref Worker wr, Cont<Unit> uK) {
      var uVT = Start();
      if (uVT.IsCompletedSuccessfully)
        Work.Do(uK, ref wr);
      else {
        var a = uVT.ConfigureAwait(false).GetAwaiter();
        a.UnsafeOnCompleted(new ValueTaskToJobAwaiter(a, uK, wr.Scheduler).Ready);
      }
    }
  }

  internal sealed class TaskToAltAwaiter<X> : Cont<Unit> {
    private CancellationTokenSource cts;
    private Pick pk;
    private int me;
    private Task<X> xT;
    private Cont<X> xK;
    private Scheduler sr;
    [MethodImpl(AggressiveInlining.Flag)]
    public TaskToAltAwaiter(CancellationTokenSource cts, Pick pk, int me, Task<X> xT, Cont<X> xK, Scheduler sr) {
      this.cts = cts;
      this.pk = pk;
      this.me = me;
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
      var xT = this.xT;
      if (TaskStatus.Faulted == xT.Status)
        xK.DoHandle(ref wr, xT.Exception);
      else
        xK.DoCont(ref wr, xT.Result);
    Done:
      return;
    }
    internal override void DoCont(ref Worker wr, Unit value) {
      this.DoWork(ref wr);
    }
    public void Ready() {
      Worker.ContinueOnThisThread(this.sr, this);
    }
  }

  ///
  public abstract class TaskToAlt<X> : Alt<X> {
    ///
    public abstract Task<X> Start(CancellationToken t);
    internal override void DoJob(ref Worker wr, Cont<X> xK) {
      var xT = Start(new CancellationToken(false));
      if (TaskStatus.RanToCompletion == xT.Status)
        Cont.Do(xK, ref wr, xT.Result);
      else
        xT.ConfigureAwait(false).GetAwaiter().UnsafeOnCompleted(new TaskToJobAwaiter<X>(xT, xK, wr.Scheduler).Ready);
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
          var state = new TaskToAltAwaiter<X>(cts, pk, i, xT, xK, wr.Scheduler);
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

  internal sealed class TaskToAltAwaiter : Cont<Unit> {
    private CancellationTokenSource cts;
    private Pick pk;
    private int me;
    private Task uT;
    private Cont<Unit> uK;
    private Scheduler sr;
    [MethodImpl(AggressiveInlining.Flag)]
    public TaskToAltAwaiter(CancellationTokenSource cts, Pick pk, int me, Task uT, Cont<Unit> uK, Scheduler sr) {
      this.cts = cts;
      this.pk = pk;
      this.me = me;
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
      var pk = this.pk;
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
      var uT = this.uT;
      if (TaskStatus.Faulted == uT.Status)
        uK.DoHandle(ref wr, uT.Exception);
      else {
        uT.Wait();
        uK.DoWork(ref wr);
      }
    Done:
      return;
    }
    internal override void DoCont(ref Worker wr, Unit value) {
      this.DoWork(ref wr);
    }
    public void Ready() {
      Worker.ContinueOnThisThread(this.sr, this);
    }
  }

  ///
  public abstract class TaskToAlt : Alt<Unit> {
    ///
    public abstract Task Start(CancellationToken t);
    internal override void DoJob(ref Worker wr, Cont<Unit> uK) {
      var uT = Start(new CancellationToken(false));
      if (TaskStatus.RanToCompletion == uT.Status)
        Work.Do(uK, ref wr);
      else
        uT.ConfigureAwait(false).GetAwaiter().UnsafeOnCompleted(new TaskToJobAwaiter(uT, uK, wr.Scheduler).Ready);
    }
    internal override void TryAlt(ref Worker wr, int i, Cont<Unit> uK, Else uE) {
      var pk = uE.pk;
      var nk = Pick.ClaimAndAddNack(pk, i);
      if (null != nk) {
        var cts = new CancellationTokenSource();
        Task uT;
        try {
          uT = this.Start(cts.Token);
        } catch (Exception e) {
          Pick.PickClaimedAndSetNacks(ref wr, i, pk);
          cts.Dispose();
          Handler.DoHandle(uK, ref wr, e);
          return;
        }
        if (TaskStatus.RanToCompletion == uT.Status) {
          Pick.PickClaimedAndSetNacks(ref wr, i, pk);
          cts.Dispose();
          Work.Do(uK, ref wr);
        } else {
          var state = new TaskToAltAwaiter(cts, pk, i, uT, uK, wr.Scheduler);
          nk.UnsafeAddReader(state);
          Pick.Unclaim(pk);
          var j = i + 1;
          nk.I1 = j;
          uT.ConfigureAwait(false).GetAwaiter().UnsafeOnCompleted(state.Ready);
          uE.TryElse(ref wr, j);
        }
      }
    }
  }
}
