// Copyright (C) by Housemarque, Inc.

namespace Hopac.Core {
  using Microsoft.FSharp.Core;
  using System;
  using System.Runtime.CompilerServices;
  using System.Threading.Tasks;

  /// Internal implementation detail.
  public sealed class AwaitTaskWithResult<T> : Job<T> {
    private readonly Task<T> task;

    /// Internal implementation detail.
    [MethodImpl(AggressiveInlining.Flag)]
    public AwaitTaskWithResult(Task<T> task) {
      this.task = task;
    }

    private sealed class State : Work {
      private readonly Task<T> task;
      private readonly Cont<T> aK;
      private readonly Scheduler sr;

      [MethodImpl(AggressiveInlining.Flag)]
      public State(Task<T> task, Cont<T> aK, Scheduler sr) {
        this.task = task;
        this.aK = aK;
        this.sr = sr;
      }

      internal override Proc GetProc() {
        return Handler.GetProc(aK);
      }

      internal override void DoHandle(ref Worker wr, Exception e) {
        Handler.DoHandle(aK, ref wr, e);
      }

      internal override void DoWork(ref Worker wr) {
        this.aK.DoCont(ref wr, this.task.Result);
      }

      public void Ready() {
        Worker.RunOnThisThread(this.sr, this);
      }
    }

    internal override void DoJob(ref Worker wr, Cont<T> aK) {
      var task = this.task;
      var state = new State(task, aK, wr.Scheduler);
      var awaiter = task.GetAwaiter();
      awaiter.UnsafeOnCompleted(state.Ready);
    }
  }

  /// Internal implementation detail.
  public sealed class AwaitTask : Job<Unit> {
    private readonly Task task;

    /// Internal implementation detail.
    [MethodImpl(AggressiveInlining.Flag)]
    public AwaitTask(Task task) {
      this.task = task;
    }

    private sealed class State : Work {
      private readonly Task task;
      private readonly Cont<Unit> uK;
      private readonly Scheduler sr;

      [MethodImpl(AggressiveInlining.Flag)]
      public State(Task task, Cont<Unit> uK, Scheduler sr) {
        this.task = task;
        this.uK = uK;
        this.sr = sr;
      }

      internal override Proc GetProc() {
        return Handler.GetProc(uK);
      }

      internal override void DoHandle(ref Worker wr, Exception e) {
        Handler.DoHandle(uK, ref wr, e);
      }

      internal override void DoWork(ref Worker wr) {
        if (this.task.Status == TaskStatus.RanToCompletion)
          this.uK.DoWork(ref wr);
        else
          Handler.DoHandle(this.uK, ref wr, this.task.Exception);
      }

      public void Ready() {
        Worker.RunOnThisThread(this.sr, this);
      }
    }

    internal override void DoJob(ref Worker wr, Cont<Unit> uK) {
      var task = this.task;
      var state = new State(task, uK, wr.Scheduler);
      var awaiter = task.GetAwaiter();
      awaiter.UnsafeOnCompleted(state.Ready);
    }
  }
}
