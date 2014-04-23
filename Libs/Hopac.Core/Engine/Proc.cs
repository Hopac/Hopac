// Copyright (C) by Housemarque, Inc.

namespace Hopac {
  using Microsoft.FSharp.Core;
  using System;
  using System.Threading;
  using Hopac.Core;

  /// <summary>Represents a joinable lightweight thread of execution.</summary>
  public sealed class Proc : Alt<Unit> {
    internal volatile int State;
    internal Cont<Unit> Joiners;

    internal const int Locked = -1;
    internal const int Running = 0;
    internal const int Terminated = 1;

    internal void Terminate(ref Worker wr) {
    Spin:
      var state = this.State;
      if (state < Running) goto Spin;
      if (state != Interlocked.CompareExchange(ref this.State, Terminated, state)) goto Spin;

      var joiners =  this.Joiners;
      if (null == joiners) return;
      this.Joiners = null;
      int me = 0;
      Work cursor = joiners;
    TryJoiner:
      var joiner = cursor as Cont<Unit>;
      cursor = cursor.Next;
      var pk = joiner.GetPick(ref me);
      if (null == pk)
        goto GotJoiner;

    TryPick:
      var st = Pick.TryPick(pk);
      if (st > 0) goto TryNextJoiner;
      if (st < 0) goto TryPick;

      Pick.SetNacks(ref wr, me, pk);
    GotJoiner:
      Worker.Push(ref wr, joiner);

    TryNextJoiner:
      if (cursor != joiners)
        goto TryJoiner;
    }

    internal override void DoJob(ref Worker wr, Cont<Unit> uK) {
    Spin:
      var state = this.State;
      if (state > Running) goto Terminated;
      if (state < Running) goto Spin;
      if (state != Interlocked.CompareExchange(ref this.State, Locked, state)) goto Spin;

      WaitQueue.AddTaker(ref this.Joiners, uK);
      this.State = Running;
      return;

    Terminated:
      Work.Do(uK, ref wr);
    }

    internal override void TryAlt(ref Worker wr, int i, Cont<Unit> uK, Else uE) {
      var pk = uE.pk;
    Spin:
      var state = this.State;
      if (state > Running) goto TryPick;
      if (state < Running) goto Spin;
      if (state != Interlocked.CompareExchange(ref this.State, Locked, state)) goto Spin;

      WaitQueue.AddTaker(ref this.Joiners, i, pk, uK);
      this.State = Running;
      uE.TryElse(ref wr, i + 1);
      return;

    TryPick:
      var st = Pick.TryPick(pk);
      if (st > 0) goto AlreadyPicked;
      if (st < 0) goto TryPick;

      Pick.SetNacks(ref wr, i, pk);

      Work.Do(uK, ref wr);
    AlreadyPicked:
      return;
    }
  }

  namespace Core {
    internal class ProcFinalizer<X> : Cont<X> {
      private Proc pr;
      private Scheduler sr;
      internal ProcFinalizer(Scheduler sr, Proc pr) {
        this.sr = sr;
        this.pr = pr;
      }
      ~ProcFinalizer() {
        Worker.RunOnThisThread(this.sr, this);
      }
      internal override Proc GetProc(ref Worker wr) {
        return this.pr;
      }
      internal override void DoHandle(ref Worker wr, Exception e) {
        GC.SuppressFinalize(this);
        this.pr.Terminate(ref wr);
        Handler.DoHandle(null, ref wr, e);
      }
      internal override void DoWork(ref Worker wr) {
        GC.SuppressFinalize(this);
        this.pr.Terminate(ref wr);
      }
      internal override void DoCont(ref Worker wr, X value) {
        GC.SuppressFinalize(this);
        this.pr.Terminate(ref wr);
      }
    }
  }
}
