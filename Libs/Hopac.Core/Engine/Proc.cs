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

    internal override void TryAlt(ref Worker wr, int i, Pick pk, Cont<Unit> uK, Else<Unit> uE) {
    Spin:
      var state = this.State;
      if (state > Running) goto TryPick;
      if (state < Running) goto Spin;
      if (state != Interlocked.CompareExchange(ref this.State, Locked, state)) goto Spin;

      WaitQueue.AddTaker(ref this.Joiners, i, pk, uK);
      this.State = Running;
      uE.TryElse(ref wr, i + 1, pk, uK);
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
}
