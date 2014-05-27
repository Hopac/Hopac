// Copyright (C) by Housemarque, Inc.

namespace Hopac {
  using Microsoft.FSharp.Core;
  using Hopac.Core;
  using System.Runtime.CompilerServices;
  using System.Threading;
  using System.Diagnostics;
  using System;

  /// <summary>Represents a dynamic latch.</summary>
  public class Latch : Alt<Unit> {
    internal volatile int State;
    internal volatile int Count;
    internal Cont<Unit> Awaiters;
    internal readonly Job<Unit> DecrementOp;

    ///
    public Latch(int count) {
      Debug.Assert(0 < count);
      this.Count = count;
      this.DecrementOp = new LatchDecrement(this);
    }

    ///
    public void Increment() { Interlocked.Increment(ref Count); }

    ///
    public Job<Unit> Decrement() { return DecrementOp; }

    internal void Lock() {
    Spin:
      var state = this.State;
      if (state < 0) goto Spin;
      if (Interlocked.Exchange(ref this.State, ~state) < state) goto Spin;
    }

    internal override void DoJob(ref Worker wr, Cont<Unit> uK) {
    Spin:
      if (0 == this.Count) goto Done;
      var state = this.State;
      if (state < 0) goto Spin;
      if (Interlocked.Exchange(ref this.State, ~state) < state) goto Spin;

      if (0 == this.Count) goto UnlockAndDone;

      WaitQueue.AddTaker(ref this.Awaiters, uK);
      this.State = 0;
      return;

    UnlockAndDone:
      this.State = 0;
    Done:
      Work.Do(uK, ref wr);
    }

    internal override void TryAlt(ref Worker wr, int i, Cont<Unit> uK, Else aE) {
      var pkSelf = aE.pk;
    Spin:
      if (0 == this.Count) goto TryPick;
      var state = this.State;
      if (state < 0) goto Spin;
      if (Interlocked.Exchange(ref this.State, ~state) < state) goto Spin;

      if (0 == this.Count) goto UnlockAndTryPick;
      
      WaitQueue.AddTaker(ref this.Awaiters, i, pkSelf, uK);
      this.State = 0;
      aE.TryElse(ref wr, i + 1);
      return;

    UnlockAndTryPick:
      this.State = 0;
    TryPick:
      var st = Pick.TryPick(pkSelf);
      if (st > 0) goto AlreadyPicked;
      if (st < 0) goto TryPick;

      Pick.SetNacks(ref wr, i, pkSelf);

      Work.Do(uK, ref wr);
    AlreadyPicked:
      return;
    }
  }

  namespace Core {
    internal sealed class LatchDecrement : Job<Unit> {
      internal Latch latch;

      internal LatchDecrement(Latch latch) { this.latch = latch; }

      internal override void DoJob(ref Worker wr, Cont<Unit> uK) {
        var latch = this.latch;
        var n = Interlocked.Decrement(ref latch.Count);
        if (0 == n) {
          latch.Lock();
          WaitQueue.PickReaders(ref latch.Awaiters, null, ref wr);
        }
        Work.Do(uK, ref wr);
      }
    }
  }
}
