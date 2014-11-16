// Copyright (C) by Housemarque, Inc.

namespace Hopac {
  using Microsoft.FSharp.Core;
  using Hopac.Core;
  using System.Runtime.CompilerServices;
  using System.Threading;
  using System;

  /// <summary>Represents a write once variable.</summary>
  public class IVar<T> : Promise<T> {
    /// Internal implementation detail.
    public IVar() { }
    /// Internal implementation detail.
    public IVar(T t) : base(t) { }
    /// Internal implementation detail.
    public IVar(Exception e) : base(e) { }

    /// Internal implementation detail.
    public class Fill : Job<Unit> {
      internal readonly IVar<T> tI;
      internal readonly T t;

      /// Internal implementation detail.
      [MethodImpl(AggressiveInlining.Flag)]
      public Fill(IVar<T> tI, T t) {
        this.tI = tI;
        this.t = t;
      }

      internal override void DoJob(ref Worker wr, Cont<Unit> uK) {
        var tI = this.tI;
        tI.Value = this.t; // This assumes correct usage of IVar.
      Spin:
        var state = tI.State;
        if (state < Delayed) goto Spin;
        if (state != Interlocked.CompareExchange(ref tI.State, HasValue, state)) goto Spin;

        if (state > Running) {
          uK.DoHandle(ref wr, new Exception("IVar full"));
        } else {
          WaitQueue.PickReaders(ref tI.Readers, tI.Value, ref wr);
          Work.Do(uK, ref wr);
        }
      }
    }

    /// Internal implementation detail.
    public sealed class TryFill : Fill {
      /// Internal implementation detail.
      [MethodImpl(AggressiveInlining.Flag)]
      public TryFill(IVar<T> tI, T t) : base (tI, t) { }

      internal override void DoJob(ref Worker wr, Cont<Unit> uK) {
        var tI = this.tI;
      Spin:
        var state = tI.State;
        if (state < Delayed) goto Spin;
        if (state > Running) goto Done;
        if (state != Interlocked.CompareExchange(ref tI.State, ~state, state)) goto Spin;

        tI.Value = this.t;
        tI.State = HasValue;

        WaitQueue.PickReaders(ref tI.Readers, tI.Value, ref wr);
      Done:
        Work.Do(uK, ref wr);
      }
    }

    /// Internal implementation detail.
    public sealed class FillFailure : Job<Unit> {
      private IVar<T> tI;
      private Exception e;

      /// Internal implementation detail.
      [MethodImpl(AggressiveInlining.Flag)]
      public FillFailure(IVar<T> tI, Exception e) {
        this.tI = tI;
        this.e = e;
      }

      internal override void DoJob(ref Worker wr, Cont<Unit> uK) {
        var tI = this.tI;
      Spin:
        var state = tI.State;
        if (state < Delayed) goto Spin;
        if (state != Interlocked.CompareExchange(ref tI.State, ~state, state)) goto Spin;

        if (state > Running) goto IVarFull;

        var readers = tI.Readers;
        var e = this.e;
        tI.Readers = new Fail<T>(e);
        tI.State = HasExn;

        WaitQueue.FailReaders(readers, e, ref wr);
        Work.Do(uK, ref wr);
        return;

      IVarFull:
        uK.DoHandle(ref wr, new Exception("IVar full"));
      }
    }
  }
}
