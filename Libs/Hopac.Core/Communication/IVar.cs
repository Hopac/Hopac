// Copyright (C) by Housemarque, Inc.

namespace Hopac {
  using Microsoft.FSharp.Core;
  using Hopac.Core;
  using System.Runtime.CompilerServices;
  using System.Threading;
  using System;

  /// <summary>Represents a write once variable.</summary>
  public class IVar<T> : Promise<T> {
    /// <summary>Creates a new write once variable.</summary>
    [MethodImpl(AggressiveInlining.Flag)]
    public IVar() { }

    /// <summary>Creates a new write once variable with the given value.</summary>
    [MethodImpl(AggressiveInlining.Flag)]
    public IVar(T t) : base(t) { }

    /// <summary>Creates a new write once variable with the given failure exception.</summary>
    [MethodImpl(AggressiveInlining.Flag)]
    public IVar(Exception e) : base(e) { }

    /// Internal implementation detail.
    public sealed class Fill : Job<Unit> {
      private readonly IVar<T> tI;
      private readonly T t;

      /// Internal implementation detail.
      [MethodImpl(AggressiveInlining.Flag)]
      public Fill(IVar<T> tI, T t) {
        this.tI = tI;
        this.t = t;
      }

      internal override void DoJob(ref Worker wr, Cont<Unit> uK) {
        var tI = this.tI;
      Spin:
        var state = tI.State;
        if (state < Delayed) goto Spin;
        if (state != Interlocked.CompareExchange(ref tI.State, ~state, state)) goto Spin;

        if (state > Running) goto IVarFull;

        tI.Value = this.t;
        tI.State = HasValue;

        WaitQueue.PickReaders(ref tI.Readers, tI.Value, ref wr);
        Work.Do(uK, ref wr);
        return;

      IVarFull:
        tI.State = state;
        uK.DoHandle(ref wr, new Exception("IVar full"));
      }
    }

    /// Internal implementation detail.
    public sealed class TryFill : Job<Unit> {
      private readonly IVar<T> tI;
      private readonly T t;

      /// Internal implementation detail.
      [MethodImpl(AggressiveInlining.Flag)]
      public TryFill(IVar<T> tI, T t) {
        this.tI = tI;
        this.t = t;
      }

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
    public sealed class TryFillFailure : Job<Unit> {
      private IVar<T> tI;
      private Exception e;

      /// Internal implementation detail.
      [MethodImpl(AggressiveInlining.Flag)]
      public TryFillFailure(IVar<T> tI, Exception e) {
        this.tI = tI;
        this.e = e;
      }

      internal override void DoJob(ref Worker wr, Cont<Unit> uK) {
        var tI = this.tI;
      Spin:
        var state = tI.State;
        if (state < Delayed) goto Spin;
        if (state > Running) goto Done;
        if (state != Interlocked.CompareExchange(ref tI.State, ~state, state)) goto Spin;

        var readers = tI.Readers;
        var e = this.e;
        tI.Readers = new Fail<T>(e);
        tI.State = HasExn;

        WaitQueue.FailReaders(readers, e, ref wr);
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
        tI.State = state;
        uK.DoHandle(ref wr, new Exception("IVar full"));
      }
    }
  }
}
