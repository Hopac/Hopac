// Copyright (C) by Housemarque, Inc.

namespace Hopac {
  using Microsoft.FSharp.Core;
  using Hopac.Core;
  using System;
  using System.Threading;
  using System.Runtime.CompilerServices;

  /// <summary>Represents a serialized variable.</summary>
  public class MVar<T> : Alt<T> {
    internal T Value;
    internal volatile int State;
    internal Cont<T> Takers;

    internal const int Demand = -1;
    internal const int Empty = 0;
    internal const int Full = 1;
    internal const int Locked = 2;

    /// <summary>Creates an initially empty MVar.</summary>
    [MethodImpl(AggressiveInlining.Flag)]
    public MVar() { }

    /// <summary>Creates an MVar initially filled with the given
    /// value.</summary>
    [MethodImpl(AggressiveInlining.Flag)]
    public MVar(T x) {
      this.Value = x;
      this.State = Full;
    }

    internal override void DoJob(ref Worker wr, Cont<T> aK) {
    Spin:
      var state = this.State;
      if (state == Locked) goto Spin;
      if (state != Interlocked.CompareExchange(ref this.State, Locked, state))
        goto Spin;

      if (state <= Empty)
        goto EmptyOrDemand;

      T value = this.Value;
      this.Value = default(T); // Avoid memory leaks.
      this.State = Empty;
      Cont.Do(aK, ref wr, value);
      return;

    EmptyOrDemand:
      WaitQueue.AddTaker(ref this.Takers, aK);
      this.State = Demand;
    }

    internal override void TryAlt(ref Worker wr, int i, Cont<T> aK, Else aE) {
      var pkSelf = aE.pk;
    Spin:
      var state = this.State;
      if (state == Locked) goto Spin;
      if (state != Interlocked.CompareExchange(ref this.State, Locked, state))
        goto Spin;

      if (state <= Empty)
        goto EmptyOrDemand;

    TryPick:
      var stSelf = Pick.TryPick(pkSelf);
      if (stSelf > 0) goto AlreadyPicked;
      if (stSelf < 0) goto TryPick;

      Pick.SetNacks(ref wr, i, pkSelf);

      T value = this.Value;
      this.Value = default(T);
      this.State = Empty;
      Cont.Do(aK, ref wr, value);
      return;

    AlreadyPicked:
      this.State = Full;
      return;

    EmptyOrDemand:
      WaitQueue.AddTaker(ref this.Takers, i, pkSelf, aK);
      this.State = Demand;
      aE.TryElse(ref wr, i + 1);
      return;
    }
  }

  namespace Core {
    /// Internal implementation detail.
    public sealed class MVarFill<T> : Job<Unit> {
      private MVar<T> MV;
      private T X;

      /// Internal implementation detail.
      [MethodImpl(AggressiveInlining.Flag)]
      public MVarFill(MVar<T> mv, T x) {
        this.MV = mv;
        this.X = x;
      }

      internal override void DoJob(ref Worker wr, Cont<Unit> uK) {
        var mv = this.MV;
      TryNextTaker:
        var state = mv.State;
        if (state == MVar<T>.Locked)
          goto TryNextTaker;
        if (state != Interlocked.CompareExchange(ref mv.State, MVar<T>.Locked, state))
          goto TryNextTaker;

        if (state == MVar<T>.Empty)
          goto WasEmpty;

        var tail = mv.Takers;
        if (null == tail)
          goto MVarFull;
        var cursor = tail.Next;
        if (tail == cursor) {
          mv.Takers = null;
          mv.State = MVar<T>.Empty;
        } else {
          tail.Next = cursor.Next;
          mv.State = MVar<T>.Demand;
          tail = cursor as Cont<T>;
        }

        var taker = tail as Taker<T>;
        if (null == taker)
          goto GotTaker;
        var pk = taker.Pick;

      TryPick:
        int st = Pick.TryPick(pk);
        if (st > 0)
          goto TryNextTaker;
        if (st < 0)
          goto TryPick;

        Pick.SetNacks(ref wr, taker.Me, pk);

        tail = taker.Cont;
      GotTaker:
        tail.Value = this.X;
        Worker.Push(ref wr, tail);
        Work.Do(uK, ref wr);
        return;

      WasEmpty:
        mv.Value = this.X;
        mv.State = MVar<T>.Full;
        Work.Do(uK, ref wr);
        return;

      MVarFull:
        uK.DoHandle(ref wr, new Exception("MVar full"));
      }
    }
  }
}
