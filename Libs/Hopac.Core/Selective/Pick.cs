// Copyright (C) by Housemarque, Inc.

namespace Hopac.Core {
  using Microsoft.FSharp.Core;
  using System;
  using System.Runtime.CompilerServices;
  using System.Threading;

  internal abstract class Pick : Else {
    internal Nack Nacks;
    internal volatile int State;

    internal const int Claimed   = -1;
    internal const int Available =  0;
    internal const int Picked    =  1;

    [MethodImpl(AggressiveInlining.Flag)]
    internal static int TryClaim(Pick pk) {
      return Interlocked.CompareExchange(ref pk.State, Claimed, Available);
    }

    [MethodImpl(AggressiveInlining.Flag)]
    internal static int Claim(Pick pk) {
    TryClaim:
      var st = TryClaim(pk);
      if (st < Available) goto TryClaim;
      return st;
    }

    [MethodImpl(AggressiveInlining.Flag)]
    internal static void ClaimAndDoJob<T>(Pick pk, ref Worker wr, Job<T> tJ, Cont<T> tK) {
    TryClaim:
      var st = TryClaim(pk);
      if (st < Available) goto TryClaim;
      if (st == Available) {
        wr.Handler = tK;
        tJ.DoJob(ref wr, tK);
      }
    }

    [MethodImpl(AggressiveInlining.Flag)]
    internal static void Unclaim(Pick pk) {
      pk.State = Available;
    }

    [MethodImpl(AggressiveInlining.Flag)]
    internal static void PickClaimedAndSetNacks(ref Worker wr, int i, Pick pk) {
      pk.State = Picked;
      SetNacks(ref wr, i, pk);
    }

    [MethodImpl(AggressiveInlining.Flag)]
    internal static int TryPick(Pick pk) {
      return Interlocked.CompareExchange(ref pk.State, Picked, Available);
    }

    [MethodImpl(AggressiveInlining.Flag)]
    internal static int DoPickOpt(Pick pk) {
      var st = Available;
      if (null == pk) goto Done;
    Retry:
      st = TryPick(pk);
      if (st < Available) goto Retry;
    Done:
      return st;
    }

    [MethodImpl(AggressiveInlining.Flag)]
    internal static Nack ClaimAndAddNack(Pick pk, int i0) {
    TryClaim:
      var state = pk.State;
      if (state > Available) goto AlreadyPicked;
      if (state < Available) goto TryClaim;
      if (0 != Interlocked.CompareExchange(ref pk.State, ~state, state)) goto TryClaim;

      var nk = new Nack(pk.Nacks, i0);
      pk.Nacks = nk;

      return nk; // Leaves the pick claimed on purpose!

    AlreadyPicked:
      return null;
    }

    [MethodImpl(AggressiveInlining.Flag)]
    internal static Nack TryAddNack(Pick pk, int i0, int i1) {
    TryClaim:
      var state = pk.State;
      if (state > Available) goto AlreadyPicked;
      if (state < Available) goto TryClaim;
      if (0 != Interlocked.CompareExchange(ref pk.State, ~state, state)) goto TryClaim;

      var nk = new Nack(pk.Nacks, i0, i1);
      pk.Nacks = nk;
      pk.State = Available;

      return nk;

    AlreadyPicked:
      return null;
    }

    [MethodImpl(AggressiveInlining.Flag)]
    internal static void SetNacks(ref Worker wr, int i, Pick pk) {
      Nack nk = pk.Nacks;
      if (null != nk) {
        pk.Nacks = null;
        SetNacks(ref wr, i, nk);
      }
    }

    private static void SetNacks(ref Worker wr, int i, Nack nk) {
      do {
        if (i < nk.I0 || nk.I1 <= i)
          Nack.Signal(ref wr, nk);
        nk = nk.Next;
      } while (null != nk);
    }

    internal static int PickAndSetNacks(Pick pk, ref Worker wr, int i) {
    TryPick:
      var state = Pick.TryPick(pk);
      if (state > Available) goto AlreadyPicked;
      if (state < Available) goto TryPick;

      SetNacks(ref wr, i, pk);

    AlreadyPicked:
      return state;
    }
  }

  internal abstract class Pick_State<S1> : Pick {
    internal S1 State1;

    [MethodImpl(AggressiveInlining.Flag)]
    internal Pick Init() {
      this.pk = this;
      return this;
    }

    [MethodImpl(AggressiveInlining.Flag)]
    internal Pick Init(S1 s1) {
      this.pk = this;
      this.State1 = s1;
      return this;
    }
  }

  internal abstract class Pick_State<S1, S2> : Pick {
    internal S1 State1;
    internal S2 State2;

    [MethodImpl(AggressiveInlining.Flag)]
    internal Pick Init(S1 s1, S2 s2) {
      this.pk = this;
      this.State1 = s1;
      this.State2 = s2;
      return this;
    }
  }
}
