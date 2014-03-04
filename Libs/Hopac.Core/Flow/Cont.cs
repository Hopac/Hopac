// Copyright (C) by Housemarque, Inc.

namespace Hopac {
  using Microsoft.FSharp.Core;
  using Hopac.Core;
  using System;
  using System.Runtime.CompilerServices;

  /// <summary>Represents a continuation of a parallel job.</summary>
  internal abstract class Cont<T> : Work {
    /// Internal implementation detail.
    internal T Value;

    internal virtual Pick GetPick(ref int me) {
      return null;
    }

    /// Internal implementation detail.
    internal override void DoWork(ref Worker wr) {
      this.DoCont(ref wr, this.Value);
    }

    /// Use DoCont when NOT invoking continuation from a Job or Alt.
    internal abstract void DoCont(ref Worker wr, T value);
  }

  internal static class Cont {
    /// Use Cont.Do when invoking continuation from a Job or Alt.
    [MethodImpl(AggressiveInlining.Flag)]
    internal static void Do<T>(Cont<T> tK, ref Worker wr, T value) {
#if TRAMPOLINE
      unsafe {
        byte stack;
        void *ptr = &stack;
        if (ptr < wr.StackLimit) {
          tK.Value = value;
          tK.Next = wr.WorkStack;
          wr.WorkStack = tK;
        } else {
          tK.DoCont(ref wr, value);
        }
      }
#else
      tK.DoContAbs(ref wr, value);
#endif
    }
    /// Use Cont.Do when invoking continuation from a Job or Alt.
    [MethodImpl(AggressiveInlining.Flag)]
    internal static void Do(Cont<Unit> tK, ref Worker wr, Unit value) {
#if TRAMPOLINE
      unsafe {
        byte stack;
        void* ptr = &stack;
        if (ptr < wr.StackLimit) {
          tK.Next = wr.WorkStack;
          wr.WorkStack = tK;
        } else {
          tK.DoCont(ref wr, null);
        }
      }
#else
      tK.DoContAbs(ref wr, null);
#endif
    }
  }

  namespace Core {
    /// Internal implementation detail.
    internal sealed class FailCont<T> : Cont<T> {
      private readonly Handler hr;
      private readonly Exception e;

      /// Internal implementation detail.
      [MethodImpl(AggressiveInlining.Flag)]
      internal FailCont(Handler hr, Exception e) {
        this.hr = hr;
        this.e = e;
      }

      /// Internal implementation detail.
      internal override void DoHandle(ref Worker wr, Exception e) {
        this.hr.DoHandle(ref wr, e);
      }

      /// Internal implementation detail.
      internal override void DoCont(ref Worker wr, T value) {
        this.hr.DoHandle(ref wr, this.e);
      }
    }

    /// Internal implementation detail.
    internal abstract class Cont_State<T, S> : Cont<T> {
      /// Internal implementation detail.
      internal S State;

      /// Internal implementation detail.
      [MethodImpl(AggressiveInlining.Flag)]
      internal Cont_State() { }

      /// Internal implementation detail.
      [MethodImpl(AggressiveInlining.Flag)]
      internal Cont_State(S s) { State = s; }
    }

    /// Internal implementation detail.
    internal abstract class Cont_State<T, S1, S2> : Cont<T> {
      /// Internal implementation detail.
      internal S1 State1;
      /// Internal implementation detail.
      internal S2 State2;

      /// Internal implementation detail.
      [MethodImpl(AggressiveInlining.Flag)]
      internal Cont_State() { }

      /// Internal implementation detail.
      [MethodImpl(AggressiveInlining.Flag)]
      internal Cont_State(S1 s1, S2 s2) { State1 = s1; State2 = s2; }
    }
  }
}
