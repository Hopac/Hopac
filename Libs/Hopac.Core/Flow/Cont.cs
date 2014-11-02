// Copyright (C) by Housemarque, Inc.

namespace Hopac.Core {
  using Microsoft.FSharp.Core;
  using Hopac.Core;
  using System;
  using System.Runtime.CompilerServices;

  /// <summary>Represents a continuation of a parallel job.</summary>
  public abstract class Cont<T> : Work {
    internal T Value;

    internal virtual Pick GetPick(ref int me) {
      return null;
    }

    /// Use DoCont when NOT invoking continuation from a Job or Alt.
    internal abstract void DoCont(ref Worker wr, T value);
  }

  ///
  public abstract class ContIterate<X, Y> : Cont<X> {
    internal Cont<Y> yK;
    ///
    public ContIterate(X x, Cont<Y> yK) { this.Value = x; this.yK = yK; }
    ///
    public abstract Job<X> Do(X x);
    internal override Proc GetProc(ref Worker wr) {
      return Handler.GetProc(ref wr, ref yK);
    }
    internal override void DoHandle(ref Worker wr, Exception e) {
      Handler.DoHandle(yK, ref wr, e);
    }
    internal override void DoWork(ref Worker wr) {
      Do(this.Value).DoJob(ref wr, this);
    }
    internal override void DoCont(ref Worker wr, X x) {
      Do(x).DoJob(ref wr, this);
    }
  }

  internal sealed class Fail<T> : Cont<T> {
    internal Exception exn;

    internal Fail(Exception exn) {
      this.exn = exn;
    }

    internal override Proc GetProc(ref Worker wr) {
      throw new NotImplementedException();
    }

    internal override void DoHandle(ref Worker wr, Exception e) {
      throw new NotImplementedException();
    }

    internal override void DoCont(ref Worker wr, T value) {
      throw new NotImplementedException();
    }

    internal override void DoWork(ref Worker wr) {
      throw new NotImplementedException();
    }
  }

  internal static class Cont {
    /// Use Cont.Do when invoking continuation from a Job or Alt.
    [MethodImpl(AggressiveInlining.Flag)]
    internal static void Do<T>(Cont<T> tK, ref Worker wr, T value) {
#if TRAMPOLINE
      unsafe {
        if (Unsafe.GetStackPtr() < wr.StackLimit) {
          tK.Value = value;
          tK.Next = wr.WorkStack;
          wr.WorkStack = tK;
        } else {
          tK.DoCont(ref wr, value);
        }
      }
#else
      tK.DoCont(ref wr, value);
#endif
    }
  }

  internal sealed class FailCont<T> : Cont<T> {
    private Handler hr;
    private readonly Exception e;

    [MethodImpl(AggressiveInlining.Flag)]
    internal FailCont(Handler hr, Exception e) {
      this.hr = hr;
      this.e = e;
    }

    internal override Proc GetProc(ref Worker wr) {
      throw new NotImplementedException();
    }

    internal override void DoHandle(ref Worker wr, Exception e) {
      Handler.DoHandle(this.hr, ref wr, e);
    }

    internal override void DoWork(ref Worker wr) {
      Handler.DoHandle(this.hr, ref wr, this.e);
    }

    internal override void DoCont(ref Worker wr, T value) {
      Handler.DoHandle(this.hr, ref wr, this.e);
    }
  }

  internal abstract class Cont_State<T, S> : Cont<T> {
    internal S State;

    [MethodImpl(AggressiveInlining.Flag)]
    internal Cont_State() { }

    [MethodImpl(AggressiveInlining.Flag)]
    internal Cont_State(S s) { State = s; }
  }

  internal abstract class Cont_State<T, S1, S2> : Cont<T> {
    internal S1 State1;
    internal S2 State2;

    [MethodImpl(AggressiveInlining.Flag)]
    internal Cont_State() { }

    [MethodImpl(AggressiveInlining.Flag)]
    internal Cont_State(S1 s1, S2 s2) { State1 = s1; State2 = s2; }
  }

  internal abstract class Cont_State<T, S1, S2, S3> : Cont<T> {
    internal S1 State1;
    internal S2 State2;
    internal S3 State3;

    [MethodImpl(AggressiveInlining.Flag)]
    internal Cont_State() { }

    [MethodImpl(AggressiveInlining.Flag)]
    internal Cont_State(S1 s1, S2 s2, S3 s3) { State1 = s1; State2 = s2; State3 = s3; }
  }
}
