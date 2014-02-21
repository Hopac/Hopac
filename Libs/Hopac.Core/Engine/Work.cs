// Copyright (C) by Housemarque, Inc.

namespace Hopac.Core {
  using System.Runtime.CompilerServices;
  using System;

  /// <summary>Work item.</summary>
  internal abstract class Work : Handler {
    internal Work Next;

    [MethodImpl(AggressiveInlining.Flag)]
    internal Work() { }

    [MethodImpl(AggressiveInlining.Flag)]
    internal Work(Work next) {
      this.Next = next;
    }

    /// Internal implementation detail.
    internal abstract void DoWork(ref Worker wr);
  }

  internal sealed class FailWork : Work {
    internal readonly Exception e;
    internal readonly Handler hr;

    [MethodImpl(AggressiveInlining.Flag)]
    internal FailWork(Work next, Exception e, Handler hr) {
      this.Next = next;
      this.e = e;
      this.hr = hr;
    }

    [MethodImpl(AggressiveInlining.Flag)]
    internal FailWork(Exception e, Handler hr) {
      this.e = e;
      this.hr = hr;
    }

    internal override void DoHandle(ref Worker wr, Exception e) {
      this.hr.DoHandle(ref wr, e);
    }

    internal override void DoWork(ref Worker wr) {
      this.hr.DoHandle(ref wr, this.e);
    }
  }
}
