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

    internal abstract void DoWork(ref Worker wr);

    [MethodImpl(AggressiveInlining.Flag)]
    internal static void Do(Work work, ref Worker wr) {
#if TRAMPOLINE
      unsafe {
        byte stack;
        void* ptr = &stack;
        if (ptr < wr.StackLimit) {
          work.Next = wr.WorkStack;
          wr.WorkStack = work;
        } else {
          work.DoWork(ref wr);
        }
      }
#else
      work.DoWork(ref wr);
#endif
    }
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
