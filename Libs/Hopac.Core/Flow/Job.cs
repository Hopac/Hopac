// Copyright (C) by Housemarque, Inc.

namespace Hopac {
  using Microsoft.FSharp.Core;
  using Hopac.Core;
  using System.Runtime.CompilerServices;
  using System;

  /// <summary>Represents a lightweight thread of execution.</summary>
  public abstract class Job<T> {
    internal abstract void DoJob(ref Worker wr, Cont<T> aK);
  }

  namespace Core {
    internal static class Job {
      [MethodImpl(AggressiveInlining.Flag)]
      internal static void Do<T>(Job<T> tJ, ref Worker wr, Cont<T> tK) {
#if TRAMPOLINE
        unsafe {
          byte stack;
          void* ptr = &stack;
          if (ptr < wr.StackLimit) {
            var w = new JobWork<T>(tJ, tK);
            w.Next = wr.WorkStack;
            wr.WorkStack = w;
          } else {
            wr.Handler = tK;
            tJ.DoJob(ref wr, tK);
          }
        }
#else
        tK.DoCont(ref wr, value);
#endif
      }
    }

    internal sealed class JobWork<T> : Work {
      internal Job<T> tJ;
      internal Cont<T> tK;

      [MethodImpl(AggressiveInlining.Flag)]
      internal JobWork(Job<T> tJ, Cont<T> tK) {
        this.tJ = tJ;
        this.tK = tK;
      }

      internal override Proc GetProc() {
        return Handler.GetProc(tK);
      }

      internal override void DoHandle(ref Worker wr, Exception e) {
        Handler.DoHandle(tK, ref wr, e);
      }

      internal override void DoWork(ref Worker wr) {
        tJ.DoJob(ref wr, tK);
      }
    }
  }
}
