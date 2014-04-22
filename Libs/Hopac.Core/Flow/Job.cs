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
    ///
    public abstract class JobBind<X, Y> : Job<Y> {
      internal Job<X> xJ;

      ///
      public JobBind(Job<X> xJ) { this.xJ = xJ; }

      ///
      public abstract Job<Y> Do(X x);

      internal override void DoJob(ref Worker wr, Cont<Y> yK) {
        xJ.DoJob(ref wr, new ContBind(this, yK));
      }

      internal sealed class ContBind : Cont<X> {
        internal JobBind<X, Y> yJ;
        internal Cont<Y> yK;

        internal ContBind(JobBind<X, Y> yJ, Cont<Y> yK) {
          this.yJ = yJ;
          this.yK = yK;
        }

        internal override Proc GetProc(ref Worker wr) {
          return Handler.GetProc(ref wr, ref yK);
        }

        internal override void DoHandle(ref Worker wr, Exception e) {
          Handler.DoHandle(yK, ref wr, e);
        }

        internal override void DoWork(ref Worker wr) {
          yJ.Do(this.Value).DoJob(ref wr, yK);
        }

        internal override void DoCont(ref Worker wr, X x) {
          yJ.Do(x).DoJob(ref wr, yK);
        }
      }
    }

    ///
    public abstract class JobMap<X, Y> : Job<Y> {
      internal Job<X> xJ;

      ///
      public JobMap(Job<X> xJ) { this.xJ = xJ; }

      ///
      public abstract Y Do(X x);

      internal override void DoJob(ref Worker wr, Cont<Y> yK) {
        xJ.DoJob(ref wr, new ContMap(this, yK));
      }

      internal sealed class ContMap : Cont<X> {
        internal JobMap<X, Y> yJ;
        internal Cont<Y> yK;

        internal ContMap(JobMap<X, Y> yJ, Cont<Y> yK) {
          this.yJ = yJ;
          this.yK = yK;
        }

        internal override Proc GetProc(ref Worker wr) {
          return Handler.GetProc(ref wr, ref yK);
        }

        internal override void DoHandle(ref Worker wr, Exception e) {
          Handler.DoHandle(yK, ref wr, e);
        }

        internal override void DoWork(ref Worker wr) {
          yK.DoCont(ref wr, yJ.Do(this.Value));
        }

        internal override void DoCont(ref Worker wr, X x) {
          yK.DoCont(ref wr, yJ.Do(x));
        }
      }
    }

    ///
    public abstract class JobDelay<X> : Job<X> {
      ///
      public abstract Job<X> Do();

      internal override void DoJob(ref Worker wr, Cont<X> xK) {
        Do().DoJob(ref wr, xK);
      }
    }

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

      internal override Proc GetProc(ref Worker wr) {
        return Handler.GetProc(ref wr, ref tK);
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
