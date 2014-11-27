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
      private Job<X> xJ;
      ///
      public JobBind(Job<X> xJ) { this.xJ = xJ; }
      ///
      public abstract Job<Y> Do(X x);
      internal override void DoJob(ref Worker wr, Cont<Y> yK) {
        xJ.DoJob(ref wr, new ContBind(this, yK));
      }
      private sealed class ContBind : Cont<X> {
        private JobBind<X, Y> yJ;
        private Cont<Y> yK;
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
    public sealed class JobJoin<X, JX> : Job<X> where JX : Job<X> {
      private Job<JX> xJJ;
      ///
      public JobJoin(Job<JX> xJJ) { this.xJJ = xJJ; }
      internal override void DoJob(ref Worker wr, Cont<X> xK) {
        xJJ.DoJob(ref wr, new ContJoin(xK));
      }
      private sealed class ContJoin : Cont<JX> {
        private Cont<X> xK;
        internal ContJoin(Cont<X> xK) { this.xK = xK; }
        internal override Proc GetProc(ref Worker wr) {
          return Handler.GetProc(ref wr, ref xK);
        }
        internal override void DoHandle(ref Worker wr, Exception e) {
          Handler.DoHandle(xK, ref wr, e);
        }
        internal override void DoWork(ref Worker wr) {
          this.Value.DoJob(ref wr, xK);
        }
        internal override void DoCont(ref Worker wr, JX xJ) {
          xJ.DoJob(ref wr, xK);         
        }
      }
    }

    ///
    public abstract class JobTryIn<X, Y> : Job<Y> {
      private Job<X> xJ;
      ///
      public JobTryIn(Job<X> xJ) { this.xJ = xJ; }
      ///
      public abstract Job<Y> DoIn(X x);
      ///
      public abstract Job<Y> DoExn(Exception e);
      internal override void DoJob(ref Worker wr, Cont<Y> yK) {
        var xK = new ContTryIn(this, yK);
        wr.Handler = xK;
        xJ.DoJob(ref wr, xK);
      }
      private sealed class ContTryIn : Cont<X> {
        private JobTryIn<X, Y> yJ;
        private Cont<Y> yK;
        internal ContTryIn(JobTryIn<X, Y> yJ, Cont<Y> yK) {
          this.yJ = yJ;
          this.yK = yK;
        }
        internal override Proc GetProc(ref Worker wr) {
          return Handler.GetProc(ref wr, ref yK);
        }
        internal override void DoHandle(ref Worker wr, Exception e) {
          var yK = this.yK;
          wr.Handler = yK;
          yJ.DoExn(e).DoJob(ref wr, yK);
        }
        internal override void DoWork(ref Worker wr) {
          var yK = this.yK;
          wr.Handler = yK;
          yJ.DoIn(this.Value).DoJob(ref wr, yK);
        }
        internal override void DoCont(ref Worker wr, X x) {
          var yK = this.yK;
          wr.Handler = yK;
          yJ.DoIn(x).DoJob(ref wr, yK);
        }
      }
    }

    ///
    public abstract class JobTryWith<X> : Job<X> {
      private Job<X> xJ;
      ///
      public JobTryWith(Job<X> xJ) { this.xJ = xJ; }
      ///
      public abstract Job<X> DoExn(Exception e);
      internal override void DoJob(ref Worker wr, Cont<X> xK_) {
        var xK = new ContTryWith(this, xK_);
        wr.Handler = xK;
        xJ.DoJob(ref wr, xK);
      }
      private sealed class ContTryWith : Cont<X> {
        private JobTryWith<X> xJ;
        private Cont<X> xK;
        internal ContTryWith(JobTryWith<X> xJ, Cont<X> xK) {
          this.xJ = xJ;
          this.xK = xK;
        }
        internal override Proc GetProc(ref Worker wr) {
          return Handler.GetProc(ref wr, ref xK);
        }
        internal override void DoHandle(ref Worker wr, Exception e) {
          var xK = this.xK;
          wr.Handler = xK;
          xJ.DoExn(e).DoJob(ref wr, xK);
        }
        internal override void DoWork(ref Worker wr) {
          var xK = this.xK;
          wr.Handler = xK;
          xK.DoCont(ref wr, this.Value);
        }
        internal override void DoCont(ref Worker wr, X x) {
          var xK = this.xK;
          wr.Handler = xK;
          xK.DoCont(ref wr, x);
        }
      }
    }

    ///
    public abstract class JobMap<X, Y> : Job<Y> {
      private Job<X> xJ;
      ///
      public JobMap(Job<X> xJ) { this.xJ = xJ; }
      ///
      public abstract Y Do(X x);
      internal override void DoJob(ref Worker wr, Cont<Y> yK) {
        xJ.DoJob(ref wr, new ContMap(this, yK));
      }
      internal sealed class ContMap : Cont<X> {
        private JobMap<X, Y> yJ;
        private Cont<Y> yK;
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
    public abstract class JobRun<X> : Job<X> {
      ///
      public abstract Work Do(Cont<X> xK);
      internal override void DoJob(ref Worker wr, Cont<X> xK) {
        Work.Do(Do(xK), ref wr);
      }
    }

    ///
    public abstract class JobStart : Job<Unit> {
      ///
      public abstract Work Do();
      internal override void DoJob(ref Worker wr, Cont<Unit> uK) {
        Worker.PushNew(ref wr, Do());
        Work.Do(uK, ref wr);
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

    ///
    public abstract class JobThunk<X> : Job<X> {
      ///
      public abstract X Do();
      internal override void DoJob(ref Worker wr, Cont<X> xK) {
        Cont.Do(xK, ref wr, Do());
      }
    }

    internal static class Job {
      [MethodImpl(AggressiveInlining.Flag)]
      internal static void Do<T>(Job<T> tJ, ref Worker wr, Cont<T> tK) {
#if TRAMPOLINE
        unsafe {
          if (Unsafe.GetStackPtr() < wr.StackLimit) {
            var w = new JobWork<T>(tJ, tK);
            w.Next = wr.WorkStack;
            wr.WorkStack = w;
          } else {
            wr.Handler = tK;
            tJ.DoJob(ref wr, tK);
          }
        }
#else
        wr.Handler = tK;
        tJ.DoJob(ref wr, tK);
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
