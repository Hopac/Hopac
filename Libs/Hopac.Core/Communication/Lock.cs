// Copyright (C) by Housemarque, Inc.

namespace Hopac {
  using Microsoft.FSharp.Core;
  using Hopac.Core;
  using System;
  using System.Diagnostics;
  using System.Runtime.CompilerServices;
  using System.Threading;

  /// <summary>A non-recursive mutual exclusion lock for jobs.</summary>
  public class Lock {
    internal volatile Work Waiters;
    internal volatile int State;

    internal const int Free   = 0;
    internal const int Held   = 1;  // Some job has entered the lock.
    internal const int Locked = 2;  // Lock locked for modification.

    [MethodImpl(AggressiveInlining.Flag)]
    internal static void Exit(Lock l, ref Worker wr) {
    Spin:
      var state = l.State;
      if (state == Lock.Locked) goto Spin;
      if (state != Interlocked.CompareExchange(ref l.State, Lock.Locked, state)) goto Spin;

      Debug.Assert(state == Lock.Held);

      var tail = l.Waiters;
      if (null == tail) goto JustUnlock;
      var cursor = tail.Next;
      if (cursor == tail)
        l.Waiters = null;
      else
        tail.Next = cursor.Next;
      l.State = Lock.Held;
      Worker.Push(ref wr, cursor);
      return;

    JustUnlock:
      l.State = Lock.Free;
    }
  }

  namespace Core {
    /// Internal implementation detail.
    public sealed class LockDuringFun<T> : Job<T> {
      internal readonly Lock l;
      internal readonly FSharpFunc<Unit, T> tF;

      /// Internal implementation detail.
      [MethodImpl(AggressiveInlining.Flag)]
      public LockDuringFun(Lock l, FSharpFunc<Unit, T> tF) {
        this.l = l;
        this.tF = tF;
      }
  
      internal override void DoJob(ref Worker wr, Cont<T> tK) {
        var l = this.l;
      Spin:
        var state = l.State;
        if (state == Lock.Locked) goto Spin;
        if (state != Interlocked.CompareExchange(ref l.State, Lock.Locked, state)) goto Spin;

        if (state == Lock.Free)
          goto GotIt;

        var waiter = new GotIt(this.l, this.tF, tK);
        var tail = l.Waiters;
        if (tail == null) {
          waiter.Next = waiter;
        } else {
          waiter.Next = tail.Next;
          tail.Next = waiter;
        }
        l.Waiters = waiter;
        l.State = Lock.Held;
        return;

      GotIt:
        // We keep the lock in Locked stated for the duration of the function
        // call.  This avoids doing a second interlocked operation.
        T value;
        try {
          value = this.tF.Invoke(null);
        } catch (Exception e) {
          tK = new FailCont<T>(tK, e);
          value = default(T);
        }
        l.State = Lock.Free;
        Cont.Do(tK, ref wr, value);
      }

      private sealed class GotIt : Work {
        private readonly Lock l;
        private readonly FSharpFunc<Unit, T> tF;
        private Cont<T> tK;

        [MethodImpl(AggressiveInlining.Flag)]
        internal GotIt(Lock l, FSharpFunc<Unit, T> tF, Cont<T> tK) {
          this.l = l;
          this.tF = tF;
          this.tK = tK;
        }

        internal override Proc GetProc(ref Worker wr) {
          return Handler.GetProc(ref wr, ref tK);
        }

        internal override void DoHandle(ref Worker wr, Exception e) {
          var tK = this.tK;
          wr.Handler = tK;
          Lock.Exit(this.l, ref wr);
          Handler.DoHandle(tK, ref wr, e);
        }

        internal override void DoWork(ref Worker wr) {
          var value = this.tF.Invoke(null);
          var tK = this.tK;
          wr.Handler = tK;
          Lock.Exit(this.l, ref wr);
          Cont.Do(tK, ref wr, value);
        }
      }
    }

    /// Internal implementation detail.
    public sealed class LockDuringJob<T> : Job<T> {
      internal readonly Lock l;
      internal readonly Job<T> tJ;

      /// Internal implementation detail.
      [MethodImpl(AggressiveInlining.Flag)]
      public LockDuringJob(Lock l, Job<T> tJ) {
        this.l = l;
        this.tJ = tJ;
      }

      internal override void DoJob(ref Worker wr, Cont<T> tK) {
        var l = this.l;
        var tKPrime = new Cont(l, tK);
      Spin:
        var state = l.State;
        if (state == Lock.Locked) goto Spin;
        if (state != Interlocked.CompareExchange(ref l.State, state+1, state)) goto Spin;

        if (state == Lock.Free)
          goto GotIt;

        var waiter = new GotIt(tKPrime, this.tJ);
        var tail = l.Waiters;
        if (tail == null) {
          waiter.Next = waiter;
        } else {
          waiter.Next = tail.Next;
          tail.Next = waiter;
        }
        l.Waiters = waiter;
        l.State = Lock.Held;
        return;

      GotIt:
        wr.Handler = tKPrime;
        this.tJ.DoJob(ref wr, tKPrime);
      }

      private sealed class GotIt : Work {
        private Cont<T> tKPrime;
        private readonly Job<T> tJ;

        [MethodImpl(AggressiveInlining.Flag)]
        internal GotIt(Cont tKPrime, Job<T> tJ) {
          this.tKPrime = tKPrime;
          this.tJ = tJ;
        }

        internal override Proc GetProc(ref Worker wr) {
          return Handler.GetProc(ref wr, ref tKPrime);
        }

        internal override void DoHandle(ref Worker wr, Exception e) {
          Handler.DoHandle(this.tKPrime, ref wr, e);
        }

        internal override void DoWork(ref Worker wr) {
          this.tJ.DoJob(ref wr, this.tKPrime);
        }
      }

      private sealed class Cont : Cont<T> {
        private readonly Lock l;
        private Cont<T> tK;

        [MethodImpl(AggressiveInlining.Flag)]
        internal Cont(Lock l, Cont<T> tK) {
          this.l = l;
          this.tK = tK;
        }

        internal override Proc GetProc(ref Worker wr) {
          return Handler.GetProc(ref wr, ref tK);
        }

        internal override void DoHandle(ref Worker wr, Exception e) {
          var tK = this.tK;
          wr.Handler = tK;
          Lock.Exit(this.l, ref wr);
          Handler.DoHandle(tK, ref wr, e);
        }

        internal override void DoWork(ref Worker wr) {
          var tK = this.tK;
          wr.Handler = tK;
          Lock.Exit(this.l, ref wr);
          tK.DoCont(ref wr, this.Value);
        }

        internal override void DoCont(ref Worker wr, T value) {
          var tK = this.tK;
          wr.Handler = tK;
          Lock.Exit(this.l, ref wr);
          tK.DoCont(ref wr, value);
        }
      }
    }
  }
}
