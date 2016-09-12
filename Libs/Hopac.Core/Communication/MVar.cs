// Copyright (C) by Housemarque, Inc.

namespace Hopac {
  using Microsoft.FSharp.Core;
  using Hopac.Core;
  using System;
  using System.Threading;
  using System.Runtime.CompilerServices;

  internal static class MVar {
    internal const int Demand = -1;
    internal const int Empty = 0;
    internal const int Full = 1;
    internal const int Locked = 2;
  }

  /// <summary>Represents a serialized variable.</summary>
  public class MVar<T> : Alt<T> {
    internal T Value;
    internal volatile int State;
    internal Cont<T> Takers;

    /// <summary>Creates a new serialized variable.</summary>
    [MethodImpl(AggressiveInlining.Flag)]
    public MVar() { }

    /// <summary>Creates a new serialized variable that is initially filled with the given value.</summary>
    [MethodImpl(AggressiveInlining.Flag)]
    public MVar(T t) {
      this.Value = t;
      this.State = MVar.Full;
    }

    internal override void DoJob(ref Worker wr, Cont<T> tK) {
    Spin:
      var state = this.State;
      if (state == MVar.Locked) goto Spin;
      if (state != Interlocked.CompareExchange(ref this.State, MVar.Locked, state))
        goto Spin;

      if (state <= MVar.Empty)
        goto EmptyOrDemand;

      T value = this.Value;
      this.Value = default(T); // Avoid memory leaks.
      this.State = MVar.Empty;
      Cont.Do(tK, ref wr, value);
      return;

    EmptyOrDemand:
      WaitQueue.AddTaker(ref this.Takers, tK);
      this.State = MVar.Demand;
    }

    internal override void TryAlt(ref Worker wr, int i, Cont<T> tK, Else tE) {
      var pkSelf = tE.pk;
    Spin:
      var state = this.State;
      if (state == MVar.Locked) goto Spin;
      if (state != Interlocked.CompareExchange(ref this.State, MVar.Locked, state))
        goto Spin;

      if (state <= MVar.Empty)
        goto EmptyOrDemand;

    TryPick:
      var stSelf = Pick.TryPick(pkSelf);
      if (stSelf > 0) goto AlreadyPicked;
      if (stSelf < 0) goto TryPick;

      Pick.SetNacks(ref wr, i, pkSelf);

      T t = this.Value;
      this.Value = default(T);
      this.State = MVar.Empty;
      Cont.Do(tK, ref wr, t);
      return;

    AlreadyPicked:
      this.State = MVar.Full;
      return;

    EmptyOrDemand:
      WaitQueue.AddTaker(ref this.Takers, i, pkSelf, tK);
      this.State = MVar.Demand;
      tE.TryElse(ref wr, i + 1);
      return;
    }

    internal void Fill(ref Worker wr, T t) {
    TryNextTaker:
      var state = this.State;
      if (state == MVar.Locked)
        goto TryNextTaker;
      if (state != Interlocked.CompareExchange(ref this.State, MVar.Locked, state))
        goto TryNextTaker;

      if (state == MVar.Empty)
        goto WasEmpty;

      var tail = this.Takers;
      if (null == tail)
        goto MVarFull;
      var cursor = tail.Next;
      if (tail == cursor) {
        this.Takers = null;
        this.State = MVar.Empty;
      } else {
        tail.Next = cursor.Next;
        this.State = MVar.Demand;
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
      tail.Value = t;
      Worker.Push(ref wr, tail);
      return;

    WasEmpty:
      this.Value = t;
      this.State = MVar.Full;
      return;

    MVarFull:
      this.State = MVar.Full;
      throw new Exception("MVar full");
    }
  }

  namespace Core {
    internal sealed class MVarReadCont<T> : Cont<T> {
      private MVar<T> tM;
      private Cont<T> tK;
      internal MVarReadCont(MVar<T> tM, Cont<T> tK) {
        this.tM = tM;
        this.tK = tK;
      }
      internal override Proc GetProc(ref Worker wr) {
        return Handler.GetProc(ref wr, ref tK);
      }
      internal override void DoHandle(ref Worker wr, Exception e) {
        Handler.DoHandle(tK, ref wr, e);
      }
      internal override void DoWork(ref Worker wr) {
        DoCont(ref wr, this.Value);
      }
      internal override void DoCont(ref Worker wr, T t) {
        tM.Fill(ref wr, t);
        Cont.Do(tK, ref wr, t);
      }
    }

    ///
    public sealed class MVarRead<T> : Alt<T> {
      private MVar<T> tM;
      ///
      [MethodImpl(AggressiveInlining.Flag)]
      public MVarRead(MVar<T> tM) { this.tM = tM; }
      internal override void DoJob(ref Worker wr, Cont<T> tK) {
        var tM = this.tM;
      Spin:
        var state = tM.State;
        if (state == MVar.Locked) goto Spin;
        if (state != Interlocked.CompareExchange(ref tM.State, MVar.Locked, state))
          goto Spin;

        if (state <= MVar.Empty)
          goto EmptyOrDemand;

        var t = tM.Value;
        tM.State = MVar.Full;
        Cont.Do(tK, ref wr, t);
        return;

      EmptyOrDemand:
        WaitQueue.AddTaker(ref tM.Takers, new MVarReadCont<T>(tM, tK));
        tM.State = MVar.Demand;
      }
      internal override void TryAlt(ref Worker wr, int i, Cont<T> tK, Else tE) {
        var tM = this.tM;
        var pkSelf = tE.pk;
      Spin:
        var state = tM.State;
        if (state == MVar.Locked) goto Spin;
        if (state != Interlocked.CompareExchange(ref tM.State, MVar.Locked, state))
          goto Spin;

        if (state <= MVar.Empty)
          goto EmptyOrDemand;

      TryPick:
        var stSelf = Pick.TryPick(pkSelf);
        if (stSelf > 0) goto AlreadyPicked;
        if (stSelf < 0) goto TryPick;

        Pick.SetNacks(ref wr, i, pkSelf);

        var t = tM.Value;
        tM.State = MVar.Full;
        Cont.Do(tK, ref wr, t);
        return;

      AlreadyPicked:
        tM.State = MVar.Full;
        return;

      EmptyOrDemand:
        WaitQueue.AddTaker(ref tM.Takers, i, pkSelf, new MVarReadCont<T>(tM, tK));
        tM.State = MVar.Demand;
        tE.TryElse(ref wr, i + 1);
        return;
      }
    }

    internal sealed class MVarModifyFunCont<T, Y> : Cont<T> {
      private MVarModifyFun<T, Y> tm;
      private Cont<Y> yK;
      internal MVarModifyFunCont(MVarModifyFun<T, Y> tm, Cont<Y> yK) {
        this.tm = tm;
        this.yK = yK;
      }
      internal override Proc GetProc(ref Worker wr) {
        return Handler.GetProc(ref wr, ref yK);
      }
      internal override void DoHandle(ref Worker wr, Exception e) {
        Handler.DoHandle(yK, ref wr, e);
      }
      internal override void DoWork(ref Worker wr) {
        DoCont(ref wr, this.Value);
      }
      internal override void DoCont(ref Worker wr, T t) {
        var tm = this.tm;
        var yK = this.yK;
        tm.tM.Fill(ref wr, tm.Do(t, ref yK.Value));
        Work.Do(yK, ref wr);
      }
    }

    ///
    public abstract class MVarModifyFun<T, Y> : Alt<Y> {
      internal MVar<T> tM;
      ///
      [MethodImpl(AggressiveInlining.Flag)]
      public Alt<Y> InternalInit(MVar<T> tM) { this.tM = tM; return this; }
      ///
      public abstract T Do(T t, ref Y y);
      internal override void DoJob(ref Worker wr, Cont<Y> yK) {
        var tM = this.tM;
      Spin:
        var state = tM.State;
        if (state == MVar.Locked) goto Spin;
        if (state != Interlocked.CompareExchange(ref tM.State, MVar.Locked, state))
          goto Spin;

        if (state <= MVar.Empty)
          goto EmptyOrDemand;

        var t = tM.Value;
        tM.State = MVar.Empty;

        tM.Fill(ref wr, Do(t, ref yK.Value));
        Work.Do(yK, ref wr);
        return;

      EmptyOrDemand:
        WaitQueue.AddTaker(ref tM.Takers, new MVarModifyFunCont<T, Y>(this, yK));
        tM.State = MVar.Demand;
      }
      internal override void TryAlt(ref Worker wr, int i, Cont<Y> yK, Else tE) {
        var tM = this.tM;
        var pkSelf = tE.pk;
      Spin:
        var state = tM.State;
        if (state == MVar.Locked) goto Spin;
        if (state != Interlocked.CompareExchange(ref tM.State, MVar.Locked, state))
          goto Spin;

        if (state <= MVar.Empty)
          goto EmptyOrDemand;

      TryPick:
        var stSelf = Pick.TryPick(pkSelf);
        if (stSelf > 0) goto AlreadyPicked;
        if (stSelf < 0) goto TryPick;

        Pick.SetNacks(ref wr, i, pkSelf);

        var t = tM.Value;
        tM.State = MVar.Empty;

        tM.Fill(ref wr, Do(t, ref yK.Value));
        Work.Do(yK, ref wr);
        return;

      AlreadyPicked:
        tM.State = MVar.Full;
        return;

      EmptyOrDemand:
        WaitQueue.AddTaker(ref tM.Takers, i, pkSelf, new MVarModifyFunCont<T, Y>(this, yK));
        tM.State = MVar.Demand;
        tE.TryElse(ref wr, i + 1);
        return;
      }
    }

    internal sealed class MVarTryModifyFunCont<T, Y> : Cont<T> {
      private MVarTryModifyFun<T, Y> tm;
      private Cont<Y> yK;
      [MethodImpl(AggressiveInlining.Flag)]
      internal MVarTryModifyFunCont(MVarTryModifyFun<T, Y> tm, Cont<Y> yK) {
        this.tm = tm;
        this.yK = yK;
      }
      internal override Proc GetProc(ref Worker wr) {
        return Handler.GetProc(ref wr, ref yK);
      }
      internal override void DoHandle(ref Worker wr, Exception e) {
        Handler.DoHandle(yK, ref wr, e);
      }
      [MethodImpl(AggressiveInlining.Flag)]
      internal static void Do(MVarTryModifyFun<T, Y> tm, Cont<Y> yK, ref Worker wr, T t) {
        Exception raised = null;
        try {
          t = tm.Do(t, ref yK.Value);
        } catch (Exception e) {
          raised = e;
        }
        tm.tM.Fill(ref wr, t);
        if (null != raised)
          Handler.DoHandle(yK, ref wr, raised);
        else
          Work.Do(yK, ref wr);
      }
      internal override void DoWork(ref Worker wr) { Do(tm, yK, ref wr, this.Value); }
      internal override void DoCont(ref Worker wr, T t) { Do(tm, yK, ref wr, t); }
    }

    ///
    public abstract class MVarTryModifyFun<T, Y> : Alt<Y> {
      internal MVar<T> tM;
      ///
      [MethodImpl(AggressiveInlining.Flag)]
      public Alt<Y> InternalInit(MVar<T> tM) { this.tM = tM; return this; }
      ///
      public abstract T Do(T t, ref Y y);
      internal override void DoJob(ref Worker wr, Cont<Y> yK) {
        var tM = this.tM;
      Spin:
        var state = tM.State;
        if (state == MVar.Locked) goto Spin;
        if (state != Interlocked.CompareExchange(ref tM.State, MVar.Locked, state))
          goto Spin;

        if (state <= MVar.Empty)
          goto EmptyOrDemand;

        var t = tM.Value;
        tM.State = MVar.Empty;

        MVarTryModifyFunCont<T, Y>.Do(this, yK, ref wr, t);
        return;

      EmptyOrDemand:
        WaitQueue.AddTaker(ref tM.Takers, new MVarTryModifyFunCont<T, Y>(this, yK));
        tM.State = MVar.Demand;
      }
      internal override void TryAlt(ref Worker wr, int i, Cont<Y> yK, Else tE) {
        var tM = this.tM;
        var pkSelf = tE.pk;
      Spin:
        var state = tM.State;
        if (state == MVar.Locked) goto Spin;
        if (state != Interlocked.CompareExchange(ref tM.State, MVar.Locked, state))
          goto Spin;

        if (state <= MVar.Empty)
          goto EmptyOrDemand;

      TryPick:
        var stSelf = Pick.TryPick(pkSelf);
        if (stSelf > 0) goto AlreadyPicked;
        if (stSelf < 0) goto TryPick;

        Pick.SetNacks(ref wr, i, pkSelf);

        var t = tM.Value;
        tM.State = MVar.Empty;

        MVarTryModifyFunCont<T, Y>.Do(this, yK, ref wr, t);
        return;

      AlreadyPicked:
        tM.State = MVar.Full;
        return;

      EmptyOrDemand:
        WaitQueue.AddTaker(ref tM.Takers, i, pkSelf, new MVarTryModifyFunCont<T, Y>(this, yK));
        tM.State = MVar.Demand;
        tE.TryElse(ref wr, i + 1);
        return;
      }
    }

    ///
    public sealed class MVarFill<T> : Job<Unit> {
      private MVar<T> tM;
      private T t;
      ///
      [MethodImpl(AggressiveInlining.Flag)]
      public MVarFill(MVar<T> tM, T t) {
        this.tM = tM;
        this.t = t;
      }
      internal override void DoJob(ref Worker wr, Cont<Unit> uK) {
        tM.Fill(ref wr, t);
        Work.Do(uK, ref wr);
      }
    }
  }
}
