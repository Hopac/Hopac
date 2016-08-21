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
      throw new Exception("MVar full");
    }
  }

  namespace Core {
    internal sealed class MVarReadCont<T> : Cont<T> {
      private MVar<T> tM;
      private Cont<T> tK;
      ///
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

    /// Internal implementation detail.
    public sealed class MVarRead<T> : Alt<T> {
      private MVar<T> tM;
      ///
      [MethodImpl(AggressiveInlining.Flag)]
      public MVarRead(MVar<T> tM) { this.tM = tM; }
      ///
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

        T t = tM.Value;
        tM.State = MVar.Empty;
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

    /// Internal implementation detail.
    public sealed class MVarFill<T> : Job<Unit> {
      private MVar<T> tM;
      private T t;

      /// Internal implementation detail.
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
