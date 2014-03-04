// Copyright (C) by Housemarque, Inc.

namespace Hopac {
  using Microsoft.FSharp.Core;
  using Hopac.Core;
  using System.Runtime.CompilerServices;
  using System;

  /// <summary>A synchronous channel.</summary>
  public class Ch<T> : Alt<T> {
    internal SpinlockTTAS Lock;
    internal Send<T> Givers;
    internal Cont<T> Takers;
    // Note that via selective communication it is possible for a job to offer
    // to both give and take on the same channel simultaneously.  So, both
    // Givers and Takers queues must be maintained even though in many cases
    // only one of them is non empty.

    /// Internal implementation detail.
    internal override void DoJob(ref Worker wr, Cont<T> xK) {
    TryNextGiver:
      this.Lock.Enter();
      var tail = this.Givers;
      if (null != tail) goto TryGiver;
      WaitQueue.AddTaker(ref this.Takers, xK);
      this.Lock.Exit();
      return;

    TryGiver:
      var cursor = tail.Next;
      if (tail == cursor)
        this.Givers = null;
      else
        tail.Next = cursor.Next;
      this.Lock.Exit();

      var giver = cursor as Giver<T>;
      if (null == giver)
        goto GotSend;

      var pkOther = giver.Pick;
      if (null == pkOther) goto GotGiver;

    TryPickOther:
      var stOther = Pick.TryPick(pkOther);
      if (stOther > 0) goto TryNextGiver;
      if (stOther < 0) goto TryPickOther;

      Pick.SetNacks(ref wr, giver.Me, pkOther);
    GotGiver:
      Worker.Push(ref wr, giver.Cont);
    GotSend:
      Cont.Do(xK, ref wr, cursor.Value);
      return;
    }

    /// Internal implementation detail.
    internal override void
      TryAlt(ref Worker wr, int i, Pick pkSelf, Cont<T> xK, Else<T> xE) {
      this.Lock.Enter();
      var tail = this.Givers;
      if (null == tail) goto TryTaker;
      Send<T> cache = null;
      var cursor = tail.Next;

    TryGiver:
      var giver = cursor as Giver<T>;
      Pick pkOther = null;
      if (null != giver)
        goto Giver;

    TryPick:
      var st = Pick.TryPick(pkSelf);
      if (st > 0) goto AlreadyPicked;
      if (st < 0) goto TryPick;

      WaitQueue.ReplaceRange(ref this.Givers, cursor, cache);
      this.Lock.Exit();

      Pick.SetNacks(ref wr, i, pkSelf);
      Cont.Do(xK, ref wr, cursor.Value);
      return;

    AlreadyPicked:
      WaitQueue.ReplaceRangeInclusive(this.Givers, cursor, cache);
      this.Lock.Exit();
      return;

    Giver:
      cursor = cursor.Next;
      pkOther = giver.Pick;

      if (null == pkOther) goto TryPickSelf;
      if (pkOther == pkSelf) goto OtherIsSelf;

    TryPickOther:
      var stOther = Pick.TryClaim(pkOther);
      if (stOther > 0) goto TryNextGiver;
      if (stOther < 0) goto TryPickOther;

    TryPickSelf:
      var stSelf = Pick.TryPick(pkSelf);
      if (stSelf > 0) goto SelfAlreadyPicked;
      if (stSelf < 0) goto BackOff;

      //GotGiver:
      WaitQueue.ReplaceRange(ref this.Givers, giver, cache);
      this.Lock.Exit();
      if (null != pkOther) {
        Pick.PickClaimed(pkOther);
        Pick.SetNacks(ref wr, giver.Me, pkOther);
      }
      Pick.SetNacks(ref wr, i, pkSelf);
      Worker.Push(ref wr, giver.Cont);
      Cont.Do(xK, ref wr, giver.Value);
      return;

    BackOff:
      if (null == pkOther) goto TryPickSelf;
      Pick.Unclaim(pkOther);
      goto TryPickOther;

    OtherIsSelf:
      WaitQueue.Enqueue(ref cache, giver);
      if (giver != tail) goto TryGiver;

      this.Givers = cache;
      goto TryTaker;

    TryNextGiver:
      if (giver != tail) goto TryGiver;

      this.Givers = cache;
    TryTaker:
      WaitQueue.AddTaker(ref this.Takers, i, pkSelf, xK);
      this.Lock.Exit();
      xE.TryElse(ref wr, i + 1, pkSelf, xK);
      return;

    SelfAlreadyPicked:
      if (null != pkOther) Pick.Unclaim(pkOther);

      WaitQueue.ReplaceRangeInclusive(this.Givers, giver, cache);
      this.Lock.Exit();
      return;
    }
  }

  namespace Core {
    /// Internal implementation detail.
    public class ChGive<T> : Alt<Unit> {
      private Ch<T> Ch;
      private T X;

      /// Internal implementation detail.
      [MethodImpl(AggressiveInlining.Flag)]
      public ChGive(Ch<T> ch, T x) {
        this.Ch = ch;
        this.X = x;
      }

      /// Internal implementation detail.
      internal override void DoJob(ref Worker wr, Cont<Unit> uK) {
        var ch = this.Ch;
      TryNextTaker:
        ch.Lock.Enter();
        var tail = ch.Takers;
        if (null == tail)
          goto TryGiver;
        var cursor = tail.Next;
        if (tail == cursor) {
          ch.Takers = null;
          ch.Lock.Exit();
        } else {
          tail.Next = cursor.Next;
          ch.Lock.Exit();
          tail = cursor as Cont<T>;
        }

        var taker = tail as Taker<T>;
        if (null == taker)
          goto GotTaker;
        var pkOther = taker.Pick;

      TryPickOther:
        var stOther = Pick.TryPick(pkOther);
        if (stOther > 0) goto TryNextTaker;
        if (stOther < 0) goto TryPickOther;

        Pick.SetNacks(ref wr, taker.Me, pkOther);

        tail = taker.Cont;
      GotTaker:
        tail.Value = this.X;
        Worker.Push(ref wr, tail);
        Cont.Do(uK, ref wr, null);
        return;

      TryGiver:
        WaitQueue.AddGiver(ref ch.Givers, this.X, uK);
        ch.Lock.Exit();
        return;
      }

      /// Internal implementation detail.
      internal override void
        TryAlt(ref Worker wr, int i, Pick pkSelf, Cont<Unit> uK, Else<Unit> uE) {
        var ch = this.Ch;
        ch.Lock.Enter();
        var tail = ch.Takers;
        if (null == tail) goto TryGiver;
        Cont<T> cache = null;
        var cursor = tail.Next;

      TryTaker:
        var taker = cursor as Cont<T>;
        cursor = cursor.Next;

        int me = 0;
        var pkOther = taker.GetPick(ref me);
        if (null == pkOther) goto TryPickSelf;
        if (pkOther == pkSelf) goto OtherIsSelf;

      TryPickOther:
        var stOther = Pick.TryClaim(pkOther);
        if (stOther > 0) goto TryNextTaker;
        if (stOther < 0) goto TryPickOther;

      TryPickSelf:
        var stSelf = Pick.TryPick(pkSelf);
        if (stSelf > 0) goto SelfAlreadyPicked;
        if (stSelf < 0) goto BackOff;

        WaitQueue.ReplaceRange(ref ch.Takers, taker, cache);
        ch.Lock.Exit();
        if (null != pkOther) {
          Pick.PickClaimed(pkOther);
          Pick.SetNacks(ref wr, me, pkOther);
        }
        Pick.SetNacks(ref wr, i, pkSelf);
        taker.Value = this.X;
        Worker.Push(ref wr, taker);
        Cont.Do(uK, ref wr, null);
        return;

      BackOff:
        if (null == pkOther) goto TryPickSelf;
        Pick.Unclaim(pkOther);
        goto TryPickOther;

      OtherIsSelf:
        WaitQueue.Enqueue(ref cache, taker);
        if (taker != tail) goto TryTaker;

        ch.Takers = cache;
        goto TryGiver;

      TryNextTaker:
        if (taker != tail) goto TryTaker;

        ch.Takers = cache;
      TryGiver:
        WaitQueue.AddGiver(ref ch.Givers, this.X, i, pkSelf, uK);
        ch.Lock.Exit();
        uE.TryElse(ref wr, i + 1, pkSelf, uK);
        return;

      SelfAlreadyPicked:
        if (null != pkOther) Pick.Unclaim(pkOther);

        WaitQueue.ReplaceRangeInclusive(ch.Takers, taker, cache);
        ch.Lock.Exit();
        return;
      }
    }

    /// Internal implementation detail.
    public class ChSend<T> : Job<Unit> {
      private Ch<T> Ch;
      private T X;

      /// Internal implementation detail.
      [MethodImpl(AggressiveInlining.Flag)]
      public ChSend(Ch<T> ch, T x) {
        this.Ch = ch;
        this.X = x;
      }

      /// Internal implementation detail.
      internal override void DoJob(ref Worker wr, Cont<Unit> uK) {
        var ch = this.Ch;
      TryNextTaker:
        ch.Lock.Enter();
        var tail = ch.Takers;
        if (null == tail)
          goto TryGiver;
        var cursor = tail.Next;
        if (tail == cursor) {
          ch.Takers = null;
          ch.Lock.Exit();
        } else {
          tail.Next = cursor.Next;
          ch.Lock.Exit();
          tail = cursor as Cont<T>;
        }

        var taker = tail as Taker<T>;
        if (null == taker)
          goto GotTaker;
        var pkOther = taker.Pick;

      TryPickOther:
        var stOther = Pick.TryPick(pkOther);
        if (stOther > 0) goto TryNextTaker;
        if (stOther < 0) goto TryPickOther;

        Pick.SetNacks(ref wr, taker.Me, pkOther);

        tail = taker.Cont;
      GotTaker:
        tail.Value = this.X;
        Worker.Push(ref wr, tail);
        Cont.Do(uK, ref wr, null);
        return;

      TryGiver:
        WaitQueue.AddSend(ref ch.Givers, this.X);
        ch.Lock.Exit();
        Cont.Do(uK, ref wr, null);
        return;
      }
    }
  }
}
