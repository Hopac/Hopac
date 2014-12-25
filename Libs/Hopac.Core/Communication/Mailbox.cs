// Copyright (C) by Housemarque, Inc.

namespace Hopac {
  using Microsoft.FSharp.Core;
  using Hopac.Core;
  using System.Collections.Generic;
  using System.Runtime.CompilerServices;

  /// <summary>Represents a asynchronous, unbounded buffered mailbox.</summary>
  public class Mailbox<T> : Alt<T> {
    internal SpinlockTTAS Lock;
    internal readonly Queue<T> Values;
    internal Cont<T> Takers;

    /// <summary>Constructs a new empty mailbox.</summary>
    [MethodImpl(AggressiveInlining.Flag)]
    public Mailbox() {
      Values = new Queue<T>();
    }

    internal override void DoJob(ref Worker wr, Cont<T> aK) {
      this.Lock.Enter();

      if (this.Values.Count > 0) goto GotValue;

      WaitQueue.AddTaker(ref this.Takers, aK);
      this.Lock.Exit();
      return;

    GotValue:
      T value = this.Values.Dequeue();
      this.Lock.Exit();
      Cont.Do(aK, ref wr, value);
      return;
    }

    internal override void TryAlt(ref Worker wr, int i, Cont<T> aK, Else aE) {
      var pkSelf = aE.pk;
      this.Lock.Enter();

      if (this.Values.Count > 0) goto GotValue;

      WaitQueue.AddTaker(ref this.Takers, i, pkSelf, aK);
      this.Lock.Exit();
      aE.TryElse(ref wr, i + 1);
      return;

    GotValue:
      var stSelf = Pick.TryPick(pkSelf);
      if (stSelf > 0) goto AlreadyPicked;
      if (stSelf < 0) goto GotValue;

      T value = this.Values.Dequeue();
      this.Lock.Exit();
      Pick.SetNacks(ref wr, i, pkSelf);
      Cont.Do(aK, ref wr, value);
      return;

    AlreadyPicked:
      this.Lock.Exit();
      return;
    }

    [MethodImpl(AggressiveInlining.Flag)]
    internal static void Send(Mailbox<T> mb, ref Worker wr, T x) {
    TryNextTaker:
      mb.Lock.Enter();
      var tail = mb.Takers;
      if (null == tail)
        goto NoTakers;
      var cursor = tail.Next;
      if (tail == cursor) {
        mb.Takers = null;
        mb.Lock.Exit();
      } else {
        tail.Next = cursor.Next;
        mb.Lock.Exit();
        tail = cursor as Cont<T>;
      }

      var taker = cursor as Taker<T>;
      if (null == taker)
        goto GotTaker;
      var pk = taker.Pick;

    TryPick:
      var st = Pick.TryPick(pk);
      if (st > 0) goto TryNextTaker;
      if (st < 0) goto TryPick;

      Pick.SetNacks(ref wr, taker.Me, pk);
    GotTaker:
      tail.Value = x;
      Worker.Push(ref wr, tail);
      return;

    NoTakers:
      mb.Values.Enqueue(x);
      mb.Lock.Exit();
      return;
    }

    [MethodImpl(AggressiveInlining.Flag)]
    internal static void Send(Scheduler sr, Mailbox<T> mb, T x) {
      Worker wr = new Worker();
      wr.Scheduler = sr;
      Send(mb, ref wr, x);
      Scheduler.PushAll(sr, wr.WorkStack);
    }
  }

  namespace Core {
    /// Internal implementation detail.
    public sealed class MailboxSend<T> : Job<Unit> {
      private Mailbox<T> Mb;
      private T X;

      /// Internal implementation detail.
      [MethodImpl(AggressiveInlining.Flag)]
      public MailboxSend(Mailbox<T> mb, T x) {
        this.Mb = mb;
        this.X = x;
      }

      internal override void DoJob(ref Worker wr, Cont<Unit> uK) {
        Mailbox<T>.Send(this.Mb, ref wr, this.X);
        Work.Do(uK, ref wr);
      }
    }
  }
}