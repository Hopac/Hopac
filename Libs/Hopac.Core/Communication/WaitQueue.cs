// Copyright (C) by Housemarque, Inc.

namespace Hopac.Core {
  using Microsoft.FSharp.Core;
  using System;
  using System.Threading;
  using System.Runtime.CompilerServices;

  internal class Send<T> {
    internal Send<T> Next;
    internal T Value;
  }

  internal sealed class Giver<T> : Send<T> {
    internal int Me;
    internal Pick Pick;
    internal Cont<Unit> Cont;
  }

  internal sealed class Taker<T> : Cont<T> {
    internal Cont<T> Cont;
    internal Pick Pick;
    internal int Me;

    internal override Pick GetPick(ref int me) {
      me = Me;
      return Pick;
    }

    internal override Proc GetProc(ref Worker wr) {
      return this.Cont.GetProc(ref wr);
    }

    internal override void DoHandle(ref Worker wr, Exception e) {
      this.Cont.DoHandle(ref wr, e);
    }

    internal override void DoWork(ref Worker wr) {
      this.Cont.DoCont(ref wr, this.Value);
    }

    internal override void DoCont(ref Worker wr, T value) {
      this.Cont.DoCont(ref wr, value);
    }
  }

  internal static class WaitQueue {
    [MethodImpl(AggressiveInlining.Flag)]
    internal static void RemoveRange(ref Work queue, Work last) {
      var tail = queue;
      if (tail == last)
        queue = null;
      else
        tail.Next = last.Next;
    }

    [MethodImpl(AggressiveInlining.Flag)]
    internal static void RemoveRange<T>(ref Send<T> queue, Send<T> last) {
      var tail = queue;
      if (tail == last)
        queue = null;
      else
        tail.Next = last.Next;
    }

    [MethodImpl(AggressiveInlining.Flag)]
    internal static void RemoveRange<T>(ref Cont<T> queue, Work last) {
      var tail = queue;
      if (tail == last)
        queue = null;
      else
        tail.Next = last.Next;
    }

    [MethodImpl(AggressiveInlining.Flag)]
    internal static void RemoveRangeInclusive<T>(Send<T> tail, Send<T> next) {
      tail.Next = next;
    }

    [MethodImpl(AggressiveInlining.Flag)]
    internal static void RemoveRangeInclusive(Work tail, Work next) {
      tail.Next = next;
    }

    [MethodImpl(AggressiveInlining.Flag)]
    internal static void ReplaceRange<T>(ref Send<T> queue, Send<T> last, Send<T> cache) {
      var tail = queue;
      if (tail == last) {
        queue = cache;
      } else {
        ReplaceRangeInclusive(tail, last.Next, cache);
      }
    }

    [MethodImpl(AggressiveInlining.Flag)]
    internal static void ReplaceRange<T>(ref Cont<T> queue, Cont<T> last, Cont<T> cache) {
      var tail = queue;
      if (tail == last) {
        queue = cache;
      } else {
        ReplaceRangeInclusive(tail, last.Next, cache);
      }
    }

    [MethodImpl(AggressiveInlining.Flag)]
    internal static void ReplaceRangeInclusive<T>(Send<T> tail, Send<T> next, Send<T> cache) {
      if (null == cache) {
        tail.Next = next;
      } else {
        tail.Next = cache.Next;
        cache.Next = next;
      }
    }

    [MethodImpl(AggressiveInlining.Flag)]
    internal static void ReplaceRangeInclusive(Work tail, Work next, Work cache) {
      if (null == cache) {
        tail.Next = next;
      } else {
        tail.Next = cache.Next;
        cache.Next = next;
      }
    }

    [MethodImpl(AggressiveInlining.Flag)]
    internal static void Enqueue<T>(ref Send<T> queue, Send<T> elem) {
      var tail = queue;
      if (null == tail) {
        elem.Next = elem;
        queue = elem;
      } else {
        elem.Next = tail.Next;
        tail.Next = elem;
      }
    }

    [MethodImpl(AggressiveInlining.Flag)]
    internal static void Enqueue<T>(ref Cont<T> queue, Cont<T> elem) {
      var tail = queue;
      if (null == tail) {
        elem.Next = elem;
        queue = elem;
      } else {
        elem.Next = tail.Next;
        tail.Next = elem;
      }
    }

    /// <summary>Note that this specifically tries to reuse a giver from the
    /// queue.  This reduces the chance of space leaks.</summary>
    [MethodImpl(AggressiveInlining.Flag)]
    internal static void AddGiver<T>(ref Send<T> queue, T x, int me, Pick pk, Cont<Unit> uK) {
      var last = queue;
      Giver<T> elem;
      if (null != last)
        goto MaybeReuse;
      elem = new Giver<T>();
      queue = elem;
      elem.Next = elem;
      goto Init;

    MaybeReuse:
      elem = last as Giver<T>;
      if (null == elem)
        goto New;
      var pkGiver = elem.Pick;
      if (null != pkGiver && pkGiver.State > 0)
        goto Init;
    New:
      elem = new Giver<T>();
      queue = elem;
      elem.Next = last.Next;
      last.Next = elem;

    Init:
      elem.Value = x;
      elem.Me = me;
      elem.Pick = pk;
      elem.Cont = uK;
    }

    /// <summary>Note that this specifically tries to reuse a giver from the
    /// queue.  This reduces the chance of space leaks.</summary>
    [MethodImpl(AggressiveInlining.Flag)]
    internal static void AddGiver<T>(ref Send<T> queue, T x, Cont<Unit> uK) {
      var last = queue;
      Giver<T> elem;
      if (null != last)
        goto MaybeReuse;
      elem = new Giver<T>();
      queue = elem;
      elem.Next = elem;
      elem.Value = x;
      elem.Cont = uK;
      return;

    MaybeReuse:
      elem = last as Giver<T>;
      if (null == elem)
        goto New;
      var pkGiver = elem.Pick;
      if (null != pkGiver && pkGiver.State > 0)
        goto Reuse;
    New:
      elem = new Giver<T>();
      queue = elem;
      elem.Next = last.Next;
      last.Next = elem;
      elem.Value = x;
      elem.Cont = uK;
      return;

    Reuse:
      elem.Pick = null;
      elem.Value = x;
      elem.Cont = uK;
    }

    [MethodImpl(AggressiveInlining.Flag)]
    internal static void AddSend<T>(ref Send<T> queue, T x) {
      var elem = new Send<T>();
      elem.Value = x;
      var last = queue;
      queue = elem;
      if (null == last) {
        elem.Next = elem;
      } else {
        elem.Next = last.Next;
        last.Next = elem;
      }
    }

    /// <summary>Note that this specifically tries to reuse a taker from the
    /// queue.  This reduces the chance of space leaks.</summary>
    [MethodImpl(AggressiveInlining.Flag)]
    internal static void AddTaker<T>(ref Cont<T> queue, int me, Pick pk, Cont<T> xK) {
      var last = queue;
      Taker<T> elem;
      if (null != last)
        goto MaybeReuse;
      elem = new Taker<T>();
      queue = elem;
      elem.Next = elem;
      goto Init;

    MaybeReuse:
      elem = last as Taker<T>;
      if (null == elem)
        goto New;
      var pkTaker = elem.Pick;
      if (/*null != pkTaker &&*/ pkTaker.State > 0)
        goto Init;
    New:
      elem = new Taker<T>();
      queue = elem;
      elem.Next = last.Next;
      last.Next = elem;

    Init:
      elem.Me = me;
      elem.Pick = pk;
      elem.Cont = xK;
    }

    [MethodImpl(AggressiveInlining.Flag)]
    internal static void AddTaker<T>(ref Cont<T> queue, Cont<T> xK) {
      var last = queue;
      queue = xK;
      if (null == last) {
        xK.Next = xK;
      } else {
        xK.Next = last.Next;
        last.Next = xK;
      }
    }

    [MethodImpl(AggressiveInlining.Flag)]
    internal static void PickReaders<T>(ref Cont<T> readersVar, T value, ref Worker wr) {
      var readers = readersVar;
      if (null == readers) return;
      readersVar = null;
      int me = 0;
      Work cursor = readers;
    TryReader:
      var reader = cursor as Cont<T>;
      cursor = cursor.Next;
      var pk = reader.GetPick(ref me);
      if (null == pk)
        goto GotReader;

    TryPick:
      var st = Pick.TryPick(pk);
      if (st > 0) goto TryNextReader;
      if (st < 0) goto TryPick;

      Pick.SetNacks(ref wr, me, pk);
    GotReader:
      reader.Value = value;
      Worker.Push(ref wr, reader);

    TryNextReader:
      if (cursor != readers)
        goto TryReader;
    }

    internal static void FailReaders<T>(Cont<T> readers, Exception e, ref Worker wr) {
      if (null == readers)
        return;
      Work cursor = readers;
    TryReader:
      var reader = cursor as Cont<T>;
      cursor = cursor.Next;
      int me = 0;
      var pk = reader.GetPick(ref me);
      if (null == pk) goto GotReader;

    TryPick:
      var st = Pick.TryPick(pk);
      if (st > 0) goto TryNextReader;
      if (st < 0) goto TryPick;

      Pick.SetNacks(ref wr, me, pk);

    GotReader:
      Worker.PushNew(ref wr, new FailWork(e, reader));

    TryNextReader:
      if (cursor != readers) goto TryReader;
    }
  }
}
