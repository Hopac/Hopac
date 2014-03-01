// Copyright (C) by Housemarque, Inc.

namespace Hopac {
  using Hopac.Core;
  using System;
  using System.Runtime.CompilerServices;
  using System.Threading;

  /// <summary>A lazy promise or eager future (depending on
  /// construction).</summary>
  public class Promise<T> : Alt<T> {
    internal T Value;
    internal volatile int State;
    internal Cont<T> Readers;

    internal const int Failed = -2;
    internal const int Completed = -1;
    internal const int Running = 0;
    internal const int Delayed = 1;
    internal const int Locked = 2;

    [MethodImpl(AggressiveInlining.Flag)]
    internal Promise(ref Worker wr, Job<T> tJ) {
      Worker.PushNew(ref wr, new Cont(tJ, this));
    }

    /// <summary>Creates a promise whose value is computed lazily with the
    /// given job when an attempt is made to read the promise.</summary>
    [MethodImpl(AggressiveInlining.Flag)]
    public Promise(Job<T> tJ) {
      this.State = Delayed;
      this.Readers = new Cont(tJ, this);
    }

    /// <summary>Creates a promise with the given value.</summary>
    [MethodImpl(AggressiveInlining.Flag)]
    public Promise(T value) {
      this.State = Completed;
      this.Value = value;
    }

    /// <summary>Creates a promise with the given failure exception.</summary>
    [MethodImpl(AggressiveInlining.Flag)]
    public Promise(Exception e) {
      this.State = Failed;
      this.Readers = new Fail(e); // We assume failures are infrequent.
    }

    /// Internal implementation detail.
    internal override void DoJob(ref Worker wr, Cont<T> aK) {
    Spin:
      var state = this.State;
      if (state < Running) goto Completed;
      if (state == Locked) goto Spin;
      if (state != Interlocked.CompareExchange(ref this.State, Locked, state)) goto Spin;

      if (state == Running) goto Running;

      var job = this.Readers as Cont;

      var taker = new Taker<T>();
      taker.Cont = aK;
      taker.Next = taker;
      this.Readers = taker;

      this.State = Running;

      job.DoWork(ref wr);
      return;

    Running:
      WaitQueue.AddTaker(ref this.Readers, aK);
      this.State = Running;
      return;

    Completed:
      if (state == Completed)
        aK.DoCont(ref wr, this.Value);
      else
        aK.DoHandle(ref wr, (this.Readers as Fail).exn);
    }

    /// Internal implementation detail.
    internal override void TryAlt(ref Worker wr, int i, Pick pkSelf, Cont<T> aK, Else<T> aE) {
    Spin:
      var state = this.State;
      if (state < Running) goto Completed;
      if (state == Locked) goto Spin;
      if (state != Interlocked.CompareExchange(ref this.State, Locked, state)) goto Spin;

      if (state == Running) goto Running;

      var job = this.Readers as Cont;

      var taker = new Taker<T>();
      taker.Me = i;
      taker.Pick = pkSelf;
      taker.Cont = aK;
      taker.Next = taker;
      this.Readers = taker;

      this.State = Running;

      Worker.PushNew(ref wr, job);
      aE.TryElse(ref wr, i + 1, pkSelf, aK);
      return;

    Running:
      WaitQueue.AddTaker(ref this.Readers, i, pkSelf, aK);
      this.State = Running;
      aE.TryElse(ref wr, i + 1, pkSelf, aK);
      return;

    Completed:
      var stSelf = Pick.TryPick(pkSelf);
      if (stSelf > 0) goto AlreadyPicked;
      if (stSelf < 0) goto Completed;

      Pick.SetNacks(ref wr, i, pkSelf);

      if (state == Completed)
        aK.DoCont(ref wr, this.Value);
      else
        aK.DoHandle(ref wr, (this.Readers as Fail).exn);
    AlreadyPicked:
      return;
    }

    private sealed class Fail : Cont<T> {
      internal Exception exn;

      internal Fail(Exception exn) {
        this.exn = exn;
      }

      internal override void DoHandle(ref Worker wr, Exception e) {
        throw new NotImplementedException();
      }

      internal override void DoCont(ref Worker wr, T value) {
        throw new NotImplementedException();
      }

      internal override void DoWork(ref Worker wr) {
        throw new NotImplementedException();
      }
    }

    private sealed class Cont : Cont<T> {
      private readonly Promise<T> pr;
      private Job<T> tJ;

      [MethodImpl(AggressiveInlining.Flag)]
      internal Cont(Job<T> tJ, Promise<T> pr) {
        this.pr = pr;
        this.tJ = tJ;
      }

      internal override void DoWork(ref Worker wr) {
        var tJ = this.tJ;
        if (null == tJ) {
          DoCont(ref wr, this.Value);
        } else {
          this.tJ = null;
          tJ.DoJob(ref wr, this);
        }
      }

      internal override void DoHandle(ref Worker wr, Exception e) {
        var pr = this.pr;
      Spin:
        var state = pr.State;
        if (state == Locked) goto Spin;
        if (Running != Interlocked.CompareExchange(ref pr.State, Locked, Running)) goto Spin;

        var readers = pr.Readers;
        pr.Readers = new Fail(e);
        pr.State = Failed;

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

      internal override void DoCont(ref Worker wr, T v) {
        var pr = this.pr;
        pr.Value = v;
      Spin:
        var state = pr.State;
        if (state == Locked) goto Spin;
        if (Running != Interlocked.CompareExchange(ref pr.State, Completed, Running)) goto Spin;

        var readers = pr.Readers;
        if (null == readers)
          return;
        pr.Readers = null;
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
        reader.Value = v;
        Worker.Push(ref wr, reader);

      TryNextReader:
        if (cursor != readers) goto TryReader;
      }
    }
  }
}