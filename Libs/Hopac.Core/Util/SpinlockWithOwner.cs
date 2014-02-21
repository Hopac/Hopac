// Copyright (C) by Housemarque, Inc.

namespace Hopac.Core {
  using System.Diagnostics;
  using System.Runtime.CompilerServices;
  using System.Threading;
  using System;

  internal struct SpinlockWithOwner {
    private volatile Work tail;

    [MethodImpl(AggressiveInlining.Flag)]
    internal Work Init() {
      var owner = new Owner();
      tail = owner;
      return owner;
    }

    [MethodImpl(AggressiveInlining.Flag)]
    internal void ExitAndEnter(Work owner) {
      var pred = tail;
      if (owner != pred) {
        Volatile.Write(ref owner.Next, null);
        while (pred == Volatile.Read(ref pred.Next)) { }
        Enter(owner);
      }
    }

    [MethodImpl(AggressiveInlining.Flag)]
    internal static void Exit(Work owner) {
      Volatile.Write(ref owner.Next, null);
    }

    [MethodImpl(AggressiveInlining.Flag)]
    internal void Enter(Work visitor) {
      visitor.Next = visitor;
      var pred = Interlocked.Exchange(ref tail, visitor);
      while (pred == Volatile.Read(ref pred.Next)) { }
    }

    internal sealed class Owner : Work {
      [MethodImpl(AggressiveInlining.Flag)]
      internal Owner() {
        Next = this;
      }

      internal override void DoHandle(ref Worker wr, Exception e) {
        throw new NotImplementedException();
      }

      internal override void DoWork(ref Worker wr) {
        throw new NotImplementedException();
      }
    }
  }
}
