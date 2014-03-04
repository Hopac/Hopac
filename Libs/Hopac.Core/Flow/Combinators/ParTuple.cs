// Copyright (C) by Housemarque, Inc.

namespace Hopac.Core {
  using System;
  using System.Runtime.CompilerServices;
  using System.Threading;

  /// Internal implementation detail.
  internal sealed class ParTuple<A, B> : Cont<B> {
    internal readonly Cont<Tuple<A, B>> abK;
    internal A a;
    internal volatile int state;
    internal B b;
    internal volatile Exception e;

    /// Internal implementation detail.
    [MethodImpl(AggressiveInlining.Flag)]
    internal ParTuple(Cont<Tuple<A, B>> abK) {
      this.abK = abK;
    }

    /// Internal implementation detail.
    internal override void DoHandle(ref Worker wr, Exception e) {
      if (null != Interlocked.CompareExchange(ref this.e, e, null))
        goto DoHandle;

      if (0 != Interlocked.CompareExchange(ref this.state, -1, 0))
        goto DoHandle;
      return;

    DoHandle:
      var abK = this.abK;
      wr.Handler = abK;
      if (null != this.e)
        e = new AggregateException(new Exception[]{this.e, e});
      abK.DoHandle(ref wr, e);
    }

    /// Internal implementation detail.
    internal override void DoContAbs(ref Worker wr, B b) {
    Interpret:
      var state = this.state;
      if (state > 0) goto DoCont;
      if (state < 0) goto DoHandle;
      this.b = b;
      if (0 != Interlocked.CompareExchange(ref this.state, 1, 0)) goto Interpret;
      return;

    DoHandle: {
        var abK = this.abK;
        wr.Handler = abK;
        abK.DoHandle(ref wr, this.e);
        return;
      }

    DoCont: {
        var abK = this.abK;
        wr.Handler = abK;
        abK.DoContAbs(ref wr, new Tuple<A, B>(this.a, b));
      }
    }

    /// Internal implementation detail.
    [MethodImpl(AggressiveInlining.Flag)]
    internal void DoOtherCont(ref Worker wr, A a) {
    Interpret:
      var state = this.state;
      if (state > 0) goto DoCont;
      if (state < 0) goto DoHandle;
      this.a = a;
      if (0 != Interlocked.CompareExchange(ref this.state, 1, 0)) goto Interpret;
      return;

    DoHandle: {
        var abK = this.abK;
        wr.Handler = abK;
        abK.DoHandle(ref wr, this.e);
        return;
      }

    DoCont: {
        var abK = this.abK;
        wr.Handler = abK;
        abK.DoContAbs(ref wr, new Tuple<A, B>(a, this.b));
      }
    }
  }
}