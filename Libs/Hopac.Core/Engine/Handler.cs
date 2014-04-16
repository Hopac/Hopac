// Copyright (C) by Housemarque, Inc.

namespace Hopac.Core {
  using Microsoft.FSharp.Core;
  using System;
  using System.Threading;
  using System.Runtime.CompilerServices;

  /// <summary>Exception handling continuation.</summary>
  internal abstract class Handler {
    /// <summary>Do not call this directly unless you know that the handler is not null.</summary>
    internal abstract void DoHandle(ref Worker wr, Exception e);

    /// <summary>Do not call this directly unless you know that the handler is not null.</summary>
    internal abstract Proc GetProc();

    internal static Proc GetProc(Handler hr) {
      if (null == hr)
        return null;
      else
        return hr.GetProc();
    }

    internal static void DoHandle(Handler hr, ref Worker wr, Exception e) {
      if (null == hr) {
        var tlh = wr.Scheduler.TopLevelHandler;
        if (null == tlh) {
          Console.WriteLine("Unhandled exception: {0}", e);
        } else {
          var uK = new Cont();
          wr.Handler = uK;
          tlh.Invoke(e).DoJob(ref wr, uK);
        }
      } else {
        hr.DoHandle(ref wr, e);
      }
    }

    private sealed class Cont : Cont<Unit> {
      internal override Proc GetProc() {
        return null;
      }
      internal override void DoHandle(ref Worker wr, Exception e) {
        Console.WriteLine("Top level handler raised: {0}", e);
      }
      internal override void DoWork(ref Worker wr) { }
      internal override void DoCont(ref Worker wr, Unit value) { }
    }
  }

  internal abstract class Handler_State<T> : Handler {
    internal T State;

    [MethodImpl(AggressiveInlining.Flag)]
    internal Handler_State() { }

    [MethodImpl(AggressiveInlining.Flag)]
    internal Handler_State(T state) { this.State = state; }
  }
}
