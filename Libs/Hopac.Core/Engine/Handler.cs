// Copyright (C) by Housemarque, Inc.

namespace Hopac.Core {
  using Microsoft.FSharp.Core;
  using System;
  using System.Threading;
  using System.Runtime.CompilerServices;

  /// <summary>Exception handling continuation.</summary>
  internal abstract class Handler {
    /// <summary>Never call this method directly, because handlers can be
    /// null in server jobs!  Use the static DoHandle method instead.</summary>
    internal abstract void DoHandle(ref Worker wr, Exception e);

    internal static FSharpFunc<Exception, Job<Unit>> TopLevelHandler;

    internal static void DoHandle(Handler h, ref Worker wr, Exception e) {
      if (null == h) {
        var tlh = TopLevelHandler;
        if (null == tlh) {
          Console.WriteLine("Unhandled exception: {0}", e);
        } else {
          var uK = new Cont();
          wr.Handler = uK;
          tlh.Invoke(e).DoJob(ref wr, uK);
        }
      } else {
        h.DoHandle(ref wr, e);
      }
    }

    private sealed class Cont : Cont<Unit> {
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
