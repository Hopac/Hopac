// Copyright (C) by Housemarque, Inc.

namespace Hopac.Core {
  using System;
  using System.Reflection;
  using Microsoft.FSharp.Core;

  /// <summary>This class contains some special static data used by the Hopac
  /// library internal implementation.  The static members of this class are
  /// normally implicitly initialized by the Hopac library, but it is possible
  /// to write a program that accesses these variables too early.  In such a
  /// corner case the program should explicitly arrange the `Init` method of
  /// this class to be called.</summary>
  public static class StaticData {
    /// <summary>Whether we are on Mono rather than .Net.</summary>
    public static bool isMono;

    /// <summary>Stores the single shared unit alternative.</summary>
    public static Alt<Unit> unit;

    /// <summary>Stores the single shared zero alternative.</summary>
    public static volatile Promise<Unit> zero;

    /// <summary>Stores the single shared scheduler job.</summary>
    public static Job<Scheduler> scheduler;

    /// <summary>Stores the single shared proc job.</summary>
    public static Job<Proc> proc;

    /// <summary>Stores the single shared proc that switches execution to a
    /// worker thread.</summary>
    public static Job<Unit> switchToWorker;

    /// <summary>Stores the single AsyncCallback delegate.</summary>
    public static AsyncCallback workAsyncCallback;

    /// <summary>Stores an action for printing diagnostics.</summary>
    public static Action<String> writeLine;

    /// <summary>Function for creating a scheduler.</summary>
    public static Func<bool, Job<int>, int, int, FSharpFunc<Exception, Job<Unit>>, Scheduler> createScheduler;

    /// <summary>The global scheduler (if it has been created).</summary>
    public static Scheduler globalScheduler;

    /// <summary>The global timer (if it has been created).</summary>
    internal static Timer globalTimer;

    /// <summary>This is normally called automatically by Hopac library code.
    /// This is safe to be called from multiple threads.</summary>
    public static void Init() {
      if (null == zero) {
        if (null == createScheduler || null == writeLine) {
          Assembly.Load(new AssemblyName("Hopac.Platform"))
          .GetType("Hopac.Platform.Init").GetTypeInfo()
          .GetDeclaredMethod("Do").Invoke(null, null);
        } else {
          isMono = null != Type.GetType ("Mono.Runtime");
          unsafe {
            if (!isMono && sizeof(IntPtr) == 8)
              unit = new AlwaysUnitTC();
            else
              unit = new AlwaysUnitNonTC();
          }

          scheduler = new GetScheduler();
          proc = new GetProc();
          switchToWorker = new SwitchToWorker();
          workAsyncCallback = (iar) => (iar.AsyncState as WorkAsyncCallback).Ready(iar);
          zero = new Never<Unit>(); // Must be written last!
        }
      }
    }

    private sealed class GetScheduler : Job<Scheduler> {
      internal override void DoJob(ref Worker wr, Cont<Scheduler> sK) {
        Cont.Do(sK, ref wr, wr.Scheduler);
      }
    }

    private sealed class GetProc : Job<Proc> {
      internal override void DoJob(ref Worker wr, Cont<Proc> pK) {
        Cont.Do(pK, ref wr, pK.GetProc(ref wr));
      }
    }

    private sealed class SwitchToWorker : Job<Unit> {
      internal override void DoJob(ref Worker wr, Cont<Unit> uK) {
        // See Worker.RunOnThisThread to understand why this works.
        Worker.Push(ref wr, uK);
      }
    }
  }
}
