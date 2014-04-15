// Copyright (C) by Housemarque, Inc.

namespace Hopac.Core {
  using Microsoft.FSharp.Core;

  /// <summary>This class contains some special static data used by the Hopac
  /// library internal implementation.</summary>
  public static class StaticData {
    /// <summary>Stores the single shared unit alternative.</summary>
    public static Alt<Unit> unit;

    /// <summary>Stores the single shared zero alternative.</summary>
    public static Alt<Unit> zero;

    /// <summary>Stores the single shared scheduler job.</summary>
    public static Job<Scheduler> scheduler;

    /// <summary>Stores the single shared proc job.</summary>
    public static Job<Proc> proc;

    /// <summary>This is normally called automatically by Hopac library
    /// code.</summary>
    public static void Init() {
      if (null == unit) {
        unit = new AlwaysUnit();
        zero = new Zero();
        scheduler = new GetScheduler();
        proc = new GetProc();
      }
    }

    private sealed class GetScheduler : Job<Scheduler> {
      internal override void DoJob(ref Worker wr, Cont<Scheduler> sK) {
        Cont.Do(sK, ref wr, wr.Scheduler);
      }
    }

    private sealed class GetProc : Job<Proc> {
      internal override void DoJob(ref Worker wr, Cont<Proc> pK) {
        Cont.Do(pK, ref wr, pK.GetProc());
      }
    }
  }
}
