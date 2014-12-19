// Copyright (C) by Housemarque, Inc.

namespace Hopac.Core {
  using System.Threading;

  /// <summary>Provides a low overhead, single shot waitable event.</summary>
  internal static class Condition {
    internal static bool warned;

    internal static void Pulse(object o, ref int v) {
      var w = v;
      if (w < 0 || 0 != Interlocked.Exchange(ref v, ~w)) {
        Monitor.Enter(o);
        Monitor.Pulse(o);
        Monitor.Exit(o);
      }
    }

    internal static void Wait(object o, ref int v) {
      if (0 <= v) {
        Monitor.Enter(o);
        var w = v;
        if (0 <= w) {
          if (0 == Interlocked.Exchange(ref v, ~w)) {
            if (!warned && Worker.IsWorkerThread) {
              warned = true;
              StaticData.writeLine(
                "WARNNG: You are making a blocking call from within a Hopac " +
                "worker thread, which means that your program may deadlock.");
            }
            Monitor.Wait(o);
          }
        }
      }
    }
  }
}
