// Copyright (C) by Housemarque, Inc.

namespace Hopac.Core {
  using System.Runtime.CompilerServices;

  internal abstract class WorkWithReady<T> : Work {
    internal T Value;
    internal Scheduler Scheduler;

    internal WorkWithReady(Scheduler sr) {
      this.Scheduler = sr;
    }

    internal void Ready(T value) {
      this.Value = value;
      Worker.RunOnThisThread(this.Scheduler, this);
    }
  }
}
