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

    /// <summary>This is normally called automatically by Hopac library
    /// code.</summary>
    public static void Init() {
      if (null == unit) {
        unit = new AlwaysUnit();
        zero = new Zero();
      }
    }
  }
}
