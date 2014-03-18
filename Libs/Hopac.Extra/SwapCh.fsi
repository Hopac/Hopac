// Copyright (C) by Housemarque, Inc.

namespace Hopac.Extra

open Hopac

[<AutoOpen>]
module SwapChTypes =
  /// Synchronous swap channel: when two jobs communicate on a swap channel,
  /// each sends a value and receives a value.
  type SwapCh<'a>

/// Operations on swap channeles.
module SwapCh =
  module Now =
    /// Creates a new swap channel.
    val create: unit -> SwapCh<'a>

  /// Creates a new swap channel.
  val create: unit -> Job<SwapCh<'a>>

  /// Swap values with another job.
  val swap: SwapCh<'a> -> 'a -> Alt<'a>
