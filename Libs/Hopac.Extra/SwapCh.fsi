// Copyright (C) by Housemarque, Inc.

namespace Hopac.Extra

open Hopac

/// Synchronous swap channel: when two jobs communicate on a swap channel, each
/// sends a value and receives a value.
type SwapCh<'a>

/// Operations on swap channels.
module SwapCh =
  /// Immediate or non-workflow operations on swap channels.
  module Now =
    /// Creates a new swap channel.
    val create: unit -> SwapCh<'a>

  /// Creates a new swap channel.
  val create: unit -> Job<SwapCh<'a>>

  /// Alternative to swap values with another job.
  val swap: SwapCh<'a> -> 'a -> Alt<'a>
