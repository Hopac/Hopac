// Copyright (C) by Housemarque, Inc.

namespace Hopac.Extra

open Hopac

/// A directional input channel.
type InCh<'a>

/// A directional output channel.
type OutCh<'a>

/// Operations on directional channels.
module DirCh =
  /// Immediate or non-workflow operations on directional channels.
  module Now =
    /// Creates a new pair of directional channels.  The created channels are
    /// associated so that giving a value on the output channel allows the value
    /// to be taken on the input channel.
    val create: unit -> InCh<'a> * OutCh<'a>

  /// Creates a new pair of directional channels.  The created channels are
  /// associated so that giving a value on the output channel allows the value
  /// to be taken on the input channel.
  val create: unit -> Job<InCh<'a> * OutCh<'a>>

  /// Waits until another job offers to give a value on the associated output
  /// channel and then takes the value.
  val take: InCh<'a> -> Job<'a>

  /// Waits until another job offers to take a value on the associated input
  /// channel and then gives the value.
  val give: OutCh<'a> -> 'a -> Job<unit>

  /// Selective operations on directional channels.
  module Alt =
    /// An alternative to take a value on the input channel.
    val take: InCh<'a> -> Alt<'a>

    /// An alternative to give a value on the input channel.
    val give: OutCh<'a> -> 'a -> Alt<unit>
