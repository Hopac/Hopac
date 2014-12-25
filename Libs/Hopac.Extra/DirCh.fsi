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

  /// An alternative to take a value to be given on the associated output
  /// channel.
  val take: InCh<'a> -> Alt<'a>

  /// An alternative to give a value to be taken on the associated input
  /// channel.
  val give: OutCh<'a> -> 'a -> Alt<unit>
