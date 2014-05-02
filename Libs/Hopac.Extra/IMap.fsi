// Copyright (C) by Housemarque, Inc.

namespace Hopac.Extra

open Hopac

/// Write once map.
type IMap<'k, 'v> when 'k: equality

/// Operations on write once maps.
module IMap =
  /// Creates a new write once map.
  val create: unit -> Job<IMap<'k, 'v>>

  /// Returns a job that adds the given key value pair to the write once map.
  /// It is considered an error if the map already holds a value associated
  /// with the key.  If the map already has `read` operations waiting for the
  /// value associated with the key, those operation are enabled.
  val fill: IMap<'k, 'v> -> 'k -> 'v -> Job<unit>

  /// Selective operations on write once maps.
  module Alt =
    /// Returns an alternative that reads the value associated with the given
    /// key.  The alternative becomes enabled once there is a value associated
    /// with the key in the map.
    val read: IMap<'k, 'v> -> 'k -> Alt<'v>
