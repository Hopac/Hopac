// Copyright (C) by Housemarque, Inc.

namespace Hopac.Extra

open Hopac

/// Write once map or dictionary of key-value pairs.
///
/// This data structure is designed for situations in which one or more writer
/// jobs fill a map that one or more reader jobs query.  When a job queries a
/// key-value pair, the job is suspended, unless the map already holds the
/// key-value pair, and resumed once the map is filled with the queried
/// key-value pair.  Once the writers have finished filling the map, the map can
/// be explicitly closed, which means that future and pending queries will be
/// completed immediately.
///
/// The goal is to allow fine grained concurrency between the writers and the
/// readers.  In particular, readers do not need to wait until the map is
/// complete, but can instead proceed as soon as their queries can be answered.
/// On the other hand, it should be straightforward to make sure that the end
/// result is consistent, because filling a write once map is a monotonic
/// process: key-value pairs can be added, but not redefined or removed; map can
/// be closed, but not reopened.
type IMap<'k, 'v> when 'k: equality

/// Operations on write once maps.
module IMap =
  /// Immediate operations on write once maps.
  module Now =
    /// Creates a new write once map.
    val create: unit -> IMap<'k, 'v>

  /// Returns a job that creates a new write once map.
  val create: unit -> Job<IMap<'k, 'v>>

  /// Returns a job that closes the map and answers pending queries negatively.
  val close: IMap<'k, 'v> -> Job<unit>

  /// Returns a job that adds the given key-value pair to the write once map.
  /// If the map already has pending queries for the key-value pair, those
  /// queries will be answered positively.
  ///
  /// It is considered an error if the map already holds a key-value pair with
  /// the same key.  It is also considered an error if the map has been closed.
  val fill: IMap<'k, 'v> -> 'k -> 'v -> Job<unit>

  /// Returns an alternative that tries to read the value associated with the
  /// given key.  The alternative becomes enabled once there is some value
  /// associated with the key in the map or after the map has been closed, in
  /// which case the result will be none.
  val query: IMap<'k, 'v> -> 'k -> Alt<option<'v>>
