// Copyright (C) by Housemarque, Inc.

namespace Hopac.Extra

open Hopac

/// Write once map.
///
/// This data structure is designed for situations in which a concurrent job
/// fills a map while other concurrent jobs are running that need to refer to
/// items in the map.  When a job tries to read an item from the map, the job is
/// suspended, unless the map already holds some value for the item, and resumed
/// once the item is added to the map.  This approach allows the job filling the
/// map and the jobs reading the map to proceed concurrently.
///
/// Once the concurrent job has finished filling the map, it can close the map,
/// which means that future read operations and pending read operations of
/// non-existing items from the map are resumed.  This way clients can proceed
/// knowing that the item will never appear in the future.
type IMap<'k, 'v> when 'k: equality

/// Operations on write once maps.
module IMap =
  /// Immediate operations on write once maps.
  module Now =
    /// Creates a new write once map.
    val create: unit -> IMap<'k, 'v>

  /// Creates a new write once map.
  val create: unit -> Job<IMap<'k, 'v>>

  /// Closes the map and flushes pending read operations by committing them to
  /// none.
  val close: IMap<'k, 'v> -> Job<unit>

  /// Returns a job that adds the given key value pair to the write once map.
  ///
  /// It is considered an error if the map already holds a value associated with
  /// the key.  If the map already has read operations waiting for the value
  /// associated with the key, those operation are enabled.
  ///
  /// It is also considered an error if them map has been previously closed.
  val fill: IMap<'k, 'v> -> 'k -> 'v -> Job<unit>

  /// Selective operations on write once maps.
  module Alt =
    /// Returns an alternative that tries to reads the value associated with the
    /// given key.  The alternative becomes enabled once there is some value
    /// associated with the key in the map or after the map has been closed, in
    /// which case the result will be none.
    val read: IMap<'k, 'v> -> 'k -> Alt<option<'v>>
