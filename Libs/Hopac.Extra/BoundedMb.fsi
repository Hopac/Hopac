// Copyright (C) by Vesa Karvonen

namespace Hopac.Extra

open Hopac

/// Represents a bounded mailbox for many to many communication.
type BoundedMb<'x>

/// Operations on bounded mailboxes.
module BoundedMb =
  /// Creates a new bounded mailbox with a buffer of the specified maximum
  /// capacity.
  val create: capacity: int -> Job<BoundedMb<'x>>

  /// Puts a message to the bounded mailbox.  If the buffer of the bounded
  /// mailbox is not full, the operation can be completed immediately.  If the
  /// buffer is full, the operation will block.
  val put: BoundedMb<'x> -> 'x -> Job<unit>

  /// Takes a message from the bounded mailbox.  If the buffer of the bounded
  /// mailbox is not empty, the operation can be completed immediately.  If the
  /// buffer is empty, the operation will block.
  val take: BoundedMb<'x> -> Job<'x>

  /// Selective operations on bounded mailboxes.
  module Alt =
    /// Alternative to put a message to a bounded mailbox.
    val put: BoundedMb<'x> -> 'x -> Alt<unit>

    /// Alternative to take a message from a bounded mailbox.
    val take: BoundedMb<'x> -> Alt<'x>
