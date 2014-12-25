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

  /// Alternative to put a message to a bounded mailbox.
  val put: BoundedMb<'x> -> 'x -> Alt<unit>

  /// Alternative to take a message from a bounded mailbox.
  val take: BoundedMb<'x> -> Alt<'x>
