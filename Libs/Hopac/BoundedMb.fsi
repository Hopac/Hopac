// Copyright (C) by Vesa Karvonen

namespace Hopac

/// Represents a bounded mailbox for many to many communication.
type BoundedMb<'x>

/// Operations on bounded mailboxes.
module BoundedMb =
  /// Creates a new bounded mailbox with a buffer of the specified maximum
  /// capacity.  Note that a bounded mailbox with a capacity of `0` behaves
  /// exactly the same as a channel.  See also: `Ch<_>`.
  val create: capacity: int -> Job<BoundedMb<'x>>

  /// Alternative to put a message to a bounded mailbox.
  val put: BoundedMb<'x> -> 'x -> Alt<unit>

  /// Alternative to take a message from a bounded mailbox.
  val take: BoundedMb<'x> -> Alt<'x>
