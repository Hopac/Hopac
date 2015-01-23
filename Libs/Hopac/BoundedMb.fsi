// Copyright (C) by Vesa Karvonen

namespace Hopac

/// Represents a bounded synchronous mailbox for many to many communication.
type BoundedMb<'x>

/// Operations on bounded synchronous mailboxes.
module BoundedMb =
  /// Creates a new bounded mailbox with a buffer of the specified maximum
  /// capacity.  Note that a bounded mailbox with a capacity of `0` behaves
  /// exactly the same as a channel.  See also: `Ch<_>`.
  val create: capacity: int -> Job<BoundedMb<'x>>

  /// Selective synchronous operation to put a message to a bounded mailbox.
  /// `put` operations are processed in FIFO order and become enabled as soon as
  /// there is room in the bounded buffer.  If the buffer capacity is `0`, `put`
  /// behaves exactly like `Ch.give`.
  val put: BoundedMb<'x> -> 'x -> Alt<unit>

  /// Selective synchronous operation to take a message from a bounded mailbox.
  /// `take` operations are processed in FIFO order and become enabled as soon
  /// as there are messages in the bounded buffer.  If the buffer capacity is
  /// `0`, `take` behaves exactly like `Ch.take`.
  val take: BoundedMb<'x> -> Alt<'x>
