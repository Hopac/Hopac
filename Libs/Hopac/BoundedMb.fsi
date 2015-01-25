// Copyright (C) by Vesa Karvonen

namespace Hopac

/// Represents a bounded synchronous mailbox for many to many communication.
#if DOC
///
/// Bounded synchronous mailboxes are a useful tool for coordinating work among
/// co-operating processes.  They provide slack in the form of buffering between
/// producers and consumers allowing them to proceed in parallel.  They also
/// provide back-pressure in the form of blocking producers when consumers
/// cannot keep up.
///
/// In cases where buffering is not necessary, the basic channel primitive,
/// `Ch<_>`, should be preferred.  In cases where unbounded buffering is not a
/// problem, the basic mailbox primitive, `Mailbox<_>`, should be preferred.
///
/// At the time of writing, `BoundedMb<_>` is not implemented as a primitive,
/// but is implemented using other primitives of Hopac, and it is likely that
/// performance can be improved significantly.  If you run into a case where the
/// performance of `BoundedMb<_>` becomes problematic, please submit an issue.
#endif
type BoundedMb<'x>

/// Operations on bounded synchronous mailboxes.
module BoundedMb =
  /// Returns a job that creates a new bounded mailbox with a buffer of the
  /// specified maximum capacity.  Note that a bounded mailbox with a capacity
  /// of `0` behaves exactly the same as a channel, `Ch<_>`.
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
