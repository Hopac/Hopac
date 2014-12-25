// Copyright (C) by Housemarque, Inc.

namespace Hopac.Extra

open Hopac

/// Represents a selectable buffered queue.
type SelectableQueue<'a>

/// Implements a selectable buffered queue similar to what is described in the
/// paper ''Kill-Safe Synchronization Abstractions'' by Matthew Flatt and Robert
/// Bruce Findler.  Note that Hopac doesn't, at least not yet, provide all the
/// operations on threads necessary to make the abstraction kill-safe as
/// described in the paper.  Even without the full kill-safety properties this
/// queue can be quite useful and serves as an interesting example of Concurrent
/// ML -style programming making use of a dynamic number of alternatives.
module SelectableQueue =
  /// Creates a new selectable queue.
  val create: unit -> Job<SelectableQueue<'a>>

  /// Constructs an alternative that sends the given message to the selectable
  /// queue.
  val send: SelectableQueue<'a> -> 'a -> Alt<unit>

  /// Constructs an alternative that takes a message that satisfies the given
  /// predicate from the specified selectable queue.
  val take: SelectableQueue<'a> -> ('a -> bool) -> Alt<'a>
