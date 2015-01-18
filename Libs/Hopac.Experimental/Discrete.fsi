// Copyright (C) by Vesa Karvonen

namespace Hopac.Experimental

open Hopac

module Alt =
  /// Operations for treating alternatives as a kind of discrete event sources
  /// allowing combinators for limited froms of event throttling, mapping and
  /// filtering.
#if DOC
  ///
  /// The essence of this module is that a subset of alternatives forms a monad
  /// with plus where `once` is return, `switchMap` is bind, `never` is zero and
  /// `merge` is plus.  Note that many forms of alternatives, like `always`, do
  /// not combine in useful ways under this interpretation.
  ///
  /// The limitation of technique is the one-shot or no-memory nature of the
  /// discrete event sources, which seems to make many useful combinators that
  /// can be expressed by choice streams, for example, impossible.
#endif
  module Discrete =
    /// Given two event sources, creates an event source that produces events
    /// from both of the given event sources.
    val merge: Alt<'x> -> Alt<'x> -> Alt<'x>

    /// Given an event source, `xE`, and a function that creates a new event
    /// source, `x2yE`, returns an event source that always produces events from
    /// the event source created by `x2yE` based on the latest value produced by
    /// `xE`.
    val switchMap: ('x -> Alt<'y>) -> Alt<'x> -> Alt<'y>

    /// Given a timeout and an event source, `xE`, creates an event source that
    /// produces only the events of `xE` after which there is a period of
    /// timeout without any events from `xE`.
    val throttle: timeout: Alt<_> -> Alt<'x> -> Alt<'x>

    /// Given a pair of event sources, creates an event source that produces
    /// pairs of the latest events from the sources.
    val combineLatest: Alt<'x> -> Alt<'y> -> Alt<'x * 'y>

    /// Given a partial function and an event source, creates an event source
    /// that produces the partially mapped events.
    val choose: ('x -> option<'y>) -> Alt<'x> -> Alt<'y>

    /// Given a predicate and an event source, creates an event source that
    /// produces the events that satisfy the predicate.
    val filter: ('x -> bool) -> Alt<'x> -> Alt<'x>

    /// Given a function and an event source, creates an event source that
    /// produces the mapped events.
    val map: ('x -> 'y) -> Alt<'x> -> Alt<'y>
