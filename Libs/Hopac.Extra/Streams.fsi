// Copyright (C) by Vesa Karvonen

namespace Hopac.Extra

open System
open Hopac

/// Represents a point in a non-deterministic stream of values.
type Stream<'x> =
  | Nil
  | Cons of Value: 'x * Next: Alt<Stream<'x>>

/// Represents a non-deterministic stream of values called a choice stream.
type Streams<'x> = Alt<Stream<'x>>

/// Represents an imperative source of a stream of values called a stream source.
type StreamSrc<'x>

/// Operations on stream sources.
module StreamSrc =
  /// Creates a new stream source.
  val create: unit -> StreamSrc<'x>

  /// Appends a new value to the end of the generated stream.  This operation is
  /// atomic and can be safely used from multiple parallel jobs.
  val value: StreamSrc<'x> -> 'x -> Job<unit>

  /// Terminates the stream with an error.  The given exception is raised in the
  /// consumers of the stream if and when they reach the end of the stream.
  val error: StreamSrc<'x> -> exn -> Job<unit>

  /// Terminates the stream.
  val close: StreamSrc<'x> -> Job<unit>

  /// Returns the remainder of the generated stream after the point in time when
  /// `tap` is called.
  val tap: StreamSrc<'x> -> Streams<'x>

/// Represents a mutable variable, called a stream variable, that generates a
/// stream of values as a side-effect.
type StreamVar<'x>

/// Operations on stream variables.
module StreamVar =
  /// Creates a new stream variable.
  val create: 'x -> StreamVar<'x>

  /// Gets the value of the variable.
  val get: StreamVar<'x> -> 'x

  /// Sets the value of the variable and appends the value to the end of the
  /// generated stream.  Note that while this operation is atomic, and can be
  /// safely used from multiple parallel jobs, a combination of `get` and `set`
  /// is not atomic.
  val set: StreamVar<'x> -> 'x -> Job<unit>

  /// Returns the generated stream, including the current value of the variable,
  /// from the point in time when `tap` is called.
  val tap: StreamVar<'x> -> Streams<'x>

/// Operations on choice streams.
module Streams =
  // Introducing streams

  /// A choice stream that never produces any values and never closes.
  val inline never<'x> : Streams<'x>

  /// Constructs a choice stream that is closed with an error.
  val inline error: exn -> Streams<'x>

  /// An empty or closed choice stream.
  val inline nil<'x> : Streams<'x>

  /// `cons x xs` constructs a choice stream whose first value is `x` and the
  /// rest of the stream is like `xs`.
#if DOC
  ///
  /// Node that `cons` and `nil` directly correspond to the ordinary list
  /// constructors `::` and `[]` and you can construct choice streams just like
  /// you would create ordinary lists.
#endif
  val cons: 'x -> Streams<'x> -> Streams<'x>

  /// `one x` is equivalent to `cons x nil`.
  val one: 'x -> Streams<'x>

  /// Preliminary and subject to change.
  val ofSeq: seq<'x> -> Streams<'x>

  /// Preliminary and subject to change.
  val foreverJob: Job<'x> -> Streams<'x>
  /// Preliminary and subject to change.
  val onceJob: Job<'x> -> Streams<'x>

  /// Preliminary and subject to change.
  val repeatJobs: Job<'x> -> Job<_> -> Streams<'x>

  /// Preliminary and subject to change.
  val unfoldJob: ('s -> #Job<option<'x * 's>>) -> 's -> Streams<'x>
  /// Preliminary and subject to change.
  val unfoldFun: ('s -> option<'x * 's>) -> 's -> Streams<'x>

  /// Preliminary and subject to change.
  val iterateJob: ('x -> #Job<'x>) -> 'x -> Streams<'x>
  /// Preliminary and subject to change.
  val iterateFun: ('x -> 'x) -> 'x -> Streams<'x>

  // Observable

  /// Preliminary and subject to change.
  val subscribingTo: IObservable<'x> -> (Streams<'x> -> #Job<'y>) -> Job<'y>

  /// Preliminary and subject to change.
  val toObservable: Streams<'x> -> IObservable<'x>

  // Sequence combinators

  /// Preliminary and subject to change.
  val chooseJob: ('x -> #Job<option<'y>>) -> Streams<'x> -> Streams<'y>
  /// Preliminary and subject to change.
  val chooseFun: ('x -> option<'y>) -> Streams<'x> -> Streams<'y>
  /// Preliminary and subject to change.
  val choose: Streams<option<'x>> -> Streams<'x>

  /// Preliminary and subject to change.
  val filterJob: ('x -> #Job<bool>) -> Streams<'x> -> Streams<'x>
  /// Preliminary and subject to change.
  val filterFun: ('x -> bool) -> Streams<'x> -> Streams<'x>

  /// Preliminary and subject to change.
  val mapJob: ('x -> #Job<'y>) -> Streams<'x> -> Streams<'y>
  /// Preliminary and subject to change.
  val mapFun: ('x -> 'y) -> Streams<'x> -> Streams<'y>

  /// Preliminary and subject to change.
  val groupByJob: ('x -> #Job<'k>) -> Streams<'x> -> Streams<'k * Streams<'x>> when 'k: equality
  /// Preliminary and subject to change.
  val groupByFun: ('x -> 'k) -> Streams<'x> -> Streams<'k * Streams<'x>> when 'k: equality

  /// Preliminary and subject to change.
  val zip: Streams<'x> -> Streams<'y> -> Streams<'x * 'y>

  /// Preliminary and subject to change.
  val scanJob: ('s -> 'x -> #Job<'s>) -> 's -> Streams<'x> -> Streams<'s>
  /// Preliminary and subject to change.
  val scanFun: ('s -> 'x -> 's) -> 's -> Streams<'x> -> Streams<'s>

  /// Preliminary and subject to change.
  val scanFromJob: 's -> ('s -> 'x -> #Job<'s>) -> Streams<'x> -> Streams<'s>
  /// Preliminary and subject to change.
  val scanFromFun: 's -> ('s -> 'x -> 's) -> Streams<'x> -> Streams<'s>

  /// Preliminary and subject to change.
  val distinctByJob: ('x -> #Job<'k>) -> Streams<'x> -> Streams<'x> when 'k: equality
  /// Preliminary and subject to change.
  val distinctByFun: ('x -> 'k) -> Streams<'x> -> Streams<'x> when 'k: equality

  /// Preliminary and subject to change.
  val distinctUntilChangedWithJob: ('x -> 'x -> #Job<bool>) -> Streams<'x> -> Streams<'x>
  /// Preliminary and subject to change.
  val distinctUntilChangedWithFun: ('x -> 'x -> bool) -> Streams<'x> -> Streams<'x>

  /// Preliminary and subject to change.
  val distinctUntilChangedByJob: ('x -> #Job<'k>) -> Streams<'x> -> Streams<'x> when 'k: equality
  /// Preliminary and subject to change.
  val distinctUntilChangedByFun: ('x -> 'k) -> Streams<'x> -> Streams<'x> when 'k: equality

  // Joining streams

  /// Preliminary and subject to change.
  val amb: Streams<'x> -> Streams<'x> -> Streams<'x>
  /// Preliminary and subject to change.
  val merge: Streams<'x> -> Streams<'x> -> Streams<'x>
  /// Preliminary and subject to change.
  val append: Streams<'x> -> Streams<'x> -> Streams<'x>
  /// Preliminary and subject to change.
  val switch: Streams<'x> -> Streams<'x> -> Streams<'x>

  /// Preliminary and subject to change.
  val joinWith: (Streams<'x> -> Streams<'y> -> Streams<'y>)
             -> Streams<Streams<'x>> -> Streams<'y>

  /// Preliminary and subject to change.
  val mapJoin: (Streams<'y> -> Streams<'z> -> Streams<'z>)
            -> ('x -> Streams<'y>)
            -> Streams<'x> -> Streams<'z>

  /// Preliminary and subject to change.
  val ambMap: ('x -> Streams<'y>) -> Streams<'x> -> Streams<'y>
  /// Preliminary and subject to change.
  val mergeMap: ('x -> Streams<'y>) -> Streams<'x> -> Streams<'y>
  /// Preliminary and subject to change.
  val appendMap: ('x -> Streams<'y>) -> Streams<'x> -> Streams<'y>
  /// Preliminary and subject to change.
  val switchMap: ('x -> Streams<'y>) -> Streams<'x> -> Streams<'y>

  // Skipping and taking

  /// Preliminary and subject to change.
  val skip: int -> Streams<'x> -> Streams<'x>
  /// Preliminary and subject to change.
  val take: int -> Streams<'x> -> Streams<'x>

  /// Preliminary and subject to change.
  val skipUntil: Alt<_> -> Streams<'x> -> Streams<'x>
  /// Preliminary and subject to change.
  val takeUntil: Alt<_> -> Streams<'x> -> Streams<'x>

  /// Preliminary and subject to change.
  val switchOn: Streams<'x> -> Streams<'x> -> Streams<'x>

  // Exceptions

  /// Preliminary and subject to change.
  val catchOnce: (exn -> Streams<'x>) -> Streams<'x> -> Streams<'x>
  /// Preliminary and subject to change.
  val catch: (exn -> Streams<'x>) -> Streams<'x> -> Streams<'x>

  // Timing

  /// Preliminary and subject to change.
  val sample: ticks: Streams<_> -> Streams<'x> -> Streams<'x>

  /// Preliminary and subject to change.
  val throttle: timeout: Alt<_> -> Streams<'x> -> Streams<'x>

  /// Preliminary and subject to change.
  val hold: timeout: Job<_> -> Streams<'x> -> Streams<'x>

  /// Preliminary and subject to change.
  val combineLatest: Streams<'x> -> Streams<'y> -> Streams<'x * 'y>

  /// Preliminary and subject to change.
  val delayEachBy: Job<_> -> Streams<'x> -> Streams<'x>

  /// Preliminary and subject to change.
  val atDateTimeOffsets: Streams<DateTimeOffset> -> Streams<DateTimeOffset>
  /// Preliminary and subject to change.
  val atDateTimeOffset: DateTimeOffset -> Streams<DateTimeOffset>

  /// Preliminary and subject to change.
  val afterTimeSpan: TimeSpan -> Streams<unit>

  // Eliminating streams

  /// Preliminary and subject to change.
  val toSeq: Streams<'x> -> Job<ResizeArray<'x>>

  /// Preliminary and subject to change.
  val values: Streams<'x> -> Job<Alt<'x>>

  /// Preliminary and subject to change.
  val foldJob: ('s -> 'x -> #Job<'s>) -> 's -> Streams<'x> -> Job<'s>
  /// Preliminary and subject to change.
  val foldFun: ('s -> 'x -> 's) -> 's -> Streams<'x> -> Job<'s>

  /// Preliminary and subject to change.
  val foldFromJob: 's -> ('s -> 'x -> #Job<'s>) -> Streams<'x> -> Job<'s>
  /// Preliminary and subject to change.
  val foldFromFun: 's -> ('s -> 'x -> 's) -> Streams<'x> -> Job<'s>

  /// Preliminary and subject to change.
  val iterJob: ('x -> #Job<unit>) -> Streams<'x> -> Job<unit>
  /// Preliminary and subject to change.
  val iterFun: ('x -> unit) -> Streams<'x> -> Job<unit>

  /// Preliminary and subject to change.
  val count: Streams<'x> -> Alt<int>

  /// Preliminary and subject to change.
  val head: Streams<'x> -> Alt<'x>
  /// Preliminary and subject to change.
  val tail: Streams<'x> -> Streams<'x>

  /// Preliminary and subject to change.
  val tails: Streams<'x> -> Streams<Streams<'x>>

  /// Preliminary and subject to change.
  val last: Streams<'x> -> Alt<'x>

  /// Preliminary and subject to change.
  val single: Streams<'x> -> Alt<'x>
