// Copyright (C) by Vesa Karvonen

namespace Hopac.Extra

open System
open Hopac

/// Preliminary and subject to change.
type Stream<'x> =
  | Nil
  | Cons of Value: 'x * Next: Alt<Stream<'x>>

/// Preliminary and subject to change.
type Streams<'x> = Alt<Stream<'x>>

/// Preliminary and subject to change.
type StreamSrc<'x>

/// Preliminary and subject to change.
module StreamSrc =
  /// Preliminary and subject to change.
  val create: unit -> StreamSrc<'x>

  /// Preliminary and subject to change.
  val value: StreamSrc<'x> -> 'x -> Alt<unit>
  /// Preliminary and subject to change.
  val error: StreamSrc<'x> -> exn -> Alt<unit>
  /// Preliminary and subject to change.
  val close: StreamSrc<'x> -> Alt<unit>

  /// Preliminary and subject to change.
  val tap: StreamSrc<'x> -> Streams<'x>

/// Preliminary and subject to change.
type StreamVar<'x>

/// Preliminary and subject to change.
module StreamVar =
  /// Preliminary and subject to change.
  val create: 'x -> StreamVar<'x>

  /// Preliminary and subject to change.
  val get: StreamVar<'x> -> Job<'x>

  /// Preliminary and subject to change.
  val updateJob: StreamVar<'x> -> ('x -> #Job<'x>) -> Alt<unit>
  /// Preliminary and subject to change.
  val updateFun: StreamVar<'x> -> ('x -> 'x) -> Alt<unit>

  /// Preliminary and subject to change.
  val maybeUpdateFun: StreamVar<'x> -> ('x -> option<'x>) -> Alt<unit>

  /// Preliminary and subject to change.
  val tap: StreamVar<'x> -> Streams<'x>

/// Preliminary and subject to change.
module Streams =
  // Introducing streams

  /// Preliminary and subject to change.
  val inline never<'x> : Streams<'x>

  /// Preliminary and subject to change.
  val inline nil<'x> : Streams<'x>
  /// Preliminary and subject to change.
  val cons: 'x -> Streams<'x> -> Streams<'x>

  /// Preliminary and subject to change.
  val one: 'x -> Streams<'x>

  /// Preliminary and subject to change.
  val ofSeq: seq<'x> -> Streams<'x>

  /// Preliminary and subject to change.
  val foreverJob: Job<'x> -> Streams<'x>
  /// Preliminary and subject to change.
  val onceJob: Job<'x> -> Streams<'x>

  /// Preliminary and subject to change.
  val unfoldJob: ('s -> #Job<option<'x * 's>>) -> 's -> Streams<'x>
  /// Preliminary and subject to change.
  val unfoldFun: ('s -> option<'x * 's>) -> 's -> Streams<'x>

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
  val distinctByJob: ('x -> #Job<'k>) -> Streams<'x> -> Streams<'x> when 'k: equality
  /// Preliminary and subject to change.
  val distinctByFun: ('x -> 'k) -> Streams<'x> -> Streams<'x> when 'k: equality

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
  val iterJob: ('x -> #Job<_>) -> Streams<'x> -> Job<unit>
  /// Preliminary and subject to change.
  val iterFun: ('x -> unit) -> Streams<'x> -> Job<unit>

  /// Preliminary and subject to change.
  val count: Streams<'x> -> Alt<int>

  /// Preliminary and subject to change.
  val head: Streams<'x> -> Alt<'x>
  /// Preliminary and subject to change.
  val tail: Streams<'x> -> Streams<'x>

  /// Preliminary and subject to change.
  val last: Streams<'x> -> Alt<'x>

  /// Preliminary and subject to change.
  val single: Streams<'x> -> Alt<'x>
