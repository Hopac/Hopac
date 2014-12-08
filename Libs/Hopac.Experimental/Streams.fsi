// Copyright (C) by Vesa Karvonen

namespace Hopac.Experimental

open System
open Hopac

type Stream<'x> =
  | Nil
  | Cons of Value: 'x * Next: Alt<Stream<'x>>

/// Represents a lazy, non-deterministic stream of values.
type Streams<'x> = Alt<Stream<'x>>

/// Represents an imperative source of a stream of values.
type StreamSrc<'x>

/// Operations on a stream source.
module StreamSrc =
  val create: unit -> StreamSrc<'x>

  val value: StreamSrc<'x> -> 'x  -> Alt<unit>
  val error: StreamSrc<'x> -> exn -> Alt<unit>
  val close: StreamSrc<'x> ->        Alt<unit>

  val tap: StreamSrc<'x> -> Streams<'x>

/// Represents a mutable variable producing a stream of values.
type StreamVar<'x>

module StreamVar =
  val create: 'x -> StreamVar<'x>

  val updateJob: StreamVar<'x> -> ('x -> #Job<'x>) -> Alt<unit>
  val updateFun: StreamVar<'x> -> ('x ->      'x ) -> Alt<unit>

  val tap: StreamVar<'x> -> Streams<'x>

/// Operations on lazy, non-deterministic streams of values.
module Streams =
  // Introducing streams

  val inline never<'x> : Streams<'x>

  val inline nil<'x> : Streams<'x>
  val cons: 'x -> Streams<'x> -> Streams<'x>

  val one: 'x -> Streams<'x>

  val ofSeq: seq<'x> -> Streams<'x>

  val foreverJob: Job<'x> -> Streams<'x>
  val onceJob: Job<'x> -> Streams<'x>

  val unfoldJob: ('s -> #Job<option<'x * 's>>) -> 's -> Streams<'x>
  val unfoldFun: ('s ->      option<'x * 's> ) -> 's -> Streams<'x>

  // Observable

  val subscribingTo: IObservable<'x> -> (Streams<'x> -> #Job<'y>) -> Job<'y>

  val toObservable: Streams<'x> -> IObservable<'x>

  // Sequence combinators

  val chooseJob: ('x -> #Job<option<'y>>) -> Streams<'x> -> Streams<'y>
  val chooseFun: ('x ->      option<'y> ) -> Streams<'x> -> Streams<'y>

  val filterJob: ('x -> #Job<bool>) -> Streams<'x> -> Streams<'x>
  val filterFun: ('x ->      bool ) -> Streams<'x> -> Streams<'x>

  val mapJob: ('x -> #Job<'y>) -> Streams<'x> -> Streams<'y>
  val mapFun: ('x ->      'y ) -> Streams<'x> -> Streams<'y>

  val groupByJob: ('x -> #Job<'k>) -> Streams<'x> -> Streams<'k * Streams<'x>> when 'k: equality
  val groupByFun: ('x ->      'k ) -> Streams<'x> -> Streams<'k * Streams<'x>> when 'k: equality

  val zipWithJob: ('x -> 'y -> #Job<'z>) -> Streams<'x> -> Streams<'y> -> Streams<'z>
  val zipWithFun: ('x -> 'y ->      'z ) -> Streams<'x> -> Streams<'y> -> Streams<'z>

  val scanJob: ('s -> 'x -> #Job<'s>) -> 's -> Streams<'x> -> Streams<'s>
  val scanFun: ('s -> 'x ->      's ) -> 's -> Streams<'x> -> Streams<'s>

  val distinctByJob: ('x -> #Job<'k>) -> Streams<'x> -> Streams<'x> when 'k: equality
  val distinctByFun: ('x ->      'k ) -> Streams<'x> -> Streams<'x> when 'k: equality

  val distinctUntilChangedByJob: ('x -> #Job<'k>) -> Streams<'x> -> Streams<'x> when 'k: equality
  val distinctUntilChangedByFun: ('x ->      'k ) -> Streams<'x> -> Streams<'x> when 'k: equality

  // Joining streams

  val amb:    Streams<'x> -> Streams<'x> -> Streams<'x>
  val merge:  Streams<'x> -> Streams<'x> -> Streams<'x>
  val append: Streams<'x> -> Streams<'x> -> Streams<'x>
  val switch: Streams<'x> -> Streams<'x> -> Streams<'x>

  val joinWith: (Streams<'x> -> Streams<'y> -> Streams<'y>)
             -> Streams<Streams<'x>> -> Streams<'y>

  val mapJoin: (Streams<'y> -> Streams<'z> -> Streams<'z>)
            -> ('x -> Streams<'y>)
            -> Streams<'x> -> Streams<'z>

  val ambMap:    ('x -> Streams<'y>) -> Streams<'x> -> Streams<'y>
  val mergeMap:  ('x -> Streams<'y>) -> Streams<'x> -> Streams<'y>
  val appendMap: ('x -> Streams<'y>) -> Streams<'x> -> Streams<'y>
  val switchMap: ('x -> Streams<'y>) -> Streams<'x> -> Streams<'y>

  // Skipping and taking

  val skip: int -> Streams<'x> -> Streams<'x>
  val take: int -> Streams<'x> -> Streams<'x>

  val skipUntil: Alt<_> -> Streams<'x> -> Streams<'x>
  val takeUntil: Alt<_> -> Streams<'x> -> Streams<'x>

  val switchOn: Streams<'x> -> Streams<'x> -> Streams<'x>

  // Exceptions

  val catchOnce: (exn -> Streams<'x>) -> Streams<'x> -> Streams<'x>
  val catch: (exn -> Streams<'x>) -> Streams<'x> -> Streams<'x>

  // Timing

  val sample: ticks: Streams<_> -> Streams<'x> -> Streams<'x>

  val throttle: timeout: Alt<_> -> Streams<'x> -> Streams<'x>

  val combineLatestWithJob: ('x -> 'y -> #Job<'z>) -> Streams<'x> -> Streams<'y> -> Streams<'z>
  val combineLatestWithFun: ('x -> 'y ->      'z ) -> Streams<'x> -> Streams<'y> -> Streams<'z>

  val delayEachBy: Job<_> -> Streams<'x> -> Streams<'x>

  // Eliminating streams

  val toSeq: Streams<'x> -> Job<ResizeArray<'x>>

  val foldJob: ('s -> 'x -> #Job<'s>) -> 's -> Streams<'x> -> Job<'s>
  val foldFun: ('s -> 'x ->      's ) -> 's -> Streams<'x> -> Job<'s>

  val iterJob: ('x -> #Job<_>) -> Streams<'x> -> Job<unit>
  val iterFun: ('x ->    unit) -> Streams<'x> -> Job<unit>

  val count: Streams<'x> -> Alt<int>

  val head: Streams<'x> -> Alt<'x>
  val tail: Streams<'x> -> Streams<'x>

  val last: Streams<'x> -> Alt<'x>

  val single: Streams<'x> -> Alt<'x>
