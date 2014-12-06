// Copyright (C) by Vesa Karvonen

namespace Hopac.Experimental

open System
open Hopac

type Stream<'x> =
  | Nil
  | Cons of Value: 'x * Next: Alt<Stream<'x>>

type Streams<'x> = Alt<Stream<'x>>

type StreamSrc<'x>

module StreamSrc =
  val create: unit -> StreamSrc<'x>

  val value: StreamSrc<'x> -> 'x -> Alt<unit>
  val error: StreamSrc<'x> -> exn -> Alt<unit>
  val close: StreamSrc<'x> -> Alt<unit>

  val tap: StreamSrc<'x> -> Streams<'x>

type StreamVar<'x>

module StreamVar =
  val create: 'x -> StreamVar<'x>

  val updateJob: StreamVar<'x> -> ('x -> #Job<'x>) -> Alt<unit>
  val updateFun: StreamVar<'x> -> ('x -> 'x) -> Alt<unit>

  val tap: StreamVar<'x> -> Streams<'x>

module Streams =
  val zero<'x> : Streams<'x>
  val one: 'x -> Streams<'x>

  val never<'x> : Streams<'x>

  val ofSeq: seq<'x> -> Streams<'x>
  val ofAlt: Alt<'x> -> Streams<'x>

  val collectAllAsSeq: Streams<'x> -> Job<seq<'x>>

  val subscribingTo: IObservable<'x> -> (Streams<'x> -> #Job<'y>) -> Job<'y>

  val toObservable: Streams<'x> -> IObservable<'x>

  val amb: Streams<'x> -> Streams<'x> -> Streams<'x>
  val merge: Streams<'x> -> Streams<'x> -> Streams<'x>
  val append: Streams<'x> -> Streams<'x> -> Streams<'x>

  val chooseJob: ('x -> #Job<option<'y>>) -> Streams<'x> -> Streams<'y>
  val chooseFun: ('x ->      option<'y> ) -> Streams<'x> -> Streams<'y>

  val filterJob: ('x -> #Job<bool>) -> Streams<'x> -> Streams<'x>
  val filterFun: ('x ->      bool ) -> Streams<'x> -> Streams<'x>

  val mapJob: ('x -> #Job<'y>) -> Streams<'x> -> Streams<'y>
  val mapFun: ('x ->      'y ) -> Streams<'x> -> Streams<'y>

  val joinWithJob: (Streams<'x> -> Streams<'y> -> #Job<Streams<'y>>) -> Streams<Streams<'x>> -> Streams<'y>
  val joinWithFun: (Streams<'x> -> Streams<'y> ->      Streams<'y> ) -> Streams<Streams<'x>> -> Streams<'y>

  val mergeMapJob: ('x -> #Job<Streams<'y>>) -> Streams<'x> -> Streams<'y>
  val mergeMapFun: ('x ->      Streams<'y> ) -> Streams<'x> -> Streams<'y>

  val appendMapJob: ('x -> #Job<Streams<'y>>) -> Streams<'x> -> Streams<'y>
  val appendMapFun: ('x ->      Streams<'y> ) -> Streams<'x> -> Streams<'y>

  val switchOn: Streams<'x> -> Streams<'x> -> Streams<'x>

  val skipUntil: Alt<_> -> Streams<'x> -> Streams<'x>
  val takeUntil: Alt<_> -> Streams<'x> -> Streams<'x>

  val catchOnce: (exn -> Streams<'x>) -> Streams<'x> -> Streams<'x>

  val collectLatestJob: ('x -> #Job<Streams<'y>>) -> Streams<'x> -> Streams<'y>
  val collectLatestFun: ('x ->      Streams<'y> ) -> Streams<'x> -> Streams<'y>

  val throttle: timeout: Alt<_> -> Streams<'x> -> Streams<'x>

  val zipWithJob: ('x -> 'y -> #Job<'z>) -> Streams<'x> -> Streams<'y> -> Streams<'z>
  val zipWithFun: ('x -> 'y ->      'z ) -> Streams<'x> -> Streams<'y> -> Streams<'z>

  val scanJob: ('s -> 'x -> #Job<'s>) -> 's -> Streams<'x> -> Streams<'s>
  val scanFun: ('s -> 'x ->      's ) -> 's -> Streams<'x> -> Streams<'s>

  val iterJob: ('x -> #Job<_>) -> Streams<'x> -> Job<unit>
  val iterFun: ('x ->    unit) -> Streams<'x> -> Job<unit>

  val delayEachBy: Job<_> -> Streams<'x> -> Streams<'x>

  val groupByJob: ('x -> #Job<'k>) -> Streams<'x> -> Streams<'k * Streams<'x>> when 'k: equality
  val groupByFun: ('x ->      'k ) -> Streams<'x> -> Streams<'k * Streams<'x>> when 'k: equality

  val sample: ticks: Streams<_> -> Streams<'x> -> Streams<'x>
