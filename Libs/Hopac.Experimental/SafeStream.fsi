// Copyright (C) by Vesa Karvonen

namespace Hopac.Experimental.SafeStream

open Hopac
open System
open System.Threading
open System.Collections.Generic

type Live = interface end
type Lazy = interface inherit Live end

module Stream =
  type Stream<'l, 'x> when 'l :> Live = Stream of Hopac.Stream.Stream<'x>

  type Src<'x> = Hopac.Stream.Src<'x>

  module Src =
    val inline create: unit -> Src<'x>
    val inline value: Src<'x> -> 'x -> Job<unit>
    val inline error: Src<'x> -> exn -> Job<unit>
    val inline close: Src<'x> -> Job<unit>
    val inline tap: Src<'x> -> Stream<Live, 'x>

  type Var<'x> = Hopac.Stream.Var<'x>

  module Var =
    val inline create: 'x -> Var<'x>
    val inline get: Var<'x> -> 'x
    val inline set: Var<'x> -> 'x -> Job<unit>
    val inline tap: Var<'x> -> Stream<Live, 'x>

  val inline nil<'l, 'x when 'l :> Live> : Stream<'l, 'x>
  val inline cons: 'x -> Stream<'l, 'x> -> Stream<'l, 'x>

  val inline one: 'x -> Stream<'l, 'x>

  val inline delay: (unit -> Stream<'l, 'x>) -> Stream<'l, 'x>

  val inline error: exn -> Stream<'l, 'x>

  val inline never<'l, 'x when 'l :> Live> : Stream<'l, 'x>

  val inline ofSeq: seq<'x> -> Stream<'l, 'x>

  val inline indefinitely: Job<'x> -> Stream<'l, 'x>

  val inline once: Job<'x> -> Stream<'l, 'x>

  val inline unfoldJob: ('s -> #Job<option<'x * 's>>) -> 's -> Stream<'l, 'x>
  val inline unfoldFun: ('s -> option<'x * 's>) -> 's -> Stream<'l, 'x>

  val inline generateFuns: 's -> Hopac.Stream.GenerateFuns<'s, 'x> -> Stream<'l, 'x>
  val inline generateFun: 's
                -> ('s -> bool)
                -> ('s -> 's)
                -> ('s -> 'x)
                -> Stream<'l, 'x>

  val inline iterateJob: ('x -> #Job<'x>) -> 'x -> Stream<'l, 'x>
  val inline iterateFun: ('x -> 'x) -> 'x -> Stream<'l, 'x>

  val inline repeat: 'x -> Stream<'l, 'x>
  val inline cycle: Stream<'l, 'x> -> Stream<'l, 'x>

  val inline ofObservableOn: subscribeOn: SynchronizationContext
                   -> IObservable<'x>
                   -> Stream<Live, 'x>
  val inline ofObservableOnMain: IObservable<'x> -> Stream<Live, 'x>
  val inline ofObservable: IObservable<'x> -> Stream<Live, 'x>

  val inline chooseJob: ('x -> #Job<option<'y>>) -> Stream<'l, 'x> -> Stream<'l, 'y>
  val inline chooseFun: ('x -> option<'y>) -> Stream<'l, 'x> -> Stream<'l, 'y>
  val inline choose: Stream<'l, option<'x>> -> Stream<'l, 'x>

  val inline filterJob: ('x -> #Job<bool>) -> Stream<'l, 'x> -> Stream<'l, 'x>
  val inline filterFun: ('x -> bool) -> Stream<'l, 'x> -> Stream<'l, 'x>

  val inline mapJob: ('x -> #Job<'y>) -> Stream<'l, 'x> -> Stream<'l, 'y>
  val inline mapFun: ('x -> 'y) -> Stream<'l, 'x> -> Stream<'l, 'y>
  val inline mapConst: 'y -> Stream<'l, 'x> -> Stream<'l, 'y>

  val inline groupByJob: ('k -> Job<unit> -> Stream<'l, 'x> -> #Job<'y>)
               -> ('x -> #Job<'k>)
               -> Stream<'l, 'x>
               -> Stream<'l, 'y> when 'k: equality
  val inline groupByFun: ('k -> Job<unit> -> Stream<'l, 'x> -> 'y)
               -> ('x -> 'k)
               -> Stream<'l, 'x>
               -> Stream<'l, 'y> when 'k: equality

  val inline zip: Stream<#Lazy, 'x     >
        -> Stream<#Lazy,      'y>
        -> Stream<'l, 'x * 'y>
  val inline zipWithFun: ('x -> 'y -> 'z)
               -> Stream<#Lazy, 'x>
               -> Stream<#Lazy, 'y>
               -> Stream<'l, 'z>

  val inline scanJob: ('s -> 'x -> #Job<'s>) -> 's -> Stream<'l, 'x> -> Stream<'l, 's>
  val inline scanFun: ('s -> 'x -> 's) -> 's -> Stream<'l, 'x> -> Stream<'l, 's>
  val inline scanFromJob: 's -> ('s -> 'x -> #Job<'s>) -> Stream<'l, 'x> -> Stream<'l, 's>
  val inline scanFromFun: 's -> ('s -> 'x -> 's) -> Stream<'l, 'x> -> Stream<'l, 's>

  val inline distinctByJob: ('x -> #Job<'k>)
                  -> Stream<'l, 'x>
                  -> Stream<'l, 'x> when 'k: equality
  val inline distinctByFun: ('x -> 'k)
                  -> Stream<'l, 'x>
                  -> Stream<'l, 'x> when 'k: equality

  val inline distinctUntilChangedWithJob: ('x -> 'x -> #Job<bool>)
                                -> Stream<'l, 'x>
                                -> Stream<'l, 'x>
  val inline distinctUntilChangedWithFun: ('x -> 'x -> bool)
                                -> Stream<'l, 'x>
                                -> Stream<'l, 'x>
  val inline distinctUntilChangedByJob: ('x -> #Job<'k>)
                              -> Stream<'l, 'x>
                              -> Stream<'l, 'x> when 'k: equality
  val inline distinctUntilChangedByFun: ('x -> 'k)
                              -> Stream<'l, 'x>
                              -> Stream<'l, 'x> when 'k: equality
  val inline distinctUntilChanged: Stream<'l, 'x> -> Stream<'l, 'x> when 'x: equality

  val inline amb: Stream<'l, 'x> -> Stream<'l, 'x> -> Stream<'l, 'x>
  val inline merge: Stream<'l, 'x> -> Stream<'l, 'x> -> Stream<'l, 'x>
  val inline append: Stream<'l, 'x> -> Stream<#Lazy, 'x> -> Stream<'l, 'x>
  val inline switch: Stream<'l, 'x> -> Stream<'l, 'x> -> Stream<'l, 'x>

  val inline switchTo: Stream<'l, 'x> -> Stream<'l, 'x> -> Stream<'l, 'x>

  val inline ambAll: Stream<'l, Stream<'l, 'x>> -> Stream<'l, 'x>
  val inline mergeAll: Stream<'l, Stream<'l, 'x>> -> Stream<'l, 'x>
  val inline appendAll: Stream<#Lazy, Stream<#Lazy, 'x>> -> Stream<'l, 'x>
  val inline switchAll: Stream<'l, Stream<'l, 'x>> -> Stream<'l, 'x>

  val inline ambMap: ('x -> Stream<'l, 'y>) -> Stream<'l, 'x> -> Stream<'l, 'y>
  val inline mergeMap: ('x -> Stream<'l, 'y>) -> Stream<'l, 'x> -> Stream<'l, 'y>
  val inline appendMap: ('x -> Stream<#Lazy, 'y>) -> Stream<#Lazy, 'x> -> Stream<'ls, 'y>
  val inline switchMap: ('x -> Stream<'l, 'y>) -> Stream<'l, 'x> -> Stream<'l, 'y>

  val inline skip: int64 -> Stream<'l, 'x> -> Stream<'l, 'x>
  val inline take: int64 -> Stream<'l, 'x> -> Stream<'l, 'x>

  val inline skipUntil: Alt<_> -> Stream<'l, 'x> -> Stream<'l, 'x>
  val inline takeUntil: Alt<_> -> Stream<'l, 'x> -> Stream<'l, 'x>

  val inline takeAndSkipUntil: Alt<_> -> Stream<'l, 'x> -> Stream<'l, 'x> * Stream<'l, 'x>

  val inline skipWhileJob: ('x -> #Job<bool>) -> Stream<'l, 'x> -> Stream<'l, 'x>
  val inline skipWhileFun: ('x -> bool) -> Stream<'l, 'x> -> Stream<'l, 'x>

  val inline takeWhileJob: ('x -> #Job<bool>) -> Stream<'l, 'x> -> Stream<'l, 'x>
  val inline takeWhileFun: ('x -> bool) -> Stream<'l, 'x> -> Stream<'l, 'x>

  val inline catch: (exn -> Stream<'l, 'x>) -> Stream<'l, 'x> -> Stream<'l, 'x>

  val inline onCloseJob: Job<unit> -> Stream<'l, 'x> -> Stream<'l, 'x>
  val inline onCloseFun: (unit -> unit) -> Stream<'l, 'x> -> Stream<'l, 'x>

  val inline doFinalizeJob: Job<unit> -> Stream<'l, 'x> -> Stream<'l, 'x>
  val inline doFinalizeFun: (unit -> unit) -> Stream<'l, 'x> -> Stream<'l, 'x>

  val inline keepPrecedingFuns: Hopac.Stream.KeepPrecedingFuns<'x, 'y>
                      -> Stream<#Live, 'x>
                      -> Stream<'l, 'y>
  val inline keepPreceding: maxCount: int
                  -> Stream<#Live, 'x>
                  -> Stream<'l, Queue<'x>>
  val inline keepPreceding1: Stream<#Live, 'x> -> Stream<'l, 'x>

  val inline keepFollowing1: Stream<#Live, 'x> -> Stream<'l, 'x>

  val inline pullOn: ticks: Stream<'l, _> -> Stream<Lazy, 'x> -> Stream<'l, 'x>

  val inline debounce: timeout: Alt<_> -> Stream<'l, 'x> -> Stream<'l, 'x>

  val inline samplesBefore: ticks: Stream<'l, _> -> Stream<'l, 'x> -> Stream<'l, 'x>
  val inline samplesAfter: ticks: Stream<'l, _> -> Stream<'l, 'x> -> Stream<'l, 'x>

  val inline ignoreUntil: timeout: Job<_> -> Stream<'l, 'x> -> Stream<'l, 'x>
  val inline ignoreWhile: timeout: Job<_> -> Stream<'l, 'x> -> Stream<'l, 'x>

  val inline combineLatest: Stream<'l, 'x> -> Stream<'l, 'y> -> Stream<'l, 'x * 'y>

  val inline shift: timeout: Job<_> -> Stream<'l, 'x> -> Stream<'l, 'x>

  val inline delayEach: timeout: Job<_> -> Stream<#Lazy, 'x> -> Stream<'l, 'x>
  val inline afterEach: timeout: Job<_> -> Stream<#Lazy, 'x> -> Stream<'l, 'x>
  val inline beforeEach: timeout: Job<_> -> Stream<#Lazy, 'x> -> Stream<'l, 'x>

  val inline afterDateTimeOffsets: Stream<#Lazy, DateTimeOffset>
                         -> Stream<'l, DateTimeOffset>
  val inline afterDateTimeOffset: DateTimeOffset -> Stream<'l, DateTimeOffset>
  val inline afterTimeSpan: TimeSpan -> Stream<'l, unit>

  val inline toSeq: Stream<_, 'x> -> Job<ResizeArray<'x>>

  val inline values: Stream<_, 'x> -> Alt<'x>

  val inline foldJob: ('s -> 'x -> #Job<'s>) -> 's -> Stream<_, 'x> -> Job<'s>
  val inline foldFun: ('s -> 'x -> 's) -> 's -> Stream<_, 'x> -> Job<'s>
  val inline foldFromJob: 's -> ('s -> 'x -> #Job<'s>) -> Stream<_, 'x> -> Job<'s>
  val inline foldFromFun: 's -> ('s -> 'x -> 's) -> Stream<_, 'x> -> Job<'s>

  val inline iterJob: ('x -> #Job<unit>) -> Stream<_, 'x> -> Job<unit>
  val inline iterFun: ('x -> unit) -> Stream<_, 'x> -> Job<unit>
  val inline iter: Stream<_, 'x> -> Job<unit>

  val inline count: Stream<_, 'x> -> Job<int64>

  val inline head: Stream<'l, 'x> -> Stream<'l, 'x>
  val inline tail: Stream<'l, 'x> -> Stream<'l, 'x>
  val inline init: Stream<'l, 'x> -> Stream<'l, 'x>

  val inline last: Stream<'l, 'x> -> Stream<'l, 'x>

  val inline tails: Stream<'l, 'x> -> Stream<'l, Stream<'l, 'x>>
  val inline inits: Stream<'l, 'x> -> Stream<'l, Stream<'l, 'x>>

  val inline tailsMapFun: (Stream<'l, 'x> -> 'y) -> Stream<'l, 'x> -> Stream<'l, 'y>
  val inline tailsMapFun: (Stream<'l, 'x> -> 'y) -> Stream<'l, 'x> -> Stream<'l, 'y>

  val inline decon: Stream<'l, 'x> -> Alt<option<'x * Stream<'l, 'x>>>
