// Copyright (C) by Vesa Karvonen

namespace Hopac.Experimental.SafeStream

open System
open System.Collections.Generic
open System.Threading
open Hopac
open Hopac.Infixes
open Hopac.Job.Infixes
open Hopac.Alt.Infixes
open Hopac.Promise.Infixes
open Hopac.Extensions

type Live = interface end
type Lazy = interface inherit Live end

module Stream =

  type Stream<'l, 'x> when 'l :> Live = Stream of Hopac.Stream.Stream<'x>

  let inline Live xs = Stream xs : Stream<Live, _>

  let inline unsafe (Stream xs) = xs

  let inline lazily (Stream xs: Stream<'l, _>) f =
    f xs |> Stream : Stream<'l, _>

  let inline lazily2 (Stream xs: Stream<'l, _>) (Stream ys: Stream<'l, _>) f =
    f xs ys |> Stream : Stream<'l, _>

  type Src<'x> = Hopac.Stream.Src<'x>

  module Src =
    let inline create () = Stream.Src.create ()
    let inline value xS x = Stream.Src.value xS x
    let inline error xS e = Stream.Src.error xS e
    let inline close xS = Stream.Src.close xS
    let inline tap xS = Stream.Src.tap xS |> Live

  type Var<'x> = Hopac.Stream.Var<'x>

  module Var =
    let inline create x = Stream.Var.create x
    let inline get xV = Stream.Var.get xV
    let inline set xV x = Stream.Var.set xV x
    let inline tap xV = Stream.Var.tap xV |> Live

  let inline nil<'l, 'x when 'l :> Live > = Stream.nil<'x> |> Stream : Stream<'l, 'x>
  let inline cons x xs = lazily xs <| Stream.cons x

  let inline one x = Stream.one x |> Stream

  let inline delay (u2xs: unit -> Stream<'l, _>) =
    Stream.delay (u2xs >> unsafe) |> Stream : Stream<'l, _>

  let inline error e = Stream.error e |> Stream

  let inline never<'l, 'x when 'l :> Live> = Stream.never<'x> |> Stream : Stream<'l, 'x>

  let inline ofSeq xs = Stream.ofSeq xs |> Stream

  let inline indefinitely xJ = Stream.indefinitely xJ |> Stream

  let inline once xJ = Stream.once xJ |> Stream

  let inline unfoldJob s2xsO s = Stream.unfoldJob s2xsO s |> Stream
  let inline unfoldFun s2xs s = Stream.unfoldFun s2xs s |> Stream

  let inline generateFuns s gfns = Stream.generateFuns s gfns |> Stream
  let inline generateFun s s2b s2s s2x = Stream.generateFun s s2b s2s s2x |> Stream

  let inline iterateJob x2xJ x = Stream.iterateJob x2xJ x |> Stream
  let inline iterateFun x2x x = Stream.iterateFun x2x x |> Stream

  let inline repeat x = Stream.repeat x |> Stream
  let inline cycle xs = lazily xs Stream.cycle

  let inline ofObservableOn c xO = Stream.ofObservableOn c xO |> Live
  let inline ofObservableOnMain xO = Stream.ofObservableOnMain xO |> Live
  let inline ofObservable xO = Stream.ofObservable xO |> Live

  let inline chooseJob x2yOJ xs = lazily xs <| Stream.chooseJob x2yOJ
  let inline chooseFun x2yO xs = lazily xs <| Stream.chooseFun x2yO
  let inline choose xOs = lazily xOs Stream.choose

  let inline filterJob x2bJ xs = lazily xs <| Stream.filterJob x2bJ
  let inline filterFun x2b xs = lazily xs <| Stream.filterFun x2b

  let inline mapJob x2yJ xs = lazily xs <| Stream.mapJob x2yJ
  let inline mapFun x2y xs = lazily xs <| Stream.mapFun x2y
  let inline mapConst y xs = lazily xs <| Stream.mapConst y

  let inline groupByJob newGroup keyOf (Stream xs: Stream<'l, _>) =
    Stream.groupByJob
        <| fun k uJ xs -> newGroup k uJ (Stream xs : Stream<'l, _>)
        <| keyOf
        <| xs
    |> Stream : Stream<'l, _>

  let inline groupByFun newGroup keyOf (Stream xs: Stream<'l, 'x>) =
    Stream.groupByFun
        <| fun k uJ xs -> newGroup k uJ (Stream xs : Stream<'l, _>)
        <| keyOf
        <| xs
    |> Stream : Stream<'l, _>

  let inline zip (Stream xs: Stream<#Lazy, _>)
                 (Stream ys: Stream<#Lazy, _>) = Stream.zip xs ys |> Stream

  let inline zipWithFun x2y2z
                        (Stream xs: Stream<#Lazy, _>)
                        (Stream ys: Stream<#Lazy, _>) =
    Stream.zipWithFun x2y2z xs ys |> Stream

  let inline scanJob s2x2sJ s xs = lazily xs <| Stream.scanJob s2x2sJ s
  let inline scanFun s2x2s s xs = lazily xs <| Stream.scanFun s2x2s s
  let inline scanFromJob s s2x2sJ xs = lazily xs <| Stream.scanFromJob s s2x2sJ
  let inline scanFromFun s s2x2s xs = lazily xs <| Stream.scanFromFun s s2x2s

  let inline distinctByJob x2kJ xs = lazily xs <| Stream.distinctByJob x2kJ
  let inline distinctByFun x2k xs = lazily xs <| Stream.distinctByFun x2k

  let inline distinctUntilChangedWithJob x2x2bJ xs = lazily xs <| Stream.distinctUntilChangedWithJob x2x2bJ
  let inline distinctUntilChangedWithFun x2x2b xs = lazily xs <| Stream.distinctUntilChangedWithFun x2x2b
  let inline distinctUntilChangedByJob x2kJ xs = lazily xs <| Stream.distinctUntilChangedByJob x2kJ
  let inline distinctUntilChangedByFun x2k xs = lazily xs <| Stream.distinctUntilChangedByFun x2k
  let inline distinctUntilChanged xs = lazily xs Stream.distinctUntilChanged

  let inline amb lhs rhs = lazily2 lhs rhs Stream.amb
  let inline merge lhs rhs = lazily2 lhs rhs Stream.merge
  let inline append (Stream lhs: Stream<'l, _>)
                    (Stream rhs: Stream<#Lazy, _>) : Stream<'l, _> =
    Stream.append lhs rhs |> Stream
  let inline switch lhs rhs = lazily2 lhs rhs Stream.switch

  let inline switchTo rhs lhs = switch lhs rhs

  let inline ambAll (Stream xxs: Stream<'l, Stream<'l, _>>) : Stream<'l, _> =
    Stream.joinWith (fun (Stream lhs) rhs -> Stream.amb lhs rhs) xxs |> Stream
  let inline mergeAll (Stream xxs: Stream<'l, Stream<'l, _>>) : Stream<'l, _> =
    Stream.joinWith (fun (Stream lhs) rhs -> Stream.merge lhs rhs) xxs |> Stream
  let inline appendAll (Stream xxs: Stream<#Lazy, Stream<#Lazy, _>>) =
    Stream.joinWith (fun (Stream lhs) rhs -> Stream.append lhs rhs) xxs |> Stream
  let inline switchAll (Stream xxs: Stream<'l, Stream<'l, _>>) : Stream<'l, _> =
    Stream.joinWith (fun (Stream lhs) rhs -> Stream.switch lhs rhs) xxs |> Stream

  let inline ambMap (x2ys: 'x -> Stream<'l, _>) (Stream xs: Stream<'l, _>) : Stream<'l, _> =
    Stream.mapJoin Stream.amb (x2ys >> unsafe) xs |> Stream
  let inline mergeMap (x2ys: 'x -> Stream<'l, _>) (Stream xs: Stream<'l, _>) : Stream<'l, _> =
    Stream.mapJoin Stream.merge (x2ys >> unsafe) xs |> Stream
  let inline appendMap (x2ys: 'x -> Stream<#Lazy, _>) (Stream xs: Stream<#Lazy, _>) =
    Stream.mapJoin Stream.append (x2ys >> unsafe) xs |> Stream
  let inline switchMap (x2ys: 'x -> Stream<'l, _>) (Stream xs: Stream<'l, _>) : Stream<'l, _> =
    Stream.mapJoin Stream.switch (x2ys >> unsafe) xs |> Stream

  let inline skip n xs = lazily xs <| Stream.skip n
  let inline take n xs = lazily xs <| Stream.take n

  let inline skipUntil yA xs = lazily xs <| Stream.skipUntil yA
  let inline takeUntil yA xs = lazily xs <| Stream.takeUntil yA

  let inline takeAndSkipUntil yA (Stream xs: Stream<'l, _>) : Stream<'l, _> * Stream<'l, _> =
    Stream.takeAndSkipUntil yA xs |> fun (a, b) -> (Stream a, Stream b)

  let inline skipWhileJob x2bJ xs = lazily xs <| Stream.skipWhileJob x2bJ
  let inline skipWhileFun x2b xs = lazily xs <| Stream.skipWhileFun x2b

  let inline takeWhileJob x2bJ xs = lazily xs <| Stream.takeWhileJob x2bJ
  let inline takeWhileFun x2b xs = lazily xs <| Stream.takeWhileFun x2b

  let inline catch (e2xs: _ -> Stream<'l, _>) (Stream xs: Stream<'l, _>) : Stream<'l, _> =
    Stream.catch (e2xs >> unsafe) xs |> Stream

  let inline onCloseJob uJ xs = lazily xs <| Stream.onCloseJob uJ
  let inline onCloseFun u2u xs = lazily xs <| Stream.onCloseFun u2u

  let inline doFinalizeJob uJ xs = lazily xs <| Stream.doFinalizeJob uJ
  let inline doFinalizeFun u2u xs = lazily xs <| Stream.doFinalizeFun u2u

  let inline keepPrecedingFuns fns (Stream xs: Stream<#Live, _>) =
    Stream.keepPrecedingFuns fns xs |> Stream
  let inline keepPreceding maxCount (Stream xs: Stream<#Live, _>) =
    Stream.keepPreceding maxCount xs |> Stream
  let inline keepPreceding1 (Stream xs: Stream<#Live, _>) =
    Stream.keepPreceding1 xs |> Stream

  let inline keepFollowing1 (Stream xs: Stream<#Live, _>) =
    Stream.keepFollowing1 xs |> Stream

  let inline pullOn ticks (Stream xs: Stream<Lazy, _>) =
    lazily ticks <| fun ticks -> Stream.pullOn ticks xs

  let inline debounce timeout xs = lazily xs <| Stream.debounce timeout

  let inline samplesBefore ticks xs = lazily2 ticks xs Stream.samplesBefore
  let inline samplesAfter ticks xs = lazily2 ticks xs Stream.samplesAfter

  let inline ignoreUntil timeout xs = lazily xs <| Stream.ignoreUntil timeout
  let inline ignoreWhile timeout xs = lazily xs <| Stream.ignoreWhile timeout

  let inline combineLatest xs ys = lazily2 xs ys Stream.combineLatest

  let inline shift timeout xs = lazily xs <| Stream.shift timeout

  let inline delayEach timeout (Stream xs: Stream<#Lazy, _>) = Stream.delayEach timeout xs |> Stream
  let inline afterEach timeout (Stream xs: Stream<#Lazy, _>) = Stream.afterEach timeout xs |> Stream
  let inline beforeEach timeout (Stream xs: Stream<#Lazy, _>) = Stream.beforeEach timeout xs |> Stream

  let inline afterDateTimeOffsets (Stream dtos: Stream<#Lazy, _>) = Stream.afterDateTimeOffsets dtos |> Stream
  let inline afterDateTimeOffset dto = Stream.afterDateTimeOffset dto |> Stream
  let inline afterTimeSpan ts = Stream.afterTimeSpan ts |> Stream

  let inline toSeq (Stream xs) = Stream.toSeq xs

  let inline values (Stream xs) = Stream.values xs

  let inline foldJob s2x2sJ s (Stream xs) = Stream.foldJob s2x2sJ s xs
  let inline foldFun s2x2s s (Stream xs) = Stream.foldFun s2x2s s xs
  let inline foldFromJob s s2x2sJ (Stream xs) = Stream.foldFromJob s s2x2sJ xs
  let inline foldFromFun s s2x2s (Stream xs) = Stream.foldFromFun s s2x2s xs

  let inline iterJob x2uJ (Stream xs) = Stream.iterJob x2uJ xs
  let inline iterFun x2u (Stream xs) = Stream.iterFun x2u xs
  let inline iter (Stream xs) = Stream.iter xs

  let inline count (Stream xs) = Stream.count xs

  let inline head xs = lazily xs Stream.head
  let inline tail xs = lazily xs Stream.tail
  let inline init xs = lazily xs Stream.init

  let inline last xs = lazily xs Stream.last

  let inline tails (Stream xs: Stream<'l, _>) : Stream<'l, Stream<'l, _>> =
    Stream.tailsMapFun Stream xs |> Stream
  let inline inits (Stream xs: Stream<'l, _>) : Stream<'l, Stream<'l, _>> =
    Stream.initsMapFun Stream xs |> Stream

  let inline tailsMapFun (xs2y: Stream<'l, _> -> 'y) (Stream xs: Stream<'l, _>) : Stream<'l, _> =
    Stream.tailsMapFun (Stream >> xs2y) xs |> Stream
  let inline initsMapFun (xs2y: Stream<'l, _> -> 'y) (Stream xs: Stream<'l, _>) : Stream<'l, _> =
    Stream.initsMapFun (Stream >> xs2y) xs |> Stream

  let inline decon (Stream xs: Stream<'l, _>) : Alt<option<'x * Stream<'l, _>>> =
    xs |>>? function Stream.Cons (x, xs) -> Some (x, Stream xs) | Stream.Nil -> None
