// Copyright (C) by Housemarque, Inc.

module AdHocTests

open FsCheck
open System
open System.Numerics
open Hopac
open Hopac.Alt.Infixes
open Hopac.Job.Infixes
open Hopac.Infixes

module Stream =
  let ofList (xs: list<_>) = Stream.ofSeq xs
  let toList xs = Stream.toSeq xs |>> List.ofSeq
  let onList f xs = ofList xs |> f |> toList |> run
  let onList2 f xs ys = f (ofList xs) (ofList ys) |> toList |> run

let testEq exp act =
  if exp <> act
  then printfn "Expected %A, but got %A" exp act
  else printfn "OK"

let quick = Check.Quick

do let fibs: Stream<BigInteger> =
     let rec lp f0 f1 = Stream.cons f0 << Stream.delay <| fun () -> lp f1 (f0 + f1)
     lp 0I 1I
   fibs
   |> Stream.take 8L
   |> Stream.toList
   |> run
   |> testEq [0I;1I;1I;2I;3I;5I;8I;13I]

do Stream.append
    (Stream.iterateFun ((+) 1) 1 |> Stream.take 4L)
    (Stream.ofSeq [5;6;7])
   |> Stream.mapFun (fun x -> x + 1)
   |> Stream.toList
   |> run
   |> testEq [2;3;4;5;6;7;8]

// The following are some quickly written naive property based test

do quick <| fun xs ->
   let s = Stream.ofList xs
   Stream.zip s (Stream.indefinitely (Stream.values s))
   |> Stream.toList |> run = List.zip xs xs

do quick <| fun (xs: list<_>) (f: _ -> _) ->
   Stream.onList (Stream.mapFun f) xs = List.map f xs
do quick <| fun (xs: list<_>) (f: _ -> _) ->
   Stream.onList (Stream.mapJob (f >> Job.result)) xs = List.map f xs
do quick <| fun (xs: list<_>) (f: _ -> option<_>) ->
   Stream.onList (Stream.chooseFun f) xs = List.choose f xs
do quick <| fun (xs: list<_>) (f: _ -> option<_>) ->
   Stream.onList (Stream.chooseJob (f >> Job.result)) xs = List.choose f xs
do quick <| fun xs ys ->
   Stream.onList2 Stream.append xs ys = List.append xs ys
do quick <| fun i xs ->
   let i = abs i
   Stream.onList (fun xs -> Stream.append (Stream.take i xs) (Stream.skip i xs)) xs = xs
do quick <| fun xs ->
   Stream.onList (fun xs -> Stream.append (Stream.head xs) (Stream.tail xs)) xs = xs
do quick <| fun xs ->
   Stream.onList (fun xs -> Stream.append (Stream.init xs) (Stream.last xs)) xs = xs
do quick <| fun xs ->
   xs
   |> Stream.onList (fun xs ->
      Stream.zip (Stream.inits xs) (Stream.tails xs)
      |> Stream.mapJob (fun (init, tail) ->
         Stream.append init tail |> Stream.toList))
   |> List.forall (fun xs' -> xs' = xs)
do quick <| fun xs ys ->
   Stream.onList2 Stream.zip xs ys = List.ofSeq (Seq.zip xs ys)
do quick <| fun xs ->
   Stream.ofSeq xs |> Stream.count |> run |> int = List.length xs
do quick <| fun xs s f ->
   xs |> Stream.onList (Stream.scanFun f s) = List.scan f s xs
do quick <| fun xs s f ->
   xs
   |> Stream.onList
       (Stream.scanJob (fun s x -> f s x |> Job.result) s) = List.scan f s xs
do quick <| fun xs f ->
   xs |> Stream.onList (Stream.distinctByFun f) = List.ofSeq (Seq.distinctBy f xs)
do quick <| fun (xs: list<byte>) (f: byte -> byte) ->
   xs |> Stream.onList (Stream.distinctByJob (f >> Job.result)) = List.ofSeq (Seq.distinctBy f xs)
do quick <| fun (xs: list<int>) (f: int -> byte) ->
   (xs
    |> Stream.onList (fun xs ->
       xs
       |> Stream.groupByFun f
       |> Stream.mapJob (fun (_, xs) -> Stream.toList xs)))
    = (xs
      |> Seq.groupBy f
      |> Seq.map (fun (_, xs) -> List.ofSeq xs)
      |> List.ofSeq)
do quick <| fun xs f ->
   xs |> Stream.onList (Stream.filterFun f) = List.filter f xs
do quick <| fun xs f ->
   xs |> Stream.onList (Stream.filterJob (f >> Job.result)) = List.filter f xs
do quick <| fun s f ->
   Stream.unfoldFun f s |> Stream.take 10L |> Stream.toList |> run =
    List.ofSeq (Seq.unfold f s |> Seq.truncate 10)
do quick <| fun s f ->
   Stream.unfoldJob (f >> Job.result) s |> Stream.take 10L |> Stream.toList |> run =
    List.ofSeq (Seq.unfold f s |> Seq.truncate 10)
do quick <| fun (xs: list<byte>) ->
   Stream.onList (Stream.distinctUntilChangedWithFun (=)) xs =
    Stream.onList (Stream.distinctUntilChangedWithJob (fun a b -> a = b |> Job.result)) xs
do quick <| fun (xs: list<byte>) (f: byte -> byte) ->
   Stream.onList (Stream.distinctUntilChangedByFun f) xs =
    Stream.onList (Stream.distinctUntilChangedByJob (f >> Job.result)) xs

do quick <| fun xs -> xs |> Stream.onList (Stream.shift (timeOutMillis 1)) = xs
do quick <| fun xs -> xs |> Stream.onList (Stream.delayEach (timeOutMillis 1)) = xs
do quick <| fun xs -> xs |> Stream.onList (Stream.afterEach (timeOutMillis 1)) = xs
do quick <| fun xs -> xs |> Stream.onList (Stream.beforeEach (timeOutMillis 1)) = xs
