// Copyright (C) by Vesa Karvonen

module Streams

open System.Reactive.Linq
open System.Reactive.Concurrency
open System
open System.Diagnostics
open System.Threading
open Hopac
open Hopac.Bench
open Hopac.Infixes

module Stream =
  open Stream

  let get xs =
    xs >>- function Cons (x, _) -> x | _ -> failwith "Nil"

  let fix (xs2xs: Stream<'x> -> Stream<'x>) : Stream<'x> =
    let xs = IVar ()
    Job.delay (fun () -> xs2xs xs) >>=* fun xs' -> xs *<= xs' >>=. xs

  let ints n =
    generateFun 0
     <| fun i -> i < n
     <| fun i -> i + 1
     <| fun i -> i

module Obs =
  let ints n =
    Observable.Generate(0, (fun i -> i < n), (fun i -> i + 1), (fun i -> i))

module BasicBench =

  let bench name max f =
    try
      for n in [10; 100; 1000; 10000; 100000; 1000000; 10000000] do
        if n <= max then
          printf "%s: " name
          GC.clean ()
          let start = Stopwatch.StartNew ()
          f n
          let elapsed = start.Elapsed
          printfn "%8d in %As is %11.2f ops/s"
            n elapsed (float n / elapsed.TotalSeconds)
    with e ->
      printfn "Failed with: %A" e

  do
     bench "keepFollowing1" 10000000 ^ fun n ->
       Stream.ints n
       |> Stream.keepFollowing1
       |> Stream.iter |> run

     bench "keepPreceding1" 10000000 ^ fun n ->
       Stream.append (Stream.ints n) (Stream.cons -1 Stream.never)
       |> Stream.keepPreceding1
       |> Stream.takeWhileFun ^ fun i -> 0 <= i
       |> Stream.iter |> run

     bench "duringEach" 10000000 ^ fun n ->
       Stream.ints n
       |> Stream.duringEach ^ Job.unit ()
       |> Stream.duringEach ^ Job.unit ()
       |> Stream.duringEach ^ Job.unit ()
       |> Stream.iter |> run

     bench "mapPipelinedJob" 1000000 ^ fun n ->
       Stream.ints n
       |> Stream.mapPipelinedJob (Environment.ProcessorCount * 2) ^ Job.lift ^ fun n -> n + 1
       |> Stream.iter |> run

     bench "ignoreWhile" 1000000 ^ fun n ->
       Stream.ints n
       |> Stream.ignoreWhile ^ Job.unit ()
       |> Stream.iter |> run

     bench "shift" 1000000 ^ fun n ->
       Stream.ints n
       |> Stream.shift ^ Job.unit ()
       |> Stream.shift ^ Job.unit ()
       |> Stream.shift ^ Job.unit ()
       |> Stream.iter |> run

     bench "appendMap" 1000000 ^ fun n ->
       let one = Stream.one 1
       Stream.ints n
       |> Stream.appendMap ^ fun _ -> one
       |> Stream.appendMap ^ fun _ -> one
       |> Stream.iter |> run

     bench "mergeMap" 1000000 ^ fun n ->
       let one = Stream.one 1
       Stream.ints n
       |> Stream.mergeMap ^ fun _ -> one
       |> Stream.mergeMap ^ fun _ -> one
       |> Stream.iter |> run

     bench "switchMap" 1000000 ^ fun n ->
       let one = Stream.one 1
       Stream.ints n
       |> Stream.switchMap ^ fun _ -> one
       |> Stream.switchMap ^ fun _ -> one
       |> Stream.iter |> run

     bench "delayEach" 10000000 ^ fun n ->
       Stream.ints n
       |> Stream.delayEach ^ Job.unit ()
       |> Stream.delayEach ^ Job.unit ()
       |> Stream.delayEach ^ Job.unit ()
       |> Stream.iter |> run

     bench "afterEach" 10000000 ^ fun n ->
       Stream.ints n
       |> Stream.afterEach ^ Job.unit ()
       |> Stream.afterEach ^ Job.unit ()
       |> Stream.afterEach ^ Job.unit ()
       |> Stream.iter |> run

     bench "distinctUntilChanged" 10000000 ^ fun n ->
       Stream.ints n
       |> Stream.distinctUntilChanged
       |> Stream.distinctUntilChanged
       |> Stream.distinctUntilChanged
       |> Stream.iter |> run

     bench "take" 10000000 ^ fun n ->
       Stream.ints n
       |> Stream.take ^ int64 n
       |> Stream.take ^ int64 n
       |> Stream.take ^ int64 n
       |> Stream.iter |> run

     bench "Observable.Take" 10000000 ^ fun n ->
       Obs.ints(n).Take(n).Take(n).Take(n).Subscribe() |> ignore

     bench "zip" 10000000 ^ fun n ->
       Stream.zip
        <| Stream.ints n
        <| Stream.ints n
       |> Stream.iter |> run

     bench "combineLatest" 10000000 ^ fun n ->
       Stream.combineLatest
        <| Stream.ints n
        <| Stream.ints n
       |> Stream.iter |> run

     bench "scanFromFun" 10000000 ^ fun n ->
       Stream.ints n
       |> Stream.scanFromFun 0 ^ fun x y -> x + y
       |> Stream.scanFromFun 0 ^ fun x y -> x - y
       |> Stream.iter |> run

     bench "filterFun" 10000000 ^ fun n ->
       Stream.ints n
       |> Stream.filterFun ^ fun i -> (i &&& 1) = 0
       |> Stream.filterFun ^ fun i -> (i &&& 2) = 0
       |> Stream.filterFun ^ fun i -> (i &&& 4) = 0
       |> Stream.filterFun ^ fun i -> (i &&& 8) = 0
       |> Stream.iter |> run
     bench "Observable.Where" 1000000 ^ fun n ->
       Obs.ints(n)
        .Where(fun i -> (i &&& 1) = 0)
        .Where(fun i -> (i &&& 2) = 0)
        .Where(fun i -> (i &&& 4) = 0)
        .Where(fun i -> (i &&& 8) = 0)
        .Subscribe() |> ignore

     bench "chooseFun" 1000000 ^ fun n ->
       Stream.ints n
       |> Stream.chooseFun ^ fun i -> if (i &&& 1) = 0 then Some (i+1) else None
       |> Stream.chooseFun ^ fun i -> if (i &&& 2) = 0 then Some (i+2) else None
       |> Stream.chooseFun ^ fun i -> if (i &&& 4) = 0 then Some (i+4) else None
       |> Stream.iter |> run

     bench "merge" 1000000 ^ fun n ->
       Stream.merge
        <| Stream.ints n
        <| Stream.ints n
       |> Stream.iter |> run

     bench "mapFun" 1000000 ^ fun n ->
       Stream.ints n
       |> Stream.mapFun ^ fun n -> n + 1
       |> Stream.mapFun ^ fun n -> n - 1
       |> Stream.iter |> run
     bench "Observable.Select" 1000000 ^ fun n ->
       Obs.ints(n)
        .Select(fun n -> n + 1)
        .Select(fun n -> n - 1)
        .Subscribe() |> ignore

     bench "generateFun" 10000000 ^ fun n ->
       Stream.ints n |> Stream.iter |> run
     bench "Observable.Generate" 10000000 ^ fun n ->
       Obs.ints(n).Subscribe() |> ignore

module Streams =
  let split xss =
    xss
    |> Seq.collect ^ fun xs ->
         [Stream.mapFun (fun x ->  x      / 2) xs
          Stream.mapFun (fun x -> (x + 1) / 2) xs]
  let join xss =
    xss
    |> Seq.pairwise
    |> Seq.map ^ fun (xs, ys) ->
         Stream.zipWithFun (+) xs ys

  let ints _ = Stream.iterateFun (fun x -> x+1) 0

  module Fib =
    let fibs () = Stream.fix ^ fun fibs ->
      Stream.cons 0I (Stream.cons 1I (Stream.zipWithFun (+) fibs (Stream.tail fibs)))

    let run n =
      let timer = Stopwatch.StartNew ()
      let result =
        fibs ()
        |> Stream.skip n
        |> Stream.get
        |> run
      let stop = timer.Elapsed
      printfn "fib %d = %s, took %A" n (result.ToString ()) stop

  module Pyramid =
    let rec pyramid m n =
      if m <= 0
      then ints (n+1)
      else let xs = pyramid (m-1) n
           let ys = pyramid (m-1) n
           Stream.zipWithFun (+) xs ys

    let run m n =
      let timer = Stopwatch.StartNew ()
      let result =
        pyramid m n
        |> Stream.skip ^ int64 n
        |> Stream.get
        |> run
      let stop = timer.Elapsed
      printfn "s pyramid %d %d = %s, took %A" m n (result.ToString ()) stop

  module Diamond =
    let rec repeat n f xs = if n = 0 then xs else repeat (n-1) f (f xs)
    let diamond m n =
      Seq.singleton (ints (n+1))
      |> repeat m split
      |> repeat m join
      |> Seq.head

    let run m n =
      let timer = Stopwatch.StartNew ()
      let result =
        diamond m n
        |> Stream.skip ^ int64 n
        |> Stream.get
        |> run
      let stop = timer.Elapsed
      printfn "s diamond %d %d = %s, took %A" m n (result.ToString ()) stop

module Rx =
  let split xss =
    xss
    |> Seq.collect ^ fun (xs: IObservable<_>) ->
         [xs.Select(fun x ->  x      / 2)
          xs.Select(fun x -> (x + 1) / 2)]
  let join xss =
    xss
    |> Seq.pairwise
    |> Seq.map ^ fun (xs: IObservable<_>, ys: IObservable<_>) ->
         Observable.Zip(xs, ys, fun x y -> x+y)

  let ints n =
    Observable.Generate(0, (fun x -> x <= n), (fun x -> x+1), (fun x -> x))

  module Pyramid =
    let rec pyramid m n =
      if m <= 0
      then ints (n+1)
      else let xs = pyramid (m-1) n
           let ys = pyramid (m-1) n
           Observable.Zip(xs, ys, fun x y -> x+y)

    let run m n =
      let timer = Stopwatch.StartNew ()
      let result = ref -1
      (pyramid m n)
       .Skip(n)
       .Take(1)
       .SubscribeOn(ThreadPoolScheduler.Instance)
       .Subscribe(fun x ->
         lock result ^ fun () ->
           result := x
           Monitor.Pulse result) |> ignore
      lock result ^ fun () ->
        while !result < 0 do
          Monitor.Wait result |> ignore
      let stop = timer.Elapsed
      printfn "r pyramid %d %d = %s, took %A" m n ((!result).ToString ()) stop

  module Diamond =
    let rec repeat n f xs = if n = 0 then xs else repeat (n-1) f (f xs)
    let diamond m n =
      Seq.singleton (ints (n+1))
      |> repeat m split
      |> repeat m join
      |> Seq.head

    let run m n =
      let timer = Stopwatch.StartNew ()
      let result = ref -1
      (diamond m n)
       .Skip(n)
       .Take(1)
       .SubscribeOn(ThreadPoolScheduler.Instance)
       .Subscribe (fun x ->
         lock result ^ fun () ->
           result := x
           Monitor.Pulse result) |> ignore
      lock result ^ fun () ->
        while !result < 0 do
          Monitor.Wait result |> ignore
      let stop = timer.Elapsed
      printfn "r diamond %d %d = %s, took %A" m n ((!result).ToString ()) stop

do for m in [0; 1; 4; 6] do
     for n in [10; 160000; 320000] do
       for f in [Streams.Pyramid.run; Rx.Pyramid.run;
                 Streams.Diamond.run; Rx.Diamond.run] do
         GC.clean ()
         f m n
       printfn ""

//do for n in [10L; 100L; 1000L; 10000L; 100000L] do
//     StreamFib.run n
