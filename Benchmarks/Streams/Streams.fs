// Copyright (C) by Vesa Karvonen

module Streams

open FSharp.Control.Reactive
open System.Reactive.Linq
open System
open System.Diagnostics
open System.Threading
open Hopac
open Hopac.Alt.Infixes
open Hopac.Job.Infixes
open Hopac.Promise.Infixes
open Hopac.Infixes

module Stream =
  let get xs =
    xs |>> function Stream.Cons (x, _) -> x | _ -> failwith "Nil"
  
  let fix (xs2xs: Stream<'x> -> Stream<'x>) : Stream<'x> =
    let xs = ivar ()
    Job.delay (fun () -> xs2xs xs) >>=* fun xs' -> xs <-= xs' >>. xs

module Streams =
  let split xss =
    xss
    |> Seq.collect (fun xs ->
       [Stream.mapFun (fun x ->  x      / 2) xs;
        Stream.mapFun (fun x -> (x + 1) / 2) xs])
  let join xss =
    xss
    |> Seq.pairwise
    |> Seq.map (fun (xs, ys) ->
       Stream.zipWithFun (+) xs ys)

  let ints () = Stream.iterateFun ((+) 1) 0

  module Fib =
    let fibs () = Stream.fix <| fun fibs ->
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
    let rec pyramid n =
      if n <= 0
      then ints ()
      else let xs = pyramid (n-1)
           let ys = pyramid (n-1)
           Stream.zipWithFun (+) xs ys

    let run m n =
      let timer = Stopwatch.StartNew ()
      let result =
        pyramid m
        |> Stream.skip (int64 n)
        |> Stream.get
        |> run
      let stop = timer.Elapsed
      printfn "s pyramid %d %d = %s, took %A" m n (result.ToString ()) stop

  module Diamond =
    let rec repeat n f xs = if n = 0 then xs else repeat (n-1) f (f xs)
    let diamond n =
      Seq.singleton (ints ())
      |> repeat n split
      |> repeat n join
      |> Seq.head

    let run m n =
      let timer = Stopwatch.StartNew ()
      let result =
        diamond m
        |> Stream.skip (int64 n)
        |> Stream.get
        |> run
      let stop = timer.Elapsed
      printfn "s diamond %d %d = %s, took %A" m n (result.ToString ()) stop

module Rx =
  let split xss =
    xss
    |> Seq.collect (fun xs ->
       [Observable.map (fun x ->  x      / 2) xs;
        Observable.map (fun x -> (x + 1) / 2) xs])
  let join xss =
    xss
    |> Seq.pairwise
    |> Seq.map (fun (xs, ys) ->
       Observable.zipWith (+) xs ys)

  let ints () = Observable.generate 0 (fun _ -> true) ((+) 1) id

  module Pyramid =
    let rec pyramid n =
      if n <= 0
      then ints ()
      else let xs = pyramid (n-1)
           let ys = pyramid (n-1)
           Observable.zipWith (+) xs ys

    let run m n =
      let timer = Stopwatch.StartNew ()
      let result = ref -1
      (pyramid m
       |> Observable.skip n).SubscribeOn(System.Reactive.Concurrency.ThreadPoolScheduler.Instance)
      |> Observable.subscribe (fun x ->
         lock result <| fun () ->
           result := x
           Monitor.Pulse result) |> ignore
      lock result <| fun () ->
        while !result < 0 do
          Monitor.Wait result |> ignore
      let stop = timer.Elapsed
      printfn "r pyramid %d %d = %s, took %A" m n ((!result).ToString ()) stop

  module Diamond =
    let rec repeat n f xs = if n = 0 then xs else repeat (n-1) f (f xs)
    let diamond n =
      Seq.singleton (ints ())
      |> repeat n split
      |> repeat n join
      |> Seq.head

    let run m n =
      let timer = Stopwatch.StartNew ()
      let result = ref -1
      (diamond m
       |> Observable.skip n
       |> Observable.take 1).SubscribeOn(System.Reactive.Concurrency.ThreadPoolScheduler.Instance)
      |> Observable.subscribe (fun x ->
         lock result <| fun () ->
           result := x
           Monitor.Pulse result) |> ignore
      lock result <| fun () ->
        while !result < 0 do
          Monitor.Wait result |> ignore
      let stop = timer.Elapsed
      printfn "r diamond %d %d = %s, took %A" m n ((!result).ToString ()) stop

// 0 1 2 3 4  5  6
// 1 2 4 8 16 32 64

do for m in [0; 1; 2; 3; 4; 5; 6] do
     for n in [10; 10000; 20000; 80000; 160000; 320000] do
       for f in [Streams.Pyramid.run; Rx.Pyramid.run] do
         f m n
         GC.Collect () ; System.Threading.Thread.Sleep 100
         GC.Collect () ; System.Threading.Thread.Sleep 100
       printfn ""

//do for n in [10L; 100L; 1000L; 10000L; 100000L] do
//     StreamFib.run n
