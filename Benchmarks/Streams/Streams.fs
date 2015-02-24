// Copyright (C) by Vesa Karvonen

module Streams

open System.Reactive.Linq
open System.Reactive.Concurrency
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
       [Stream.mapFun (fun x ->  x      / 2) xs
        Stream.mapFun (fun x -> (x + 1) / 2) xs])
  let join xss =
    xss
    |> Seq.pairwise
    |> Seq.map (fun (xs, ys) ->
       Stream.zipWithFun (+) xs ys)

  let ints _ = Stream.iterateFun (fun x -> x+1) 0

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
        |> Stream.skip (int64 n)
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
        |> Stream.skip (int64 n)
        |> Stream.get
        |> run
      let stop = timer.Elapsed
      printfn "s diamond %d %d = %s, took %A" m n (result.ToString ()) stop

module Rx =
  let split xss =
    xss
    |> Seq.collect (fun (xs: IObservable<_>) ->
       [xs.Select(fun x ->  x      / 2)
        xs.Select(fun x -> (x + 1) / 2)])
  let join xss =
    xss
    |> Seq.pairwise
    |> Seq.map (fun (xs: IObservable<_>, ys: IObservable<_>) ->
       Observable.Zip(xs, ys, fun x y -> x+y))

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
         lock result <| fun () ->
           result := x
           Monitor.Pulse result) |> ignore
      lock result <| fun () ->
        while !result < 0 do
          Monitor.Wait result |> ignore
      let stop = timer.Elapsed
      printfn "r diamond %d %d = %s, took %A" m n ((!result).ToString ()) stop

do for m in [0; 1; 4; 6] do
     for n in [10; 160000; 320000] do
       for f in [Streams.Pyramid.run; Rx.Pyramid.run;
                 Streams.Diamond.run; Rx.Diamond.run] do
         f m n
         GC.Collect () ; System.Threading.Thread.Sleep 100
         GC.Collect () ; System.Threading.Thread.Sleep 100
       printfn ""

//do for n in [10L; 100L; 1000L; 10000L; 100000L] do
//     StreamFib.run n
