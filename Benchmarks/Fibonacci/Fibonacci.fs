// Copyright (C) by Housemarque, Inc.

module Fibonacci

/////////////////////////////////////////////////////////////////////////

let mutable n = 38L

/////////////////////////////////////////////////////////////////////////

open Hopac
open Hopac.Job.Infixes
open System
open System.Threading
open System.Threading.Tasks
open System.Diagnostics

/////////////////////////////////////////////////////////////////////////

let mutable numSpawns = 0L

module SerialFun =
  let rec fib n =
    if n < 2L then
      n
    else
      numSpawns <- numSpawns + 1L
      fib (n-2L) + fib (n-1L)

  let run () =
    printf "SerFun: "
    let timer = Stopwatch.StartNew ()
    let r = fib n
    let d = timer.Elapsed
    printf "%d - %fs (%d recs)\n" r d.TotalSeconds numSpawns

/////////////////////////////////////////////////////////////////////////

module SerialJob =
  let rec fib n = job {
    if n < 2L then
      return n
    else
      let! x = fib (n-2L)
      let! y = fib (n-1L)
      return x + y
  }

  let run () =
    printf "SerJob: "
    let timer = Stopwatch.StartNew ()
    let r = run (fib n)
    let d = timer.Elapsed
    printf "%d - %fs\n" r d.TotalSeconds

/////////////////////////////////////////////////////////////////////////

module SerialOpt =
  let rec fib n =
    if n < 2L then
      Job.result n
    else
      fib (n-2L) >>= fun x ->
      fib (n-1L) |>> fun y ->
      x + y

  let run () =
    printf "SerOpt: "
    let timer = Stopwatch.StartNew ()
    let r = run (fib n)
    let d = timer.Elapsed
    printf "%d - %fs\n" r d.TotalSeconds

/////////////////////////////////////////////////////////////////////////

module ParallelJob =
  let rec fib n = job {
    if n < 2L then
      return n
    else
      let! (x, y) = fib (n-2L) <*> fib (n-1L)
      return x + y
  }

  let run () =
    printf "ParJob: "
    let timer = Stopwatch.StartNew ()
    let r = run (fib n)
    let d = timer.Elapsed
    printf "%d - %fs (%f jobs/s)\n"
     r d.TotalSeconds (float numSpawns / d.TotalSeconds)

/////////////////////////////////////////////////////////////////////////

module ParallelOpt =
  let rec fib n =
    if n < 2L then
      Job.result n
    else
      Job.delay <| fun () ->
      fib (n-2L) <*> fib (n-1L) |>> fun (x, y) ->
      x + y

  let run () =
    printf "ParOpt: "
    let timer = Stopwatch.StartNew ()
    let r = run (fib n)
    let d = timer.Elapsed
    printf "%d - %fs (%f jobs/s)\n"
     r d.TotalSeconds (float numSpawns / d.TotalSeconds)

/////////////////////////////////////////////////////////////////////////

module SerAsc =
  let rec fib n = async {
    if n < 2L then
      return n
    else
      let! x = fib (n-2L)
      let! y = fib (n-1L)
      return x + y
  }

  let run () =
    printf "SerAsc: "
    let timer = Stopwatch.StartNew ()
    let r = Async.RunSynchronously (fib n)
    let d = timer.Elapsed
    printf "%d - %fs\n" r d.TotalSeconds

/////////////////////////////////////////////////////////////////////////

module ParAsc =
  let rec fib n = async {
    if n < 2L then
      return n
    else
//#if NO_STARTCHILD_LEAK
      let! x = Async.StartChild (fib (n-2L))
//#else
//      let x = Async.StartAsTask (fib (n-2L))
//      let x = Async.AwaitTask (x)
//#endif
      let! y = fib (n-1L)
      let! x = x
      return x + y
  }

  let run () =
    printf "ParAsc: "
    let timer = Stopwatch.StartNew ()
    let r = Async.RunSynchronously (fib n)
    let d = timer.Elapsed
    printf "%d - %fs\n" r d.TotalSeconds

/////////////////////////////////////////////////////////////////////////

module Task =
  let rec fib n =
    if n < 2L then
      n
    else
      let x = Task.Factory.StartNew (fun _ -> fib (n-2L))
      let y = fib (n-1L)
      x.Result + y

  let run () =
    printf "ParTsk: "
    let timer = Stopwatch.StartNew ()
    let r = fib n
    let d = timer.Elapsed
    printf "%d - %fs (%f tasks/s)\n"
     r d.TotalSeconds (float numSpawns / d.TotalSeconds)

/////////////////////////////////////////////////////////////////////////

let cleanup () =
  for i=1 to 10 do
    GC.Collect ()
    Threading.Thread.Sleep 50

do SerialFun.run () ; cleanup ()
   ParallelOpt.run () ; cleanup ()
   ParallelJob.run () ; cleanup ()
   SerialOpt.run () ; cleanup ()
   SerialJob.run () ; cleanup ()
   SerAsc.run () ; cleanup ()
   Task.run () ; cleanup ()
   
   n <- 30L

   ParAsc.run ()
