// Copyright (C) by Housemarque, Inc.

module Fibonacci

/////////////////////////////////////////////////////////////////////////

let mutable n = 38L

/////////////////////////////////////////////////////////////////////////

open Hopac
open Hopac.Job.Infixes
open System
open System.IO
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
    let timer = Stopwatch.StartNew ()
    let r = fib n
    let d = timer.Elapsed
    let m = sprintf "SerFun: %d - %fs (%d recs)\n" r d.TotalSeconds numSpawns
    printf "%s" m

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
    let timer = Stopwatch.StartNew ()
    let r = run (fib n)
    let d = timer.Elapsed
    let m = sprintf "SerJob: %d - %fs\n" r d.TotalSeconds
    printf "%s" m

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
    let timer = Stopwatch.StartNew ()
    let r = run (fib n)
    let d = timer.Elapsed
    let m = sprintf "SerOpt: %d - %fs\n" r d.TotalSeconds
    printf "%s" m

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
    let timer = Stopwatch.StartNew ()
    let r = run (fib n)
    let d = timer.Elapsed
    let m = sprintf "ParJob: %d - %fs (%f jobs/s)\n"
             r d.TotalSeconds (float numSpawns / d.TotalSeconds)
    do use w = new StreamWriter ("Results.txt", true)
       w.Write m
    printf "%s" m

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
    let timer = Stopwatch.StartNew ()
    let r = run (fib n)
    let d = timer.Elapsed
    let m = sprintf "ParOpt: %d - %fs (%f jobs/s)\n"
             r d.TotalSeconds (float numSpawns / d.TotalSeconds)
    do use w = new StreamWriter ("Results.txt", true)
       w.Write m
    printf "%s" m

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
    let timer = Stopwatch.StartNew ()
    let r = Async.RunSynchronously (fib n)
    let d = timer.Elapsed
    let m = sprintf "SerAsc: %d - %fs\n" r d.TotalSeconds
    printf "%s" m

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
    let timer = Stopwatch.StartNew ()
    let r = Async.RunSynchronously (fib n)
    let d = timer.Elapsed
    let m = sprintf "ParAsc: %d - %fs\n" r d.TotalSeconds
    printf "%s" m

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
    let timer = Stopwatch.StartNew ()
    let r = fib n
    let d = timer.Elapsed
    let m =
      sprintf "ParTsk: %d - %fs (%f tasks/s)\n"
       r d.TotalSeconds (float numSpawns / d.TotalSeconds)
    printf "%s" m

/////////////////////////////////////////////////////////////////////////

do printf "Running SerFun: "
   GC.Collect () ; SerialFun.run ()
   
   System.Threading.Thread.Sleep (System.TimeSpan.FromSeconds 1.0)
   printf "Running ParOpt: "
   GC.Collect () ; ParallelOpt.run ()
   
   System.Threading.Thread.Sleep (System.TimeSpan.FromSeconds 1.0)
   printf "Running ParJob: "
   GC.Collect () ; ParallelJob.run ()

   System.Threading.Thread.Sleep (System.TimeSpan.FromSeconds 1.0)
   printf "Running SerOpt: "
   GC.Collect () ; SerialOpt.run ()
   
   System.Threading.Thread.Sleep (System.TimeSpan.FromSeconds 1.0)
   printf "Running SerJob: "
   GC.Collect () ; SerialJob.run ()
   
   System.Threading.Thread.Sleep (System.TimeSpan.FromSeconds 1.0)
   printf "Running SerAsc: "
   GC.Collect () ; SerAsc.run ()
   
   System.Threading.Thread.Sleep (System.TimeSpan.FromSeconds 1.0)
   printf "Running ParTsk: "
   GC.Collect () ; Task.run ()
   
   n <- 30L

   System.Threading.Thread.Sleep (System.TimeSpan.FromSeconds 1.0)
   printf "Running ParAsc (WITH MUCH SMALLER WORKLOAD): "
   GC.Collect () ; ParAsc.run ()
