// Copyright (C) by Housemarque, Inc.

module AsyncOverhead

open Hopac
open Hopac.Extensions
open Hopac.Job.Infixes
open System
open System.Diagnostics
open System.Threading.Tasks

let runHopacTCS numOps n =
  printf "HopacTCS: "
  let timer = Stopwatch.StartNew ()
  let rec loop n = job {
    if 0 < n then
      let tcs = TaskCompletionSource<int> ()
      let t = tcs.Task
      do tcs.TrySetResult (n-1) |> ignore
      let! n = t
      return! loop n
  }
  run (Array.create n (loop numOps) |> Job.conIgnore)
  let d = timer.Elapsed
  printf "%8d*%-2d %8fs - %8.0f ops/s\n"
   numOps n d.TotalSeconds (float (numOps*n) / d.TotalSeconds)

let runHopac numOps n =
  printf "Hopac:    "
  let timer = Stopwatch.StartNew ()
  let rec loop n = job {
    if 0 < n then
      let! n = Task.Factory.StartNew (fun _ -> n-1)
      return! loop n
  }
  run (Array.create n (loop numOps) |> Job.conIgnore)
  let d = timer.Elapsed
  printf "%8d*%-2d %8fs - %8.0f ops/s\n"
   numOps n d.TotalSeconds (float (numOps*n) / d.TotalSeconds)

let runAsyncTCS numOps n =
  printf "AsyncTCS: "
  let timer = Stopwatch.StartNew ()
  let rec loop n = async {
    if 0 < n then
      let tcs = TaskCompletionSource<int> ()
      let t = tcs.Task
      do tcs.TrySetResult (n-1) |> ignore
      let! n = Async.AwaitTask t
      return! loop n
  }
  Array.create n (loop numOps)
  |> Async.Parallel
  |> Async.RunSynchronously |> ignore
  let d = timer.Elapsed
  printf "%8d*%-2d %8fs - %8.0f ops/s\n"
   numOps n d.TotalSeconds (float (numOps*n) / d.TotalSeconds)

let runAsync numOps n =
  printf "Async:    "
  let timer = Stopwatch.StartNew ()
  let rec loop n = async {
    if 0 < n then
      let! n = Task.Factory.StartNew (fun _ -> n-1) |> Async.AwaitTask
      return! loop n
  }
  Array.create n (loop numOps)
  |> Async.Parallel
  |> Async.RunSynchronously |> ignore
  let d = timer.Elapsed
  printf "%8d*%-2d %8fs - %8.0f ops/s\n"
   numOps n d.TotalSeconds (float (numOps*n) / d.TotalSeconds)

let inline isMono () =
  match Type.GetType "Mono.Runtime" with
   | null -> false
   | _ -> true

do let d = if isMono () then 10 else 1
   for (f, d) in [(runHopacTCS, 1); (runHopac, d); (runAsyncTCS, 1); (runAsync, d)] do
     for (numOps, n) in [(100, 1)
                         (1500000, 1)
                         (1000000, 2)
                         (1500000, 4)
                         (1000000, 8)] do
      f (numOps / d) n
      GC.Collect ()
      System.Threading.Thread.Sleep 100
