// Copyright (C) by Housemarque, Inc.

module AsyncOverhead

open Hopac
open Hopac.Infixes
open Hopac.Extensions
open System
open System.Diagnostics
open System.Threading.Tasks

let inline (^) x = x

do let doTaskAltBinds n =
     let timer = Stopwatch.StartNew ()
     printf "Task-as-Alt-in-Job binds: "
     let dI = IVar ()
     job {
       do! Job.Scheduler.switchToWorker ()
       for i=1 to n do
         do ignore i
         do! dI <|> Alt.fromCancellableTask ^ fun _ -> Task.FromResult ()
     }
     |> run
     let d = timer.Elapsed
     printfn "%d hops in %A" n d

   for n in [100; 1000; 10000; 100000; 1000000; 10000000] do
     doTaskAltBinds n
     GC.Collect ()
     System.Threading.Thread.Sleep 100

do let doAsyncAltBinds n =
     let timer = Stopwatch.StartNew ()
     printf "Async-as-Alt-in-Job binds: "
     let dI = IVar ()
     job {
       do! Job.Scheduler.switchToWorker ()
       for i=1 to n do
         do ignore i
         do! dI <|> Alt.fromAsync ^ async { return () }
     }
     |> run
     let d = timer.Elapsed
     printfn "%d hops in %A" n d

   for n in [100; 1000; 10000; 100000; 1000000; 10000000] do
     doAsyncAltBinds n
     GC.Collect ()
     System.Threading.Thread.Sleep 100

do let doJobAsyncBinds n =
     let timer = Stopwatch.StartNew ()
     printf "Job-in-Async binds: "
     async {
       do! Async.SwitchToThreadPool ()
       for i=1 to n do
         do ignore i
         do! Async.Global.ofJob ^ job { return () }
     }
     |> Async.RunSynchronously
     let d = timer.Elapsed
     printfn "%d hops in %A" n d

   for n in [100; 1000; 10000; 100000; 1000000; 10000000] do
     doJobAsyncBinds n
     GC.Collect ()
     System.Threading.Thread.Sleep 100

do let doAsyncJobBinds n =
     let timer = Stopwatch.StartNew ()
     printf "Async-in-Job binds: "
     job {
       do! Job.Scheduler.switchToWorker ()
       for i=1 to n do
         do ignore i
         do! Job.fromAsync ^ async { return () }
     }
     |> run
     let d = timer.Elapsed
     printfn "%d hops in %A" n d

   for n in [100; 1000; 10000; 100000; 1000000; 10000000] do
     doAsyncJobBinds n
     GC.Collect ()
     System.Threading.Thread.Sleep 100

//

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
