// Copyright (C) by Housemarque, Inc.

module AsyncOverhead

open Hopac
open Hopac.Infixes
open Hopac.Extensions
open System
open System.Diagnostics
open System.Threading.Tasks

let inline (^) x = x

do let doAsTasks n =
     printf "Jobs-started-as-Tasks: "
     let timer = Stopwatch.StartNew ()
     run ^ job {
       for i=1 to n do
         do ignore i
         do! Hopac.startAsTask ^ job { return () }
     }
     let d = timer.Elapsed
     printfn "%d hops in %A" n d
   for n in [100; 1000; 10000; 100000; 1000000; 10000000] do
     doAsTasks n
     GC.Collect ()
     System.Threading.Thread.Sleep 100

do let doAsTasks n =
     printf "Jobs-queued-as-Tasks:  "
     let timer = Stopwatch.StartNew ()
     run ^ job {
       for i=1 to n do
         do ignore i
         do! Hopac.queueAsTask ^ job { return () }
     }
     let d = timer.Elapsed
     printfn "%d hops in %A" n d
   for n in [100; 1000; 10000; 100000; 1000000; 10000000] do
     doAsTasks n
     GC.Collect ()
     System.Threading.Thread.Sleep 100

do let doAltAsyncBinds n =
     printf "Alt-in-Async binds: "
     let timer = Stopwatch.StartNew ()
     Async.RunSynchronously ^ async {
       do! Async.SwitchToThreadPool ()
       for i=1 to n do
         do ignore i
         do! Alt.toAsync ^ Alt.unit ()
     }
     let d = timer.Elapsed
     printfn "%d hops in %A" n d

   for n in [100; 1000; 10000; 100000; 1000000; 10000000] do
     doAltAsyncBinds n
     GC.Collect ()
     System.Threading.Thread.Sleep 100

do let doAsyncAltBinds n =
     printf "Async-as-Alt-in-Job binds: "
     let timer = Stopwatch.StartNew ()
     let dI = IVar ()
     run ^ job {
       do! Job.Scheduler.switchToWorker ()
       for i=1 to n do
         do ignore i
         do! dI <|> Alt.fromAsync ^ async.Zero ()
     }
     let d = timer.Elapsed
     printfn "%d hops in %A" n d

   for n in [100; 1000; 10000; 100000; 1000000; 10000000] do
     doAsyncAltBinds n
     GC.Collect ()
     System.Threading.Thread.Sleep 100

do let doTaskAltBinds n =
     printf "Task-as-Alt-in-Job binds: "
     let timer = Stopwatch.StartNew ()
     let dI = IVar ()
     run ^ job {
       do! Job.Scheduler.switchToWorker ()
       for i=1 to n do
         do ignore i
         do! dI <|> Alt.fromTask ^ fun _ -> Task.FromResult ()
     }
     let d = timer.Elapsed
     printfn "%d hops in %A" n d

   for n in [100; 1000; 10000; 100000; 1000000; 10000000] do
     doTaskAltBinds n
     GC.Collect ()
     System.Threading.Thread.Sleep 100

do let doJobAsyncBinds n =
     printf "Job-in-Async binds: "
     let timer = Stopwatch.StartNew ()
     Async.RunSynchronously ^ async {
       do! Async.SwitchToThreadPool ()
       for i=1 to n do
         do ignore i
         do! Job.toAsync ^ job.Zero ()
     }
     let d = timer.Elapsed
     printfn "%d hops in %A" n d

   for n in [100; 1000; 10000; 100000; 1000000; 10000000] do
     doJobAsyncBinds n
     GC.Collect ()
     System.Threading.Thread.Sleep 100

do let doAsyncJobBinds n =
     printf "Async-in-Job binds: "
     let timer = Stopwatch.StartNew ()
     run ^ job {
       do! Job.Scheduler.switchToWorker ()
       for i=1 to n do
         do ignore i
         do! Job.fromAsync ^ async.Zero ()
     }
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
