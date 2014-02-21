// Copyright (C) by Housemarque, Inc.

module CounterActor

open Hopac
open Hopac.Extensions
open Hopac.Job.Infixes
open System
open System.Diagnostics

module Native =
  open System.Threading.Tasks

  type CounterActor () =
    let lockObj = obj ()
    let mutable count = 0L
    member self.Add n : unit =
      lock lockObj <| fun () ->
      count <- count + n
    member self.GetAndReset () : int64 =
      lock lockObj <| fun () ->
      let result = count
      count <- 0L
      result

  let run title n =
    printf "%s: " title
    let actor = new CounterActor ()
    let timer = Stopwatch.StartNew ()
    let r =
      Parallel.For (0, n, fun i -> actor.Add 100L) |> ignore
      actor.GetAndReset ()
    let d = timer.Elapsed
    printf "%f ops/s - %d ops - result is %A\n" (float n / d.TotalSeconds) n r

module Hopac =
  type CounterActor () =
    let mutable count = 0L
    let lock = Lock.Now.create ()
    member self.Add n : Job<unit> =
      Lock.duringFun lock <| fun () -> count <- count + n
    member self.GetAndReset : Job<int64> =
      Lock.duringFun lock <| fun () ->
      let result = count
      count <- 0L
      result

  let run title n m =
    printf "%s: " title
    let timer = Stopwatch.StartNew ()
    let actor = CounterActor ()
    let r =
      Job.Now.run <| job {
        let! promises =
          seq {1 .. m}
          |> Seq.Parallel.mapJ (fun _ ->
             Promise.start <| job {
               do! Job.forN n (actor.Add 100L)
               return! actor.GetAndReset
             })
        return! promises |> Seq.Parallel.mapJ Promise.read
      }
    let d = timer.Elapsed
    printf "%f ops/s - %d*%d ops - result is %A\n" (float (n*m) / d.TotalSeconds) n m r

do Native.run "native" 30000
   GC.Collect ()
   System.Threading.Thread.Sleep (TimeSpan.FromSeconds 5.0)
   Native.run "native" 30000000
   GC.Collect ()
   System.Threading.Thread.Sleep (TimeSpan.FromSeconds 5.0)
   Hopac.run "warmup" 10000 Environment.ProcessorCount
   GC.Collect ()
   System.Threading.Thread.Sleep (TimeSpan.FromSeconds 5.0)
   Hopac.run "benchm" 30000000 Environment.ProcessorCount
