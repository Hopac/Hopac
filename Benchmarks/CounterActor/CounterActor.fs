// Copyright (C) by Housemarque, Inc.

module CounterActor

// Inspired by: http://zbray.com/2012/12/09/building-an-actor-in-f-with-higher-throughput-than-akka-and-erlang-actors/

// Note that within Hopac there are many ways to implement something similar
// to the kind of actor described in that blog post.

open System
open System.Diagnostics
open Hopac
open Hopac.Extensions
open Hopac.Job.Infixes

module ChMsg =
  type Msg =
   | Add of int64
   | GetAndReset of IVar<int64>

  type CounterActor =
   | CA of Ch<Msg>

  let create : Job<CounterActor> = job {
    let inCh = Ch.Now.create ()
    let state = ref 0L
    do! Job.start
         (Job.forever
           (Ch.take inCh >>= function
             | Add n ->
               state := !state + n
               Job.unit
             | GetAndReset replyVar ->
               let was = !state
               state := 0L
               IVar.fill replyVar was))
    return (CA inCh)
  }

  let add (CA inCh) (n: int64) : Job<unit> = Ch.give inCh (Add n)
  let getAndReset (CA inCh) : Job<int64> = job {
    let replyVar = IVar.Now.create ()
    do! Ch.give inCh (GetAndReset replyVar)
    return! IVar.read replyVar
  }

let run numPerThread =
  printf "ChMsg: "
  let timer = Stopwatch.StartNew ()
  let r = run <| job {
    let! actor = ChMsg.create
    do! seq {1 .. Environment.ProcessorCount}
        |> Seq.Parallel.iterJob
            (fun _ -> Job.forN numPerThread (ChMsg.add actor 100L))
    return! ChMsg.getAndReset actor
  }
  let d = timer.Elapsed
  printf "%d * %d msgs => %f msgs/s\n"
   Environment.ProcessorCount numPerThread
   (float (Environment.ProcessorCount * numPerThread) / d.TotalSeconds)

let cleanup () =
  for i=1 to 10 do
    GC.Collect () ; Threading.Thread.Sleep 50
 
do run 300 ; cleanup ()
   run 3000 ; cleanup ()
   run 30000 ; cleanup ()
   run 300000 ; cleanup ()
   run 3000000 ; cleanup ()
   run 30000000
