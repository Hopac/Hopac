// Copyright (C) by Housemarque, Inc.

module CounterActor

// Note that within Hopac there are many ways to implement something similar
// to the kind of actor described in that blog post.

open System
open System.Diagnostics
open System.Threading.Tasks
open Hopac
open Hopac.Infixes
open Hopac.Extensions
open Hopac.Job.Infixes

module ChMsg =
  type Msg =
   | Add of int64
   | GetAndReset of IVar<int64>

  type CounterActor =
   | CA of Ch<Msg>

  let create : Job<CounterActor> = job {
    let inCh = ch ()
    let state = ref 0L
    do! Job.foreverServer
         (inCh >>= function
           | Add n ->
             state := !state + n
             Job.unit ()
           | GetAndReset replyVar ->
             let was = !state
             state := 0L
             replyVar <-= was)
    return CA inCh
  }

  let add (CA inCh) (n: int64) = inCh <-- Add n
  let getAndReset (CA inCh) : Job<int64> = Job.delay <| fun () ->
    let replyVar = ivar ()
    inCh <-+ GetAndReset replyVar >>.
    replyVar

  let run numPerThread =
    printf "ChMsg: "
    let timer = Stopwatch.StartNew ()
    let r = run <| job {
      let! actor = create
      do! seq {1 .. Environment.ProcessorCount}
          |> Seq.Con.iterJob
              (fun _ -> Job.forN numPerThread (add actor 100L))
      return! getAndReset actor
    }
    let d = timer.Elapsed
    printf "%d * %8d msgs => %8.0f msgs/s\n"
     Environment.ProcessorCount numPerThread
     (float (Environment.ProcessorCount * numPerThread) / d.TotalSeconds)

module MbMsg =
  type Msg =
   | Add of int64
   | GetAndReset of IVar<int64>

  type CounterActor =
   | CA of Mailbox<Msg>

  let create : Job<CounterActor> = job {
    let inMb = mb ()
    let state = ref 0L
    do! Job.foreverServer
         (inMb >>= function
           | Add n ->
             state := !state + n
             Job.unit ()
           | GetAndReset replyVar ->
             let was = !state
             state := 0L
             replyVar <-= was)
    return CA inMb
  }

  let add (CA inMb) (n: int64) = Mailbox.Global.send inMb (Add n)
  let getAndReset (CA inMb) : Job<int64> = Job.delay <| fun () ->
    let replyVar = ivar ()
    inMb <<-+ GetAndReset replyVar >>.
    replyVar

  let run numPerThread =
    printf "MbMsg: "
    let timer = Stopwatch.StartNew ()
    let r = run <| job {
      let! actor = create
      do Parallel.For (0, Environment.ProcessorCount, fun _ ->
           for i=1 to numPerThread do
             add actor 100L) |> ignore
      return! getAndReset actor
    }
    let d = timer.Elapsed
    printf "%d * %8d msgs => %8.0f msgs/s\n"
     Environment.ProcessorCount numPerThread
     (float (Environment.ProcessorCount * numPerThread) / d.TotalSeconds)

let cleanup () =
  for i=1 to 5 do
    GC.Collect ()
    Threading.Thread.Sleep 50
 
do for f in [ChMsg.run; MbMsg.run] do
     for n in [300; 3000; 30000; 300000; 3000000] do
       f n
       cleanup ()
