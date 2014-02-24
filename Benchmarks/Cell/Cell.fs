module Cell

open Hopac
open Hopac.Job.Infixes
open Hopac.Extensions
open System
open System.IO
open System.Diagnostics
open System.Threading.Tasks

module HopacCell =
  type Request<'a> =
   | Get
   | Put of 'a

  type Cell<'a> = {
    reqCh: Ch<Request<'a>>
    replyCh: Ch<'a>
  }

  let put (c: Cell<'a>) (x: 'a) : Job<unit> =
    Ch.give c.reqCh (Put x)

  let get (c: Cell<'a>) : Job<'a> = Ch.give c.reqCh Get >>. Ch.take c.replyCh

  let create (x: 'a) : Job<Cell<'a>> = Job.delay <| fun () ->
    let reqCh = Ch.Now.create ()
    let replyCh = Ch.Now.create ()
    let rec server x =
      Ch.take reqCh >>= function
       | Get ->
         Ch.give replyCh x >>= fun () ->
         server x
       | Put x ->
         server x
    Job.start (server x) >>% {reqCh = reqCh; replyCh = replyCh}

  let run nCells nJobs nUpdates =
    printf "Hopac: "
    let timer = Stopwatch.StartNew ()
    Job.Now.run <| job {
      let cells = Array.zeroCreate nCells
      for i=0 to nCells-1 do
        let! cell = create i
        do cells.[i] <- cell
      do printf "%d bytes " (GC.GetTotalMemory true)
      let! jobs =
        seq {1 .. nJobs}
        |> Seq.mapJ (fun _ ->
           let rnd = Random ()
           Promise.start
            (Job.forUpTo 1 nUpdates (fun _ ->
               let c = rnd.Next (0, nCells)
               get cells.[c] >>= fun x ->
               put cells.[c] (x+1))))
      do! Seq.iterJ Promise.read jobs
    }
    let d = timer.Elapsed
    let m = sprintf "%f seconds to %d cells * %d parallel jobs * %d updates\n"
             d.TotalSeconds nCells nJobs nUpdates
    printf "%s" m

module AsyncCell =
  type Request<'a> =
   | Get of AsyncReplyChannel<'a>
   | Put of 'a

  type Cell<'a> = MailboxProcessor<Request<'a>>

  let create (x: 'a) : Cell<'a> = MailboxProcessor.Start <| fun inbox ->
    let rec server x = async {
      let! req = inbox.Receive ()
      match req with
       | Get reply ->
         do reply.Reply x
         return! server x
       | Put x ->
         return! server x
    }
    server x

  let put (c: Cell<'a>) (x: 'a) : unit =
    c.Post (Put x)

  let get (c: Cell<'a>) : Async<'a> = c.PostAndAsyncReply Get

  let run nCells nJobs nUpdates =
    printf "Async: "
    let timer = Stopwatch.StartNew ()
    let cells = Array.zeroCreate nCells
    for i=0 to nCells-1 do
      cells.[i] <- create i
    printf "%d bytes " (GC.GetTotalMemory true)
    seq {1 .. nJobs}
    |> Seq.map (fun _ -> async {
       let rnd = Random ()
       for i=0 to nUpdates do
         let c = rnd.Next (0, nCells)
         let! x = get cells.[c]
         do put cells.[c] (x+1)
       })
    |> Async.Parallel
    |> Async.RunSynchronously
    |> ignore
    for i=0 to nCells-1 do
      (cells.[i] :> IDisposable).Dispose ()
    let d = timer.Elapsed
    let m = sprintf "%f seconds to %d cells * %d parallel asyncs * %d updates\n"
             d.TotalSeconds nCells nJobs nUpdates
    printf "%s" m

let tick () =
  GC.Collect ()
  Threading.Thread.Sleep 500
  GC.Collect ()
  Threading.Thread.Sleep 500

let test m n p =
  HopacCell.run m n p ; tick ()
  AsyncCell.run m n p ; tick ()

do test 10 10 10
   test 100 100 100
   test 1000 1000 1000
   test 1000 1000 10000
   test 10000 1000 1000
   test 1000 10000 1000
   test 10000 10000 1000
   test 1000000 100000 10
   test 10000 10000 10000
