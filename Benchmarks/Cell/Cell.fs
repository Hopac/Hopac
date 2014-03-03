module Cell

open Hopac
open Hopac.Job.Infixes
open Hopac.Alt.Infixes
open Hopac.Extensions
open System
open System.Diagnostics

module HopacReq =
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

  let cell (x: 'a) : Job<Cell<'a>> = Job.delay <| fun () ->
    let c = {reqCh = Ch.Now.create (); replyCh = Ch.Now.create ()}
    Job.start
     (Job.iterate x (fun x ->
       Ch.take c.reqCh >>= function
        | Get   -> Ch.give c.replyCh x >>% x
        | Put x -> Job.result x)) >>% c

  let run nCells nJobs nUpdates =
    printf "HopacReq: "
    let timer = Stopwatch.StartNew ()
    let cells = Array.zeroCreate nCells
    let before = GC.GetTotalMemory true
    run <| job {
      do! Job.forUpTo 0 (nCells-1) <| fun i ->
            cell i |>> fun cell -> cells.[i] <- cell
      do printf "%4d b/c " (max 0L (GC.GetTotalMemory true - before) / int64 nCells)
      return!
        seq {1 .. nJobs}
        |> Seq.map (fun _ ->
           let rnd = Random ()
           Job.forUpTo 1 nUpdates <| fun _ ->
             let c = rnd.Next (0, nCells)
             get cells.[c] >>= fun x ->
             put cells.[c] (x+1))
        |> Job.parIgnore
    }
    let d = timer.Elapsed
    printf "%8.5f s to %d c * %d p * %d u\n"
     d.TotalSeconds nCells nJobs nUpdates

module HopacAlt =
  type Cell<'a> = {
    getCh: Ch<'a>
    putCh: Ch<'a>
  }

  let get (c: Cell<'a>) : Job<'a> = Ch.take c.getCh
  let put (c: Cell<'a>) (x: 'a) : Job<unit> = Ch.give c.putCh x

  let cell (x: 'a) : Job<Cell<'a>> = Job.delay <| fun () ->
    let c = {getCh = Ch.Now.create (); putCh = Ch.Now.create ()}
    Job.start
     (Job.iterate x (fun x ->
       Alt.pick ((Ch.Alt.take c.putCh)
             <|> (Ch.Alt.give c.getCh x >-> fun () -> x)))) >>% c

  let run nCells nJobs nUpdates =
    printf "HopacAlt: "
    let timer = Stopwatch.StartNew ()
    let cells = Array.zeroCreate nCells
    let before = GC.GetTotalMemory true
    run <| job {
      do! Job.forUpTo 0 (nCells-1) <| fun i ->
            cell i |>> fun cell -> cells.[i] <- cell
      do printf "%4d b/c " (max 0L (GC.GetTotalMemory true - before) / int64 nCells)
      return!
        seq {1 .. nJobs}
        |> Seq.map (fun _ ->
           let rnd = Random ()
           Job.forUpTo 1 nUpdates <| fun _ ->
             let c = rnd.Next (0, nCells)
             get cells.[c] >>= fun x ->
             put cells.[c] (x+1))
        |> Job.parIgnore
    }
    let d = timer.Elapsed
    printf "%8.5f s to %d c * %d p * %d u\n"
     d.TotalSeconds nCells nJobs nUpdates

module AsyncCell =
  type Request<'a> =
   | Get of AsyncReplyChannel<'a>
   | Put of 'a

  type Cell<'a> = MailboxProcessor<Request<'a>>

  let cell (x: 'a) : Cell<'a> = MailboxProcessor.Start <| fun inbox ->
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
    printf "Async:    "
    let timer = Stopwatch.StartNew ()
    let cells = Array.zeroCreate nCells
    let before = GC.GetTotalMemory true
    for i=0 to nCells-1 do
      cells.[i] <- cell i
    printf "%4d b/c " (max 0L (GC.GetTotalMemory true - before) / int64 nCells)
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
    printf "%8.5f s to %d c * %d p * %d u\n"
     d.TotalSeconds nCells nJobs nUpdates

let tick () =
  for i=1 to 10 do
    Runtime.GCSettings.LargeObjectHeapCompactionMode <- Runtime.GCLargeObjectHeapCompactionMode.CompactOnce
    GC.Collect ()
    Threading.Thread.Sleep 50

let test m n p =
  HopacReq.run m n p ; tick ()
  HopacAlt.run m n p ; tick ()
  AsyncCell.run m n p ; tick ()

do tick ()
   test 10 10 10
   test 100 100 100
   test 1000 1000 1000
   test 10 10 1000000
   test 1000 1000 10000
   test 10000 1000 1000
   test 1000 10000 1000
   test 10000 10000 1000
   test 1000000 100000 100
   test 10000 10000 10000
