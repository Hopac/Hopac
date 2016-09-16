module Cell

open Hopac
open Hopac.Infixes
open Hopac.Extensions
open System
open System.Diagnostics

module HopacMVar =
  type Cell<'a> = C of MVar<'a>

  let get (C c) = MVar.read c
  let put (C c) x = MVar.mutateFun (fun _ -> x) c

  let cell x = C (MVar x)

  let run nCells nJobs nUpdates =
    printf "HopacMVar: "
    let timer = Stopwatch.StartNew ()
    let cells = Array.zeroCreate nCells
    let before = GC.GetTotalMemory true
    run <| job {
      for i=0 to nCells-1 do
        cells.[i] <- cell i
      do printf "%4d b/c "
           (max 0L (GC.GetTotalMemory true - before) / int64 nCells)
      return!
        seq {1 .. nJobs}
        |> Seq.map (fun _ ->
           let rnd = Random ()
           Job.forUpTo 1 nUpdates <| fun _ ->
             let c = rnd.Next (0, nCells)
             get cells.[c] >>= fun x ->
             put cells.[c] (x+1))
        |> Job.conIgnore
    }
    let d = timer.Elapsed
    for i=0 to nCells-1 do
      cells.[i] <- Unchecked.defaultof<_>
    printf "%8.5f s to %d c * %d p * %d u\n"
     d.TotalSeconds nCells nJobs nUpdates

module HopacReq =
  type Request<'a> =
   | Get
   | Put of 'a

  type Cell<'a> = {
    reqCh: Ch<Request<'a>>
    replyCh: Ch<'a>
  }

  let put (c: Cell<'a>) (x: 'a) =
    c.reqCh *<- Put x

  let get (c: Cell<'a>) : Job<'a> = c.reqCh *<+ Get >>=. c.replyCh

  let cell (x: 'a) : Job<Cell<'a>> = Job.delay <| fun () ->
    let c = {reqCh = Ch (); replyCh = Ch ()}
    Job.iterateServer x <| fun x ->
          c.reqCh >>= function
           | Get   -> c.replyCh *<+ x >>-. x // XXX
           | Put x -> Job.result x
    >>-. c

  let run nCells nJobs nUpdates =
    printf "HopacReq:  "
    let timer = Stopwatch.StartNew ()
    let cells = Array.zeroCreate nCells
    let before = GC.GetTotalMemory true
    run <| job {
      do! Job.forUpTo 0 (nCells-1) <| fun i ->
            cell i >>- fun cell -> cells.[i] <- cell
      do printf "%4d b/c "
           (max 0L (GC.GetTotalMemory true - before) / int64 nCells)
      return!
        seq {1 .. nJobs}
        |> Seq.map (fun _ ->
           let rnd = Random ()
           Job.forUpTo 1 nUpdates <| fun _ ->
             let c = rnd.Next (0, nCells)
             get cells.[c] >>= fun x ->
             put cells.[c] (x+1))
        |> Job.conIgnore
    }
    let d = timer.Elapsed
    for i=0 to nCells-1 do
      cells.[i] <- Unchecked.defaultof<_>
    printf "%8.5f s to %d c * %d p * %d u\n"
     d.TotalSeconds nCells nJobs nUpdates

module HopacDyn =
  type Request<'a> =
   | Get of IVar<'a>
   | Put of 'a

  type Cell<'a> = {
    reqCh: Ch<Request<'a>>
  }

  let put (c: Cell<'a>) (x: 'a) =
    c.reqCh *<- Put x

  let get (c: Cell<'a>) =
    c.reqCh *<+=>- Get :> Job<_>

  let cell (x: 'a) : Job<Cell<'a>> = Job.delay <| fun () ->
    let reqCh = Ch ()
    Job.iterateServer x <| fun x ->
          reqCh >>= function
           | Get replyIv -> replyIv *<= x >>-. x
           | Put x -> Job.result x
    >>-. {reqCh = reqCh}

  let run nCells nJobs nUpdates =
    printf "HopacDyn:  "
    let timer = Stopwatch.StartNew ()
    let cells = Array.zeroCreate nCells
    let before = GC.GetTotalMemory true
    run <| job {
      do! Job.forUpTo 0 (nCells-1) <| fun i ->
            cell i >>- fun cell -> cells.[i] <- cell
      do printf "%4d b/c "
           (max 0L (GC.GetTotalMemory true - before) / int64 nCells)
      return!
        seq {1 .. nJobs}
        |> Seq.map (fun _ ->
           let rnd = Random ()
           Job.forUpTo 1 nUpdates <| fun _ ->
             let c = rnd.Next (0, nCells)
             get cells.[c] >>= fun x ->
             put cells.[c] (x+1))
        |> Job.conIgnore
    }
    for i=0 to nCells-1 do
      cells.[i] <- Unchecked.defaultof<_>
    let d = timer.Elapsed
    printf "%8.5f s to %d c * %d p * %d u\n"
     d.TotalSeconds nCells nJobs nUpdates

module HopacAlt =
  type Cell<'a> = {
    getCh: Ch<'a>
    putCh: Ch<'a>
  }

  let get (c: Cell<'a>) = c.getCh :> Alt<_>
  let put (c: Cell<'a>) (x: 'a) = c.putCh *<- x

  let cell (x: 'a) : Job<Cell<'a>> = Job.delay <| fun () ->
    let c = {getCh = Ch (); putCh = Ch ()}
    Job.iterateServer x <| fun x ->
          c.putCh <|> c.getCh *<- x ^->. x
    >>-. c

  let run nCells nJobs nUpdates =
    printf "HopacAlt:  "
    let timer = Stopwatch.StartNew ()
    let cells = Array.zeroCreate nCells
    let before = GC.GetTotalMemory true
    run <| job {
      do! Job.forUpTo 0 (nCells-1) <| fun i ->
            cell i >>- fun cell -> cells.[i] <- cell
      do printf "%4d b/c "
           (max 0L (GC.GetTotalMemory true - before) / int64 nCells)
      return!
        seq {1 .. nJobs}
        |> Seq.map (fun _ ->
           let rnd = Random ()
           Job.forUpTo 1 nUpdates <| fun _ ->
             let c = rnd.Next (0, nCells)
             get cells.[c] >>= fun x ->
             put cells.[c] (x+1))
        |> Job.conIgnore
    }
    let d = timer.Elapsed
    for i=0 to nCells-1 do
      cells.[i] <- Unchecked.defaultof<_>
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
    printf "Async:     "
    let timer = Stopwatch.StartNew ()
    let cells = Array.zeroCreate nCells
    let before = GC.GetTotalMemory true
    for i=0 to nCells-1 do
      cells.[i] <- cell i
    printf "%4d b/c " (max 0L (GC.GetTotalMemory true - before) / int64 nCells)
    seq {1 .. nJobs}
    |> Seq.map (fun _ -> async {
       let rnd = Random ()
       for i=1 to nUpdates do
         let c = rnd.Next (0, nCells)
         let! x = get cells.[c]
         do ignore i; put cells.[c] (x+1)
       })
    |> Async.Parallel
    |> Async.RunSynchronously
    |> ignore
    for i=0 to nCells-1 do
      (cells.[i] :> IDisposable).Dispose ()
    let d = timer.Elapsed
    for i=0 to nCells-1 do
      cells.[i] <- Unchecked.defaultof<_>
    printf "%8.5f s to %d c * %d p * %d u\n"
     d.TotalSeconds nCells nJobs nUpdates

let tick () =
  for i=1 to 2 do
    GC.Collect ()
    GC.WaitForPendingFinalizers ()

let test doAs m n p =
  HopacMVar.run m n p ; tick ()
  HopacReq.run  m n p ; tick ()
  HopacDyn.run  m n p ; tick ()
  HopacAlt.run  m n p ; tick ()
  if doAs then AsyncCell.run m n p ; tick ()

do tick ()
   test  true 10 10 10
   test  true 100 100 100
   test  true 1000 1000 1000
   test false 100 10 100000
   test false 1000 1000 10000
   test  true 10000 1000 1000
   test false 1000 10000 1000
   test false 10000 10000 1000
   test false 1000000 100000 100
   test false 10000 10000 10000
