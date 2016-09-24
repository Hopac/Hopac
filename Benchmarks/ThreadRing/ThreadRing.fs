// Copyright (C) by Housemarque, Inc.

module ThreadRing

#nowarn "40"

open Hopac
open Hopac.Bench
open Hopac.Infixes
open Hopac.Extensions
open System
open System.Diagnostics

module Native =
  open System.Threading

  let run n m p =
    printf " Native: "
    let timer = Stopwatch.StartNew ()
    let before = GC.GetTotalMemory true
    let counters = Array.zeroCreate p
    let events =
      Array.init p ^ fun i ->
      counters.[i] <- ref m
      Array.init n ^ fun _ ->
        (new AutoResetEvent (false), new AutoResetEvent (false))
    for i=0 to p-1 do
      let counter = counters.[i]
      let events = events.[i]
      for j=0 to n-1 do
        let (myEvent, meDoneEvent) = events.[j]
        let (nextEvent, _) = events.[(j+1)%n]
        let thread =
          Thread (ThreadStart (fun () ->
                    let rec loop () =
                      myEvent.WaitOne () |> ignore
                      match !counter with
                       | 0 ->
                         nextEvent.Set () |> ignore
                         myEvent.Dispose ()
                         meDoneEvent.Set () |> ignore
                       | 1 ->
                         counter := 0
                         nextEvent.Set () |> ignore
                         myEvent.WaitOne () |> ignore
                         myEvent.Dispose ()
                         meDoneEvent.Set () |> ignore
                       | n ->
                         counter := n - 1
                         nextEvent.Set () |> ignore
                         loop ()
                    loop ()))
        thread.Start ()
    printf "%5d b/c " (max 0L (GC.GetTotalMemory true - before) / int64 (p*n))
    for i=0 to p-1 do
      (fst events.[i].[0]).Set () |> ignore
    for i=0 to p-1 do
      for j=0 to n-1 do
        (snd events.[i].[j]).WaitOne () |> ignore
        snd events.[i].[j] |> dispose
    let d = timer.Elapsed
    printf "%9.0f m/s - %fs\n"
     (float (p*m) / d.TotalSeconds) d.TotalSeconds

module ChGive =
  let proc (name: int) (inCh: Ch<int>) (outCh: Ch<int>) (finishCh: Ch<int>) =
    Job.foreverServer
     (inCh >>= fun n ->
      if n <> 0 then
        outCh *<- (n-1)
      else
        finishCh *<- name)

  let mkChain finishCh n = Job.delay ^ fun () ->
    let ch0 = Ch ()
    seq {1 .. n}
    |> Seq.foldFromJob ch0 ^ fun chIn i ->
         let chOut = if i=n then ch0 else Ch ()
         proc i chIn chOut finishCh >>-. chOut

  let run n m p =
    printf " ChGive: "
    let timer = Stopwatch.StartNew ()
    let before = GC.GetTotalMemory true
    ignore ^ runDelay ^ fun () ->
      let ps = Array.create p n
      let finishCh = Ch ()
      ps
      |> Seq.Con.mapJob ^ mkChain finishCh
      >>= fun chs ->
      printf "%5d b/c " (max 0L (GC.GetTotalMemory true - before) / int64 (p*n))
      chs
      |> Seq.Con.iterJob ^ fun ch -> ch *<+ m
      >>=. Seq.Con.mapJob (fun _ -> finishCh) (seq {1 .. p})
    let d = timer.Elapsed
    printf "%9.0f m/s - %fs\n"
     (float (p*m) / d.TotalSeconds) d.TotalSeconds

module ChSend =
  let proc (name: int) (inCh: Ch<int>) (outCh: Ch<int>) (finishCh: Ch<int>) =
    Job.foreverServer
     (inCh >>= fun n ->
      if n <> 0 then
        outCh *<+ (n-1)
      else
        finishCh *<+ name)

  let mkChain finishCh n = Job.delay ^ fun () ->
    let ch0 = Ch ()
    seq {1 .. n}
    |> Seq.foldFromJob ch0 ^ fun chIn i ->
         let chOut = if i=n then ch0 else Ch ()
         proc i chIn chOut finishCh >>-. chOut

  let run n m p =
    printf " ChSend: "
    let timer = Stopwatch.StartNew ()
    let before = GC.GetTotalMemory true
    ignore ^ runDelay ^ fun () ->
      let ps = Array.create p n
      let finishCh = Ch ()
      ps
      |> Seq.Con.mapJob ^ mkChain finishCh
      >>= fun chs ->
      printf "%5d b/c " (max 0L (GC.GetTotalMemory true - before) / int64 (p*n))
      chs
      |> Seq.Con.iterJob ^ fun ch ->
            ch *<+ m
      >>=. Seq.Con.mapJob (fun _ -> finishCh) (seq {1 .. p})
    let d = timer.Elapsed
    printf "%9.0f m/s - %fs\n"
     (float (p*m) / d.TotalSeconds) d.TotalSeconds

module MbSend =
  let proc (name: int)
           (inMS: Mailbox<int>)
           (outMS: Mailbox<int>)
           (finishCh: Ch<int>) =
    Job.foreverServer
     (inMS >>= fun n ->
      if n <> 0 then
        outMS *<<+ (n-1)
      else
        finishCh *<- name :> Job<_>)

  let mkChain finishCh n = Job.delay ^ fun () ->
    let ms0 = Mailbox ()
    seq {1 .. n}
    |> Seq.foldFromJob ms0 ^ fun msIn i ->
         let msOut = if i=n then ms0 else Mailbox ()
         proc i msIn msOut finishCh >>-. msOut

  let run n m p =
    printf " MbSend: "
    let timer = Stopwatch.StartNew ()
    let before = GC.GetTotalMemory true
    ignore ^ runDelay ^ fun () ->
      let ps = Array.create p n
      let finishCh = Ch ()
      ps
      |> Seq.Con.mapJob ^
           mkChain finishCh
      >>= fun chs ->
      printf "%5d b/c " (max 0L (GC.GetTotalMemory true - before) / int64 (p*n))
      chs
      |> Seq.Con.iterJob ^ fun ms ->
            ms *<<+ m
      >>=. Seq.Con.mapJob (fun _ -> finishCh) (seq {1 .. p})
    let d = timer.Elapsed
    printf "%9.0f m/s - %fs\n"
     (float (p*m) / d.TotalSeconds) d.TotalSeconds

module MPPost =
  open Async.Infixes

  type MbPr<'a> = MailboxProcessor<'a>

  let mkChain n (finishPr: MbPr<_>) : array<MbPr<_>> =
    let mbprs = Array.zeroCreate n
    for i = 0 to n-1 do
      mbprs.[i] <- new MbPr<_>(fun inbox ->
        let name = i+1
        let nextPr = mbprs.[(i+1) % n]
        let rec loop =
          inbox.Receive () >>= fun n ->
          if n <> 0 then
            nextPr.Post (n-1)
          else
            finishPr.Post name
          loop
        loop)
    mbprs |> Array.iter ^ fun mbpr -> mbpr.Start ()
    mbprs

  let run n m p =
    printf " MPPost: "
    let timer = Stopwatch.StartNew ()
    let before = GC.GetTotalMemory true
    use allDone = new System.Threading.ManualResetEventSlim ()
    let results = ResizeArray<_>()
    use finishPr = MbPr<_>.Start ^ fun inbox ->
      async {
        for i=1 to p do
          let! x = inbox.Receive ()
          do ignore i; results.Add x
        do allDone.Set ()
      }
    let chains = Array.init p ^ fun _ -> mkChain n finishPr
    printf "%5d b/c " (max 0L (GC.GetTotalMemory true - before) / int64 (p*n))
    for i=0 to p-1 do
      chains.[i].[0].Post m
    allDone.Wait ()
    for i=0 to p-1 do
      for j=0 to n-1 do
        dispose chains.[i].[j]
    let d = timer.Elapsed
    printf "%9.0f m/s - %fs\n"
     (float (p*m) / d.TotalSeconds) d.TotalSeconds

do for p in [1; Environment.ProcessorCount] do
     for l in [50003; 503; 53] do
       for n in [500; 500000; 50000000] do
         printf "\nWith %d rings of length %d passing %d msgs:\n\n" p l n
         if n <= 500000 then
           if l <= 53 then
             Native.run l n p ; GC.clean ()
           MPPost.run l n p ; GC.clean ()
         ChGive.run l n p ; GC.clean ()
         MbSend.run l n p ; GC.clean ()
         ChSend.run l n p ; GC.clean ()
