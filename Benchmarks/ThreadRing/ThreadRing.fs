// Copyright (C) by Housemarque, Inc.

module ThreadRing

open Hopac
open Hopac.Extensions
open Hopac.Job.Infixes
open System
open System.IO
open System.Diagnostics

module Ch =
  let proc (name: int) (inCh: Ch<int>) (outCh: Ch<int>) (finishCh: Ch<int>) : Job<unit> =
    Job.forever
     (Ch.take inCh >>= fun n ->
      if n <> 0 then
        Ch.give outCh (n-1)
      else
        Ch.give finishCh name)

  let mkChain n finishCh = Job.delay <| fun () ->
    let ch0 = Ch.Now.create ()
    seq {1 .. n}
    |> Seq.foldJ
        (fun chIn i ->
           let chOut = if i=n then ch0 else Ch.Now.create ()
           proc i chIn chOut finishCh |> Job.start >>%
           chOut)
        ch0

  let run n m p =
    GC.Collect ()
    let timer = Stopwatch.StartNew ()
    let i =
      run
       (Job.delay <| fun () ->
        let ps = Array.create p n
        let finishCh = Ch.Now.create ()
        ps
        |> Seq.Parallel.iterJ (fun n ->
           mkChain n finishCh >>= fun ch ->
           Ch.give ch m) >>= fun () ->
        Seq.Parallel.mapJ (fun _ -> Ch.take finishCh) (seq {1 .. p}))
    let d = timer.Elapsed
    let m =
      sprintf "Ch: %f msgs/s - %dm/%fs - %A\n"
       (float (p*m) / d.TotalSeconds) (p*m) d.TotalSeconds i
    do use w = new StreamWriter ("Results.txt", true)
       w.Write m
    printf "%s" m

module Mailbox =
  let proc (name: int)
           (inMS: Mailbox<int>)
           (outMS: Mailbox<int>)
           (finishCh: Ch<int>) : Job<unit> =
    Job.forever
     (Mailbox.take inMS >>= fun n ->
      if n <> 0 then
        Mailbox.send outMS (n-1)
      else
        Ch.give finishCh name)

  let mkChain n finishCh = Job.delay <| fun () ->
    let ms0 = Mailbox.Now.create ()
    seq {1 .. n}
    |> Seq.foldJ
        (fun msIn i ->
           let msOut = if i=n then ms0 else Mailbox.Now.create ()
           proc i msIn msOut finishCh |> Job.start >>%
           msOut)
        ms0

  let run n m p =
    GC.Collect ()
    let timer = Stopwatch.StartNew ()
    let i =
      run
       (Job.delay <| fun () ->
        let ps = Array.create p n
        let finishCh = Ch.Now.create ()
        ps
        |> Seq.Parallel.iterJ (fun n ->
           mkChain n finishCh >>= fun ms ->
           Mailbox.send ms m) >>= fun () ->
        Seq.Parallel.mapJ (fun _ -> Ch.take finishCh) (seq {1 .. p}))
    let d = timer.Elapsed
    let m =
      sprintf "Mb: %f msgs/s - %dm/%fs - %A\n"
       (float (p*m) / d.TotalSeconds) (p*m) d.TotalSeconds i
    do use w = new StreamWriter ("Results.txt", true)
       w.Write m
    printf "%s" m

module MbPr =
  type MbPr<'a> = MailboxProcessor<'a>

  let mkChain n (finishPr: MbPr<_>) : array<MbPr<_>> =
    let mbprs = Array.zeroCreate n
    for i = 0 to n-1 do
      mbprs.[i] <- new MbPr<_>(fun inbox ->
        let name = i+1
        let nextPr = mbprs.[(i+1) % n]
        async {
          while true do
            let! n = inbox.Receive ()
            do if n <> 0 then
                 nextPr.Post (n-1)
               else
                 finishPr.Post name
        })
    mbprs |> Array.iter (fun mbpr -> mbpr.Start ())
    mbprs

  let run n m p =
    GC.Collect ()
    let timer = Stopwatch.StartNew ()
    let allDone = new System.Threading.ManualResetEventSlim ()
    let results = ResizeArray<_>()
    let finishPr = new MbPr<_>(fun inbox ->
      async {
        for i=1 to p do
          let! x = inbox.Receive ()
          do results.Add x
        do allDone.Set ()
      })
    finishPr.Start ()
    let chains = Array.init p (fun _ -> mkChain n finishPr)
    for i=0 to p-1 do
      chains.[i].[0].Post m
    allDone.Wait ()
    let d = timer.Elapsed
    let m =
      sprintf "MbPr: %f msgs/s - %dm/%fs - %A\n"
       (float (p*m) / d.TotalSeconds) (p*m) d.TotalSeconds results
    //do use w = new StreamWriter ("Results.txt", true)
    //   w.Write m
    printf "%s" m

do Ch.run 503 5000 1
   GC.Collect() ; System.Threading.Thread.Sleep (System.TimeSpan.FromSeconds 1.0)
   Ch.run 503 50000000 1
   GC.Collect() ; System.Threading.Thread.Sleep (System.TimeSpan.FromSeconds 1.0)
   Ch.run 53 50000000 Environment.ProcessorCount
   GC.Collect() ; System.Threading.Thread.Sleep (System.TimeSpan.FromSeconds 1.0)
   Mailbox.run 503 5000 1
   GC.Collect() ; System.Threading.Thread.Sleep (System.TimeSpan.FromSeconds 1.0)
   Mailbox.run 503 50000000 1
   GC.Collect() ; System.Threading.Thread.Sleep (System.TimeSpan.FromSeconds 1.0)
   Mailbox.run 53 50000000 Environment.ProcessorCount
   GC.Collect() ; System.Threading.Thread.Sleep (System.TimeSpan.FromSeconds 1.0)
   MbPr.run 503 5000 1
   GC.Collect() ; System.Threading.Thread.Sleep (System.TimeSpan.FromSeconds 1.0)
   MbPr.run 503 50000000 1
   GC.Collect() ; System.Threading.Thread.Sleep (System.TimeSpan.FromSeconds 1.0)
   MbPr.run 53 50000000 Environment.ProcessorCount
   