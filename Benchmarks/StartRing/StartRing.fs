// Copyright (C) by Housemarque, Inc.

module StartRing

open Hopac
open Hopac.Infixes
open Hopac.Extensions
open Hopac.Job.Infixes
open System
open System.Diagnostics

module Native =
  open System.Threading

  let run n =
    printf "Native: "
    let timer = Stopwatch.StartNew ()
    let selfCh = new AutoResetEvent (false)
    let rec proc n (selfCh: AutoResetEvent) (toCh: AutoResetEvent) =
      if n = 0 then
        toCh.Set () |> ignore
      else
        let childCh = new AutoResetEvent (false)
        let child = Thread (ThreadStart (fun () ->
                              proc (n-1) childCh toCh),
                            512)
        child.Start ()
        childCh.Set () |> ignore
      selfCh.WaitOne () |> ignore
      selfCh.Dispose ()
    proc n selfCh selfCh
    let d = timer.Elapsed
    printf "%9.0f ops/s - %fs\n"
     (float n / d.TotalSeconds) d.TotalSeconds

module ThPool =
  open System.Threading

  let run n =
    printf "ThPool: "
    let timer = Stopwatch.StartNew ()
    let selfCh = new AutoResetEvent (false)
    let rec proc n (selfCh: AutoResetEvent) (toCh: AutoResetEvent) =
      if n = 0 then
        toCh.Set () |> ignore
      else
        let childCh = new AutoResetEvent (false)
        ThreadPool.QueueUserWorkItem (WaitCallback (fun _ ->
          proc (n-1) childCh toCh)) |> ignore
        childCh.Set () |> ignore
      selfCh.WaitOne () |> ignore
      selfCh.Dispose ()
    proc n selfCh selfCh
    let d = timer.Elapsed
    printf "%9.0f ops/s - %fs\n"
     (float n / d.TotalSeconds) d.TotalSeconds

module Tasks =
  open System.Threading
  open System.Threading.Tasks

  let run n =
    printf "Tasks:  "
    let timer = Stopwatch.StartNew ()
    let selfCh = new AutoResetEvent (false)
    let rec proc n (selfCh: AutoResetEvent) (toCh: AutoResetEvent) =
      if n = 0 then
        toCh.Set () |> ignore
      else
        let childCh = new AutoResetEvent (false)
        Task.Factory.StartNew (fun _ ->
          proc (n-1) childCh toCh) |> ignore
        childCh.Set () |> ignore
      selfCh.WaitOne () |> ignore
      selfCh.Dispose ()
    proc n selfCh selfCh
    let d = timer.Elapsed
    printf "%9.0f ops/s - %fs\n"
     (float n / d.TotalSeconds) d.TotalSeconds

module JQueue =
  let run n =
    printf "JQueue: "
    let timer = Stopwatch.StartNew ()
    run << Job.delay <| fun () ->
      let selfCh = ch ()
      let rec proc n selfCh toCh = Job.delay <| fun () ->
        if n = 0 then
          toCh <-+ ()
        else
          let childCh = ch ()
          Job.queue (proc (n-1) childCh toCh) >>.
          (childCh <-+ ()) >>.
          selfCh
      proc n selfCh selfCh
    let d = timer.Elapsed
    printf "%9.0f ops/s - %fs\n"
     (float n / d.TotalSeconds) d.TotalSeconds

module JStart =
  let run n =
    printf "JStart: "
    let timer = Stopwatch.StartNew ()
    run << Job.delay <| fun () ->
      let selfCh = ch ()
      let rec proc n selfCh toCh = Job.delay <| fun () ->
        if n = 0 then
          toCh <-+ ()
        else
          let childCh = ch ()
          Job.start (proc (n-1) childCh toCh) >>.
          (childCh <-+ ()) >>.
          selfCh
      proc n selfCh selfCh
    let d = timer.Elapsed
    printf "%9.0f ops/s - %fs\n"
     (float n / d.TotalSeconds) d.TotalSeconds

module PStart =
  let run n =
    printf "PStart: "
    let timer = Stopwatch.StartNew ()
    run << Job.delay <| fun () ->
      let selfCh = ch ()
      let rec proc n selfCh toCh = Job.delay <| fun () ->
        if n = 0 then
          toCh <-+ ()
        else
          let childCh = ch ()
          Proc.start (proc (n-1) childCh toCh) >>.
          (childCh <-+ ()) >>.
          selfCh
      proc n selfCh selfCh
    let d = timer.Elapsed
    printf "%9.0f ops/s - %fs\n"
     (float n / d.TotalSeconds) d.TotalSeconds

module PQueue =
  let run n =
    printf "PQueue: "
    let timer = Stopwatch.StartNew ()
    run << Job.delay <| fun () ->
      let selfCh = ch ()
      let rec proc n selfCh toCh = Job.delay <| fun () ->
        if n = 0 then
          toCh <-+ ()
        else
          let childCh = ch ()
          Proc.queue (proc (n-1) childCh toCh) >>.
          (childCh <-+ ()) >>.
          selfCh
      proc n selfCh selfCh
    let d = timer.Elapsed
    printf "%9.0f ops/s - %fs\n"
     (float n / d.TotalSeconds) d.TotalSeconds

module Async =
  let run n =
    printf "Async:  "
    let timer = Stopwatch.StartNew ()
    Async.RunSynchronously <| async {
      let selfMb = new MailboxProcessor<unit>(fun _ -> async { () })
      let rec proc n
                   (toMb: MailboxProcessor<_>)
                   (selfMb: MailboxProcessor<_>) = async {
        if n = 0 then
          do toMb.Post ()
        else
          let childMb = MailboxProcessor<unit>.Start (proc (n-1) toMb)
          do childMb.Post ()
        let! () = selfMb.Receive ()
        do (selfMb :> IDisposable).Dispose ()
      }
      do! proc n selfMb selfMb
    }
    let d = timer.Elapsed
    printf "%9.0f ops/s - %fs\n"
     (float n / d.TotalSeconds) d.TotalSeconds

let cleanup () =
  for i=1 to 5 do
    GC.Collect ()
    Threading.Thread.Sleep 50

do for f in [JQueue.run; PQueue.run; JStart.run; PStart.run] do
     for n in [10; 100; 1000; 10000; 100000; 1000000; 10000000] do
       f n
       cleanup ()

do for f in [ThPool.run; Tasks.run; Async.run] do
     for n in [10; 100; 1000; 10000; 100000; 1000000] do
       f n
       cleanup ()

do for n in [10; 100; 1000; 10000] do
     Native.run n
     cleanup ()
