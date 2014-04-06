// Copyright (C) by Housemarque, Inc.

module StartRing

open Hopac
open Hopac.Infixes
open Hopac.Extensions
open Hopac.Job.Infixes
open System
open System.Diagnostics

module ChSend =
  let run n =
    printf "ChSend: "
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
          let! () = selfMb.Receive ()
          do (selfMb :> IDisposable).Dispose ()
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
    Runtime.GCSettings.LargeObjectHeapCompactionMode <- Runtime.GCLargeObjectHeapCompactionMode.CompactOnce
    GC.Collect ()
    Threading.Thread.Sleep 50

do for f in [ChSend.run; Async.run] do
     for n in [1000; 10000; 100000; 1000000; 10000000] do
       f n
       cleanup ()
