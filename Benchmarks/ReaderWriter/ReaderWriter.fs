module ReaderWriter

// Inspired by http://t0yv0.blogspot.com/2011/12/making-async-5x-faster.html

open System
open System.Diagnostics
open Hopac
open Hopac.Bench
open Hopac.Infixes

module Literal =
  let run n =
    printf "Literal: "
    let timer = Stopwatch.StartNew ()
    ignore ^ run ^ job {
      let iCh = Ch ()
      let rec writer i = job {
        if i = 0 then
          return! iCh *<- 0
        else
          do! iCh *<- i
          return! writer ^ i-1
      }
      let rec reader sum = job {
        let! x = iCh
        if x = 0 then
          return sum
        else
          return! reader ^ sum + x
      }
      do! Job.start ^ writer n
      return! reader 0
    }
    let d = timer.Elapsed
    printf "%9.0f hops/s\n" ^ float n / d.TotalSeconds

module Tweaked =
  let run n =
    printf "Tweaked: "
    let timer = Stopwatch.StartNew ()
    ignore ^ run ^ job {
      let iCh = Ch ()
      let rec writer i =
        iCh *<- i >>= fun () ->
        if i = 0 then
          Job.unit ()
        else
          writer ^ i-1
      let rec reader sum =
        iCh >>= fun x ->
        if x = 0 then
          Job.result sum
        else
          reader ^ sum + x
      do! Job.start ^ writer n
      return! reader 0
    }
    let d = timer.Elapsed
    printf "%9.0f hops/s\n" ^ float n / d.TotalSeconds

module AsyncPR =
  open Async.Infixes

  let run n =
    printf "AsyncPR: "
    let timer = Stopwatch.StartNew ()
    use readerMb = MailboxProcessor.Start ^ fun _ -> Async.result ()
    let rec writer i =
      readerMb.PostAndAsyncReply (fun arc -> (i, arc)) >>= fun () ->
      if i = 0 then
        Async.unit
      else
        writer ^ i-1
    writer n |> Async.StartImmediate
    let recv = readerMb.Receive ()
    let rec reader sum =
      recv >>= fun (x, r: AsyncReplyChannel<unit>) ->
      r.Reply ()
      if x = 0 then
        Async.result sum
      else
        reader ^ sum + x
    reader 0 |> Async.RunSynchronously |> ignore
    let d = timer.Elapsed
    printf "%9.0f hops/s\n" ^ float n / d.TotalSeconds

do for f in [Literal.run; Tweaked.run; AsyncPR.run] do
     for n in [2000; 20000; 200000; 2000000; 20000000] do
       f n ; GC.clean ()
