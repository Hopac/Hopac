module ReaderWriter

// Inspired by http://t0yv0.blogspot.com/2011/12/making-async-5x-faster.html

open System
open System.Diagnostics
open Hopac
open Hopac.Infixes
open Hopac.Job.Infixes

module Literal =
  let run n =
    printf "Literal: "
    let timer = Stopwatch.StartNew ()
    let i =
      run <| job {
        let iCh = ch ()
        let rec writer i = job {
          if i = 0 then
            return! iCh <-- 0
          else
            do! iCh <-- i
            return! writer (i-1)
        }
        let rec reader sum = job {
          let! x = iCh
          if x = 0 then
            return sum
          else
            return! reader (sum + x)
        }
        do! Job.start (writer n)
        return! reader 0
      }
    let d = timer.Elapsed
    printf "%9.0f hops/s\n" (float n / d.TotalSeconds)

module Tweaked =
  let run n =
    printf "Tweaked: "
    let timer = Stopwatch.StartNew ()
    let i =
      run <| job {
        let iCh = ch ()
        let rec writer i =
          iCh <-- i >>= fun () ->
          if i = 0 then
            Job.unit ()
          else
            writer (i-1)
        let rec reader sum =
          iCh >>= fun x ->
          if x = 0 then
            Job.result sum
          else
            reader (sum + x)
        do! Job.start (writer n)
        return! reader 0
      }
    let d = timer.Elapsed
    printf "%9.0f hops/s\n" (float n / d.TotalSeconds)

module AsyncPR =
  let run n =
    printf "AsyncPR: "
    let timer = Stopwatch.StartNew ()
    use readerMb = MailboxProcessor.Start <| fun _ -> async { return () }
    let rec writer i = async {
      do! readerMb.PostAndAsyncReply (fun arc -> (i, arc))
      if i = 0 then
        return ()
      else
        return! writer (i-1)
    }
    Async.StartImmediate (writer n)
    let rec reader sum = async {
      let! (x, r: AsyncReplyChannel<unit>) = readerMb.Receive ()
      do r.Reply ()
      if x = 0 then
        return sum
      else
        return! reader (sum + x)
    }
    let i = Async.RunSynchronously (reader 0)
    let d = timer.Elapsed
    printf "%9.0f hops/s\n" (float n / d.TotalSeconds)

let cleanup () =
  for i=1 to 5 do
    GC.Collect ()
    Threading.Thread.Sleep 50

do for f in [Literal.run; Tweaked.run; AsyncPR.run] do
     for n in [2000; 20000; 200000; 2000000; 20000000] do
       f n ; cleanup ()
