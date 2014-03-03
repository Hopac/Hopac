module PostMailbox

// Inspired by: http://theburningmonk.com/2012/03/f-how-many-messages-can-you-post-to-a-f-agent-in-one-second/

open System
open System.Diagnostics
open System.Threading

let dataWarmUp = [|1..100000|]
let data       = [|1..20000000|]

module Array =
  let inline last (xs: array<_>) = xs.[xs.Length-1]

module Async =
  type Agent<'t> = MailboxProcessor<'t>

  let run data =
    printfn "Async:"
    let timer = Stopwatch.StartNew ()
    let max = Array.last data
    use ping = new ManualResetEventSlim ()
    let agent = Agent<int>.Start (fun inbox ->
        async {
          while true do
            let! msg = inbox.Receive ()
            if msg = max then ping.Set () else ()
        })
    do data |> Array.iter (fun i -> agent.Post i)
    let d1 = timer.Elapsed
    ping.Wait ()
    let d2 = timer.Elapsed
    printfn "- Posted:   %f msgs/s" (float max / d1.TotalSeconds)
    printfn "- Finished: %f msgs/s" (float max / d2.TotalSeconds)

module HopacMb =
  open Hopac
  open Hopac.Job.Infixes
  open Hopac.Extensions

  let run data =
    printfn "HopacMb:"
    let timer = Stopwatch.StartNew ()
    let max = Array.last data
    use ping = new ManualResetEventSlim ()
    let mb = Mailbox.Now.create ()
    do run <| job {
         do! Job.start
              (Job.forever
                (Mailbox.take mb |>> fun msg ->
                 if msg = max then ping.Set ()))
         do! data |> Array.iterJob (fun i -> Mailbox.send mb i)
       }
    let d1 = timer.Elapsed
    ping.Wait ()
    let d2 = timer.Elapsed
    printfn "- Posted:   %f msgs/s" (float max / d1.TotalSeconds)
    printfn "- Finished: %f msgs/s" (float max / d2.TotalSeconds)

module ChGive =
  open Hopac
  open Hopac.Job.Infixes
  open Hopac.Extensions

  let run data =
    printfn "ChGive:"
    let timer = Stopwatch.StartNew ()
    let max = Array.last data
    use ping = new ManualResetEventSlim ()
    let mb = Ch.Now.create ()
    do run <| job {
         do! Job.start 
              (Job.forever
                (Ch.take mb |>> fun msg ->
                 if msg = max then ping.Set ()))
         do! data |> Array.iterJob (fun i -> Ch.give mb i)
       }
    let d1 = timer.Elapsed
    ping.Wait ()
    let d2 = timer.Elapsed
    printfn "- Posted:   %f msgs/s" (float max / d1.TotalSeconds)
    printfn "- Finished: %f msgs/s" (float max / d2.TotalSeconds)

module ChSend =
  open Hopac
  open Hopac.Job.Infixes
  open Hopac.Extensions

  let run data =
    printfn "ChSend:"
    let timer = Stopwatch.StartNew ()
    let max = Array.last data
    use ping = new ManualResetEventSlim ()
    let mb = Ch.Now.create ()
    do run <| job {
         do! Job.start 
              (Job.forever
                (Ch.take mb |>> fun msg ->
                 if msg = max then ping.Set ()))
         do! data |> Array.iterJob (fun i -> Ch.send mb i)
       }
    let d1 = timer.Elapsed
    ping.Wait ()
    let d2 = timer.Elapsed
    printfn "- Posted:   %f msgs/s" (float max / d1.TotalSeconds)
    printfn "- Finished: %f msgs/s" (float max / d2.TotalSeconds)
    
let cleanup () =
  for i=1 to 10 do
    GC.Collect ()
    Threading.Thread.Sleep 50

do ChGive.run dataWarmUp ; cleanup ()
   ChSend.run dataWarmUp ; cleanup ()
   HopacMb.run dataWarmUp ; cleanup ()
   Async.run dataWarmUp ; cleanup ()

   ChGive.run data ; cleanup ()
   ChGive.run data ; cleanup ()
   ChSend.run data ; cleanup ()
   ChSend.run data ; cleanup ()
   HopacMb.run data ; cleanup ()
   HopacMb.run data ; cleanup ()
   Async.run data ; cleanup ()
   Async.run data
   
