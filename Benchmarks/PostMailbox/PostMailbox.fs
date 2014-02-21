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
  open Hopac.Extensions

  let run data =
    printfn "HopacMb:"
    let timer = Stopwatch.StartNew ()
    let max = Array.last data
    use ping = new ManualResetEventSlim ()
    let mb = Mailbox.Now.create ()
    do Job.Now.run <| job {
         do! Job.start <| job {
               while true do
                 let! msg = Mailbox.take mb
                 if msg = max then ping.Set () else ()
             }
         do! data |> Array.iterJ (fun i -> Mailbox.send mb i)
       }
    let d1 = timer.Elapsed
    ping.Wait ()
    let d2 = timer.Elapsed
    printfn "- Posted:   %f msgs/s" (float max / d1.TotalSeconds)
    printfn "- Finished: %f msgs/s" (float max / d2.TotalSeconds)

module HopacCh =
  open Hopac
  open Hopac.Extensions

  let run data =
    printfn "HopacCh:"
    let timer = Stopwatch.StartNew ()
    let max = Array.last data
    use ping = new ManualResetEventSlim ()
    let mb = Ch.Now.create ()
    do Job.Now.run <| job {
         do! Job.start <| job {
               while true do
                 let! msg = Ch.take mb
                 if msg = max then ping.Set () else ()
             }
         do! data |> Array.iterJ (fun i -> Ch.give mb i)
       }
    let d1 = timer.Elapsed
    ping.Wait ()
    let d2 = timer.Elapsed
    printfn "- Posted:   %f msgs/s" (float max / d1.TotalSeconds)
    printfn "- Finished: %f msgs/s" (float max / d2.TotalSeconds)
    
do HopacCh.run dataWarmUp
   HopacCh.run data
   HopacMb.run dataWarmUp
   HopacMb.run data
   Async.run dataWarmUp
   Async.run data
   
