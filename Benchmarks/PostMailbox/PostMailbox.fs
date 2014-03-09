module PostMailbox

open System
open System.Diagnostics
open System.Threading

module Array =
  let inline last (xs: array<_>) = xs.[xs.Length-1]

module Async =
  type Agent<'t> = MailboxProcessor<'t>

  let run data =
    printf "Async:     "
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
    printfn "%10.0f and %10.0f msgs/s" (float max / d1.TotalSeconds) (float max / d2.TotalSeconds)

module MbSend =
  open Hopac
  open Hopac.Job.Infixes
  open Hopac.Extensions

  let run data =
    printf "MbSend:    "
    let timer = Stopwatch.StartNew ()
    let max = Array.last data
    use ping = new ManualResetEventSlim ()
    let mb = Mailbox.Now.create ()
    do run <| job {
         do! Job.server
              (Job.forever
                (Mailbox.take mb |>> fun msg ->
                 if msg = max then ping.Set ()))
         do! data |> Array.iterJob (fun i -> Mailbox.send mb i)
       }
    let d1 = timer.Elapsed
    ping.Wait ()
    let d2 = timer.Elapsed
    printfn "%10.0f and %10.0f msgs/s" (float max / d1.TotalSeconds) (float max / d2.TotalSeconds)

module MbSendNow =
  open Hopac
  open Hopac.Job.Infixes
  open Hopac.Extensions

  let run data =
    printf "MbSendNow: "
    let timer = Stopwatch.StartNew ()
    let max = Array.last data
    use ping = new ManualResetEventSlim ()
    let mb = Mailbox.Now.create ()
    Job.Now.server
     (Job.forever
       (Mailbox.take mb |>> fun msg ->
        if msg = max then ping.Set ()))
    data |> Array.iter (fun i -> Mailbox.Now.send mb i)
    let d1 = timer.Elapsed
    ping.Wait ()
    let d2 = timer.Elapsed
    printfn "%10.0f and %10.0f msgs/s" (float max / d1.TotalSeconds) (float max / d2.TotalSeconds)

module ChGive =
  open Hopac
  open Hopac.Job.Infixes
  open Hopac.Extensions

  let run data =
    printf "ChGive:    "
    let timer = Stopwatch.StartNew ()
    let max = Array.last data
    use ping = new ManualResetEventSlim ()
    let mb = Ch.Now.create ()
    do run <| job {
         do! Job.server 
              (Job.forever
                (Ch.take mb |>> fun msg ->
                 if msg = max then ping.Set ()))
         do! data |> Array.iterJob (fun i -> Ch.give mb i)
       }
    let d1 = timer.Elapsed
    ping.Wait ()
    let d2 = timer.Elapsed
    printfn "%10.0f and %10.0f msgs/s" (float max / d1.TotalSeconds) (float max / d2.TotalSeconds)

module ChSend =
  open Hopac
  open Hopac.Job.Infixes
  open Hopac.Extensions

  let run data =
    printf "ChSend:    "
    let timer = Stopwatch.StartNew ()
    let max = Array.last data
    use ping = new ManualResetEventSlim ()
    let mb = Ch.Now.create ()
    do run <| job {
         do! Job.server 
              (Job.forever
                (Ch.take mb |>> fun msg ->
                 if msg = max then ping.Set ()))
         do! data |> Array.iterJob (fun i -> Ch.send mb i)
       }
    let d1 = timer.Elapsed
    ping.Wait ()
    let d2 = timer.Elapsed
    printfn "%10.0f and %10.0f msgs/s" (float max / d1.TotalSeconds) (float max / d2.TotalSeconds)
    
let cleanup () =
  for i=1 to 10 do
    Runtime.GCSettings.LargeObjectHeapCompactionMode <- Runtime.GCLargeObjectHeapCompactionMode.CompactOnce
    GC.Collect ()
    Threading.Thread.Sleep 50

do for f in [ChGive.run; MbSend.run; MbSendNow.run; ChSend.run; Async.run] do
     for n in [|2000; 20000; 200000; 2000000; 20000000|] do
       let data = [|1 .. n|]
       f data
       cleanup ()
