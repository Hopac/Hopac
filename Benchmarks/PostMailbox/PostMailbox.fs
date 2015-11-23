module PostMailbox

open System
open System.Diagnostics
open System.Threading
open Hopac
open Hopac.Infixes
open Hopac.Extensions

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
            if msg = max then ping.Set ()
        })
    do data |> Array.iter (fun i -> agent.Post i)
    let d1 = timer.Elapsed
    ping.Wait ()
    let d2 = timer.Elapsed
    printfn "%10.0f and %10.0f msgs/s"
      (float max / d1.TotalSeconds) (float max / d2.TotalSeconds)

module MbSend =
  let run data =
    printf "MbSend:    "
    let timer = Stopwatch.StartNew ()
    let max = Array.last data
    use ping = new ManualResetEventSlim ()
    let mMb = Mailbox ()
    do run <| job {
         do! Job.foreverServer
              (mMb >>- fun msg ->
               if msg = max then ping.Set ())
         do! data |> Array.iterJob (fun i -> mMb *<<+ i)
       }
    let d1 = timer.Elapsed
    ping.Wait ()
    let d2 = timer.Elapsed
    printfn "%10.0f and %10.0f msgs/s"
      (float max / d1.TotalSeconds) (float max / d2.TotalSeconds)

module MbSendNow =
  let run data =
    printf "MbSendNow: "
    let timer = Stopwatch.StartNew ()
    let max = Array.last data
    use ping = new ManualResetEventSlim ()
    let mMb = Mailbox ()
    Job.Global.server
     (Job.forever
       (mMb >>- fun msg ->
        if msg = max then ping.Set ()))
    data |> Array.iter (fun i -> Mailbox.Global.send mMb i)
    let d1 = timer.Elapsed
    ping.Wait ()
    let d2 = timer.Elapsed
    printfn "%10.0f and %10.0f msgs/s"
      (float max / d1.TotalSeconds) (float max / d2.TotalSeconds)

module ChGive =
  let run data =
    printf "ChGive:    "
    let timer = Stopwatch.StartNew ()
    let max = Array.last data
    use ping = new ManualResetEventSlim ()
    let mb = Ch ()
    do run <| job {
         do! Job.foreverServer
              (mb >>- fun msg ->
               if msg = max then ping.Set ())
         do! data |> Array.iterJob (fun i -> mb *<- i)
       }
    let d1 = timer.Elapsed
    ping.Wait ()
    let d2 = timer.Elapsed
    printfn "%10.0f and %10.0f msgs/s"
      (float max / d1.TotalSeconds) (float max / d2.TotalSeconds)

module ChSend =
  let run data =
    printf "ChSend:    "
    let timer = Stopwatch.StartNew ()
    let max = Array.last data
    use ping = new ManualResetEventSlim ()
    let mb = Ch ()
    do run <| job {
         do! Job.foreverServer
              (mb >>- fun msg ->
               if msg = max then ping.Set ())
         do! data |> Array.iterJob (fun i -> mb *<+ i)
       }
    let d1 = timer.Elapsed
    ping.Wait ()
    let d2 = timer.Elapsed
    printfn "%10.0f and %10.0f msgs/s"
      (float max / d1.TotalSeconds) (float max / d2.TotalSeconds)

module ChSendNow =
  let run data =
    printf "ChSendNow: "
    let timer = Stopwatch.StartNew ()
    let max = Array.last data
    use ping = new ManualResetEventSlim ()
    let ch = Ch ()
    Job.Global.server
     (Job.forever
       (ch >>- fun msg ->
        if msg = max then ping.Set ()))
    data |> Array.iter (fun i -> Ch.Global.send ch i)
    let d1 = timer.Elapsed
    ping.Wait ()
    let d2 = timer.Elapsed
    printfn "%10.0f and %10.0f msgs/s"
      (float max / d1.TotalSeconds) (float max / d2.TotalSeconds)
    
let cleanup () =
  for i=1 to 10 do
    GC.Collect ()
    Threading.Thread.Sleep 50

do for f in [ChGive.run; MbSend.run; MbSendNow.run; ChSend.run; ChSendNow.run; Async.run] do
     for n in [|2000; 20000; 200000; 2000000; 20000000|] do
       let data = [|1 .. n|]
       f data
       cleanup ()
