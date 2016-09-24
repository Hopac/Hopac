module PostMailbox

#nowarn "40"

open System
open System.Diagnostics
open System.Threading
open Hopac
open Hopac.Bench
open Hopac.Infixes
open Hopac.Extensions

module Array =
  let inline last (xs: array<_>) = xs.[xs.Length-1]

module Async =
  open Async.Infixes

  type Agent<'t> = MailboxProcessor<'t>

  let run data =
    printf "Async:     "
    let timer = Stopwatch.StartNew ()
    let max = Array.last data
    use ping = new ManualResetEventSlim ()
    let agent = Agent<int>.Start ^ fun inbox ->
        let rec loop =
          inbox.Receive () >>= fun msg ->
          if msg = max then ping.Set ()
          loop
        loop
    do data |> Array.iter agent.Post
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
    do run ^ job {
         do! Job.foreverServer
              (mMb >>- fun msg ->
               if msg = max then ping.Set ())
         do! data |> Array.iterJob ^ fun i -> mMb *<<+ i
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
    server
     (Job.forever
       (mMb >>- fun msg ->
        if msg = max then ping.Set ()))
    data |> Array.iter ^ Mailbox.Now.send mMb
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
    do run ^ job {
         do! Job.foreverServer
              (mb >>- fun msg ->
               if msg = max then ping.Set ())
         do! data |> Array.iterJob ^ fun i -> mb *<- i
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
    do run ^ job {
         do! Job.foreverServer
              (mb >>- fun msg ->
               if msg = max then ping.Set ())
         do! data |> Array.iterJob ^ fun i -> mb *<+ i
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
    server
     (Job.forever
       (ch >>- fun msg ->
        if msg = max then ping.Set ()))
    data |> Array.iter ^ Ch.Now.send ch
    let d1 = timer.Elapsed
    ping.Wait ()
    let d2 = timer.Elapsed
    printfn "%10.0f and %10.0f msgs/s"
      (float max / d1.TotalSeconds) (float max / d2.TotalSeconds)

do for f in [ChGive.run; MbSend.run; MbSendNow.run; ChSend.run; ChSendNow.run; Async.run] do
     for n in [|2000; 20000; 200000; 2000000; 20000000|] do
       let data = [|1 .. n|]
       f data
       GC.clean ()
