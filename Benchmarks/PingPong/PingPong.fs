module PingPong

open Hopac
open Hopac.Infixes
open Hopac.Job.Infixes
open Hopac.Extensions
open System
open System.Diagnostics

module ChGive =
  type Msg = Msg of Ch<Msg>

  let run numPairs numPingPongsPerPair =
    printf "ChGive: "
    let timer = Stopwatch.StartNew ()
    seq {1 .. numPairs}
    |> Seq.Con.iterJob (fun _ ->
       let chPing = ch ()
       let msgPing = Msg chPing
       let chPong = ch ()
       let msgPong = Msg chPong
       Job.foreverServer
        (chPing >>= fun (Msg chPong) ->
         chPong <-- msgPing) >>= fun () ->
       chPing <-- msgPong >>= fun () ->
       Job.forN (numPingPongsPerPair-1)
        (chPong >>= fun (Msg chPing) ->
         chPing <-- msgPong) >>= fun () ->
       chPong >>% ())
    |> run
    let d = timer.Elapsed
    let total = numPairs * numPingPongsPerPair * 2
    printf "%10d - %9.0f msgs/s - %fs\n"
     total (float (total * 2) / d.TotalSeconds) d.TotalSeconds

module ChGiSe =
  type Msg = Msg of Ch<Msg>

  let run numPairs numPingPongsPerPair =
    printf "ChGiSe: "
    let timer = Stopwatch.StartNew ()
    seq {1 .. numPairs}
    |> Seq.Con.iterJob (fun _ ->
       let chPing = ch ()
       let msgPing = Msg chPing
       let chPong = ch ()
       let msgPong = Msg chPong
       Job.foreverServer
        (chPing >>= fun (Msg chPong) ->
         chPong <-- msgPing) >>= fun () ->
       chPing <-+ Msg chPong >>= fun () ->
       Job.forN (numPingPongsPerPair-1)
        (chPong >>= fun (Msg chPing) ->
         chPing <-+ msgPong) >>= fun () ->
       chPong >>% ())
    |> run
    let d = timer.Elapsed
    let total = numPairs * numPingPongsPerPair * 2
    printf "%10d - %9.0f msgs/s - %fs\n"
     total (float (total * 2) / d.TotalSeconds) d.TotalSeconds

module ChSeGi =
  type Msg = Msg of Ch<Msg>

  let run numPairs numPingPongsPerPair =
    printf "ChSeGi: "
    let timer = Stopwatch.StartNew ()
    seq {1 .. numPairs}
    |> Seq.Con.iterJob (fun _ ->
       let chPing = ch ()
       let msgPing = Msg chPing
       let chPong = ch ()
       let msgPong = Msg chPong
       Job.foreverServer
        (chPing >>= fun (Msg chPong) ->
         chPong <-+ msgPing) >>= fun () ->
       chPing <-- Msg chPong >>= fun () ->
       Job.forN (numPingPongsPerPair-1)
        (chPong >>= fun (Msg chPing) ->
         chPing <-- msgPong) >>= fun () ->
       chPong >>% ())
    |> run
    let d = timer.Elapsed
    let total = numPairs * numPingPongsPerPair * 2
    printf "%10d - %9.0f msgs/s - %fs\n"
     total (float (total * 2) / d.TotalSeconds) d.TotalSeconds

module ChSend =
  type Msg = Msg of Ch<Msg>

  let run numPairs numPingPongsPerPair =
    printf "ChSend: "
    let timer = Stopwatch.StartNew ()
    seq {1 .. numPairs}
    |> Seq.Con.iterJob (fun _ ->
       let chPing = ch ()
       let msgPing = Msg chPing
       let chPong = ch ()
       let msgPong = Msg chPong
       Job.foreverServer
        (chPing >>= fun (Msg chPong) ->
         chPong <-+ msgPing) >>= fun () ->
       chPing <-+ msgPong >>= fun () ->
       Job.forN (numPingPongsPerPair-1)
        (chPong >>= fun (Msg chPing) ->
         chPing <-+ msgPong) >>= fun () ->
       chPong >>% ())
    |> run
    let d = timer.Elapsed
    let total = numPairs * numPingPongsPerPair * 2
    printf "%10d - %9.0f msgs/s - %fs\n"
     total (float (total * 2) / d.TotalSeconds) d.TotalSeconds

module MbSend =
  type Msg = Msg of Mailbox<Msg>

  let run numPairs numPingPongsPerPair =
    printf "MbSend: "
    let timer = Stopwatch.StartNew ()
    seq {1 .. numPairs}
    |> Seq.Con.iterJob (fun _ ->
       let mbPing = mb ()
       let msgPing = Msg mbPing
       let mbPong = mb ()
       let msgPong = Msg mbPong
       Job.foreverServer
        (mbPing >>= fun (Msg mbPong) ->
         mbPong <<-+ msgPing) >>= fun () ->
       mbPing <<-+ msgPong >>= fun () ->
       Job.forN (numPingPongsPerPair-1)
        (mbPong >>= fun (Msg mbPing) ->
         mbPing <<-+ msgPong) >>= fun () ->
       mbPong >>% ())
    |> run
    let d = timer.Elapsed
    let total = numPairs * numPingPongsPerPair * 2
    printf "%10d - %9.0f msgs/s - %fs\n"
     total (float (total * 2) / d.TotalSeconds) d.TotalSeconds

module AsPost =
  type Msg = Msg of MailboxProcessor<Msg>

  let run numPairs numPingPongsPerPair =
    printf "AsPost: "
    let timer = Stopwatch.StartNew ()
    use onDone = new MailboxProcessor<unit> (fun _ -> async { return () })
    let startPair () =
      let ping = MailboxProcessor.Start <| fun inbox -> async {
        let msgPing = Msg inbox
        while true do
          let! Msg pong = inbox.Receive ()
          do pong.Post msgPing
      }
      let pong = MailboxProcessor.Start <| fun inbox -> async {
        let msgPong = Msg inbox
        do ping.Post msgPong
        for i=2 to numPingPongsPerPair do
          let! Msg ping = inbox.Receive ()
          do ping.Post msgPong
        let! Msg ping = inbox.Receive ()
        do onDone.Post ()
      }
      [ping; pong]
    let pairs =
      [1 .. numPairs]
      |> List.collect (fun _ -> startPair ())
    Async.RunSynchronously <| async {
      for i=1 to numPairs do
        do! onDone.Receive ()
      return ()
    }
    pairs |> List.iter (fun mb -> (mb :> IDisposable).Dispose ())
    let d = timer.Elapsed
    let total = numPairs * numPingPongsPerPair * 2
    printf "%10d - %9.0f msgs/s - %fs\n"
     total (float (total * 2) / d.TotalSeconds) d.TotalSeconds

let cleanup () =
  for i=1 to 5 do
    Runtime.GCSettings.LargeObjectHeapCompactionMode <- Runtime.GCLargeObjectHeapCompactionMode.CompactOnce
    GC.Collect ()
    Threading.Thread.Sleep 50

do for f in [ChGive.run; ChSend.run; ChGiSe.run; ChSeGi.run; MbSend.run; AsPost.run] do
     printf "\n"
     for p in [1; Environment.ProcessorCount/2; Environment.ProcessorCount] do
       for n in [1000; 10000; 100000; 1000000; 10000000] do
         f p n ; cleanup ()
