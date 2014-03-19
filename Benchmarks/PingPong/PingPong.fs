module PingPong

// Inspired by: http://letitcrash.com/post/20397701710/50-million-messages-per-second-on-a-single-machine

open Hopac
open Hopac.Infixes
open Hopac.Job.Infixes
open System
open System.Diagnostics

type Msg<'a> = Msg of Ch<Msg<'a>> * 'a

module Ch =
  let run numPairs numMsgsPerPair =
    printf "Ch: "
    let timer = Stopwatch.StartNew ()
    run <| job {
      let chEnd = ch ()
      do! Job.forN numPairs <| job {
        let chPing = ch ()
        let chPong = ch ()
        do! Job.server
             (Job.forever (chPing >>= fun (Msg (chPong, msg)) ->
                           chPong <-- Msg (chPing, msg)))
        do! Job.start <| job {
          do! chPing <-- Msg (chPong, "msg")
          do! Job.forN (numMsgsPerPair-1)
               (chPong >>= fun (Msg (chPing, msg)) ->
                chPing <-+ Msg (chPong, msg))
          do! chEnd <-- ()
        }
      }
      do! Job.forN numPairs chEnd
    }
    let d = timer.Elapsed
    let total = numPairs * numMsgsPerPair
    printf "%10d - %f msgs/s - %fs\n"
     total (float (total * 2) / d.TotalSeconds) d.TotalSeconds

let cleanup () =
  for i=1 to 5 do
    Runtime.GCSettings.LargeObjectHeapCompactionMode <- Runtime.GCLargeObjectHeapCompactionMode.CompactOnce
    GC.Collect ()
    Threading.Thread.Sleep 50

do for p in seq {1 .. Environment.ProcessorCount} do
     for n in [1000; 10000; 100000; 1000000; 10000000] do
       Ch.run p n ; cleanup ()
