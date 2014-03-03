module PingPong

// Inspired by: http://letitcrash.com/post/20397701710/50-million-messages-per-second-on-a-single-machine

open Hopac
open Hopac.Job.Infixes
open System
open System.IO
open System.Diagnostics

type Msg<'a> = Msg of Ch<Msg<'a>> * 'a

let run numPairs numMsgsPerPair =
  GC.Collect ()
  let timer = Stopwatch.StartNew ()
  run <| job {
    let chEnd = Ch.Now.create ()
    do! Job.forN numPairs <| job {
      let chPing = Ch.Now.create ()
      let chPong = Ch.Now.create ()
      do! Job.start
           (Job.forever (Ch.take chPing >>= fun (Msg (chPong, msg)) ->
                         Ch.send chPong (Msg (chPing, msg))))
      do! Job.start <| job {
        do! Ch.give chPing (Msg (chPong, "msg"))
        do! Job.forN (numMsgsPerPair-1)
             (Ch.take chPong >>= fun (Msg (chPing, msg)) ->
              Ch.send chPing (Msg (chPong, msg)))
        do! Ch.give chEnd ()
      }
    }
    do! Job.forN numPairs (Ch.take chEnd)
  }
  let d = timer.Elapsed
  let total = numPairs * numMsgsPerPair
  let m = sprintf "%d ping-pongs - %f msgs/s - %fs\n"
           total (float (total * 2) / d.TotalSeconds) d.TotalSeconds
  do use w = new StreamWriter ("Results.txt", true)
     w.Write m
  printf "%s" m

do [(1,                          20000000)
    (2,                          20000000)
    (Environment.ProcessorCount, 20000000)]
   |> List.iter (fun (numPairs, numMsgsPerPair) ->
      GC.Collect ()
      run numPairs numMsgsPerPair)
