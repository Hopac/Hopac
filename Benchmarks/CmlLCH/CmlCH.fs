module CmlLCH

// Inspired by http://www.cs.umd.edu/~avik/projects/cmllch/

open System.Diagnostics
open Hopac
open Hopac.Extra
open Hopac.Alt.Infixes
open Hopac.Job.Infixes

module EgPaper =
  let egpaper =
    let l = obj ()
    let inline show stage kind var =
      lock l <| fun () -> printf "<%s%s%s>" stage kind var
    let inline show stage kind var = ()
    let inline make kind var op =
      Alt.withNack <| fun nack ->
      show "T" kind var
      Job.start (Alt.pick nack |>> fun () -> show "A" kind var) |>> fun () ->
      (op >-> fun _ -> show "D" kind var)
    let inline spawn x = Job.start (Alt.pick x)
    Job.delay <| fun () ->
    let x = Ch.Now.create ()
    let y = Ch.Now.create ()
    let z = Ch.Now.create ()
    spawn (make "+" "x" (Ch.Alt.give x ()) <|>
           make "+" "y" (Ch.Alt.give y ())) >>= fun () ->
    spawn (make "-" "y" (Ch.Alt.take y) <|>
           make "-" "z" (Ch.Alt.take z)) >>= fun () ->
    spawn (make "-" "x" (Ch.Alt.take x)) >>= fun () ->
    spawn (make "+" "z" (Ch.Alt.give z ()))

  let run n =
    printf "EgPaper %d: " n
    let timer = Stopwatch.StartNew ()
    let r = run (Job.forN n egpaper)
    let d = timer.Elapsed
    printf "%fs (%d threads)\n" d.TotalSeconds System.Environment.ProcessorCount

module SwapCh =
  type SwapChannel<'a> = Ch<'a * Ch<'a>>

  module Now =
    let create () = Ch.Now.create ()

  let swap ch outMsg =
    ((Ch.Alt.take ch >=> fun (inMsg, outCh) ->
      Ch.give outCh outMsg >>% inMsg) <|>
     (Alt.delay <| fun () ->
       let inCh = Ch.Now.create ()
       Ch.Alt.give ch (outMsg, inCh) >=> fun () ->
       Ch.take inCh))

  let bench = Job.delay <| fun () ->
    let ch = Now.create ()
    Job.start (Alt.pick (swap ch ())) >>= fun () ->
    Alt.pick (swap ch ())

  let run n =
    printf "SwapCh %d: " n
    let timer = Stopwatch.StartNew ()
    let r = run (Job.forN n bench)
    let d = timer.Elapsed
    printf "%fs (%d threads)\n" d.TotalSeconds System.Environment.ProcessorCount

module BufferedCh =
  type BufferedCh<'a> = Ch<'a> * Ch<'a>

  let create () = Job.delay <| fun () ->
    let inCh = Ch.Now.create ()
    let outCh = Ch.Now.create ()
    Job.server
     (Job.iterate ([], [])
       (function
         | ([], []) ->
           Ch.take inCh |>> fun x ->
           ([x], [])
         | ((x::xs) as xxs, ys) ->
           Alt.pick ((Ch.Alt.take inCh >-> fun y -> (xxs, y::ys))
                 <|> (Ch.Alt.give outCh x >-> fun () -> (xs, ys)))
         | ([], ys) ->
           Job.result (List.rev ys, []))) >>%
    (inCh, outCh)

  let send (inCh, _) x = Ch.give inCh x
  let recv (_, outCh) = Ch.take outCh

  let bench =
    create () >>= fun buf ->
    Job.start (send buf 1) >>= fun () ->
    Job.start (send buf 2) >>= fun () ->
    Job.start (recv buf) >>= fun () ->
    recv buf

  let run n =
    printf "BufferedCh %d: " n
    let timer = Stopwatch.StartNew ()
    let r = run (Job.forN n bench)
    let d = timer.Elapsed
    printf "%fs (%d threads)\n" d.TotalSeconds System.Environment.ProcessorCount

do let mutable n = 1
   for i=1 to 6 do
     n <- n*10
     System.GC.Collect () ; System.Threading.Thread.Sleep 100
     EgPaper.run n

do let mutable n = 1
   for i=1 to 6 do
     n <- n*10
     System.GC.Collect () ; System.Threading.Thread.Sleep 100
     SwapCh.run n

do let mutable n = 1
   for i=1 to 6 do
     n <- n*10
     System.GC.Collect () ; System.Threading.Thread.Sleep 100
     BufferedCh.run n
