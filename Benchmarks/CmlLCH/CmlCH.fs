module CmlLCH

open System
open System.Diagnostics
open Hopac
open Hopac.Infixes
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
      Job.start (nack |>> fun () -> show "A" kind var) |>> fun () ->
      (op |>>? fun _ -> show "D" kind var)
    Job.delay <| fun () ->
    let x = ch ()
    let y = ch ()
    let z = ch ()
    Job.queue (make "+" "x" (x <-- ()) <|>?
               make "+" "y" (y <-- ())) >>= fun () ->
    Job.queue (make "-" "y" y <|>?
               make "-" "z" z) >>= fun () ->
    Job.queue (make "-" "x" x) >>= fun () ->
    Job.queue (make "+" "z" (z <-- ()))

  let run n =
    printf "EgPaper %8d: " n
    let timer = Stopwatch.StartNew ()
    let r = run (Job.forN n egpaper)
    let d = timer.Elapsed
    printf "%fs\n" d.TotalSeconds

module SwapCh =
  type SwapChannel<'a> = Ch<'a * Ch<'a>>

  let swapch () = ch ()

  let swap sCh outMsg =
    (sCh >>=? fun (inMsg, outCh) ->
     outCh <-- outMsg >>% inMsg) <|>?
    (Alt.delay <| fun () ->
     let inCh = ch ()
     sCh <-- (outMsg, inCh) >>.? inCh)

  let bench = Job.delay <| fun () ->
    let sCh = swapch ()
    Job.queue (swap sCh ()) >>= fun () ->
    swap sCh ()

  let run n =
    printf "SwapCh %8d: " n
    let timer = Stopwatch.StartNew ()
    let r = run (Job.forN n bench)
    let d = timer.Elapsed
    printf "%fs\n" d.TotalSeconds

module BufferedCh =
  type BufferedCh<'a> = Ch<'a> * Ch<'a>

  let buff () = Job.delay <| fun () ->
    let inCh = ch ()
    let outCh = ch ()
    Job.iterateServer ([], [])
     (function
       | ([], []) ->
         inCh |>> fun x -> ([x], [])
       | ((x::xs) as xxs, ys) ->
         (inCh |>>? fun y -> (xxs, y::ys)) <|>?
         (outCh <-- x >>%? (xs, ys)) :> Job<_>
       | ([], ys) ->
         Job.result (List.rev ys, [])) >>%
    (inCh, outCh)

  let send (inCh, _) x = inCh <-- x
  let recv (_, outCh) = outCh :> Job<_>

  let bench =
    buff () >>= fun buf ->
    Job.queue (send buf 1) >>= fun () ->
    Job.queue (send buf 2) >>= fun () ->
    Job.queue (recv buf) >>= fun () ->
    recv buf

  let run n =
    printf "BufferedCh %8d: " n
    let timer = Stopwatch.StartNew ()
    let r = run (Job.forN n bench)
    let d = timer.Elapsed
    printf "%fs\n" d.TotalSeconds

let cleanup () =
  for i=1 to 2 do
    GC.Collect ()
    Threading.Thread.Sleep 50

do let mutable n = 1
   for i=1 to 6 do
     n <- n*10
     cleanup ()
     EgPaper.run n

do let mutable n = 1
   for i=1 to 6 do
     n <- n*10
     cleanup ()
     SwapCh.run n

do let mutable n = 1
   for i=1 to 6 do
     n <- n*10
     cleanup ()
     BufferedCh.run n
