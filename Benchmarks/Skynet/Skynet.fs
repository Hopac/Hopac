// Copyright (C) by Vesa Karvonen

module Skynet

open Hopac
open Hopac.Infixes
open Hopac.Extensions
open System
open System.Diagnostics

let time name skynet =
  GC.Collect ()
  Threading.Thread.Sleep 50
  let timer = Stopwatch.StartNew ()
  let sum = skynet ()
  printfn "%s %d %A" name sum timer.Elapsed

module Actorish =
  // This is a quick translation from the Erlang version:
  //
  //   https://github.com/atemerev/skynet/blob/master/erlang/skynet.erl

  let rec skynet parent num size div =
    if size <= 1L then
      parent *<+ num
    else
      let newSize = size / div
      let self = Ch ()
      Job.forUpTo 0 (int div - 1) (fun x ->
        Job.queue (skynet self (num + int64 x * newSize) newSize div)) >>=.
      let rec collect sum toWait =
        self >>= fun n ->
        let sum = sum + n
        let toWait = toWait - 1L
        if 0L < toWait then
          collect sum toWait
        else
          parent *<+ sum
      collect 0L div

  let run size div =
    let result = Ch ()
    Job.queue (skynet result 0L size div)
    >>=. result
    |> run

module Parallelish =
  // This is a quick translation from the Haskell version:
  //
  //  https://github.com/atemerev/skynet/blob/master/haskell/src/Parallel.hs
  
  let children = 10L

  let rec skynet lvl num =
    if lvl = 0 then
      Job.result num
    else
      let numFirst = num      * children
      let numLast  = numFirst + children - 1L
      let lvl1     = lvl - 1
      seq { numFirst .. numLast }
      |> Seq.Con.mapJob (skynet lvl1)
      >>- Seq.sum

  let run () =
    skynet 6 0L |> run

do for i=1 to 10 do
     time "Parallelish: " Parallelish.run

   for i=1 to 10 do
     time "Actorish:    " <| fun () ->
       Actorish.run 1000000L 10L
