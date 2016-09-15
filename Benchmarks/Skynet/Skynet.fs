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
  //
  // Note that `Seq.Con.mapJob` is not really a parallel combinator.  It treats
  // each started job as a separate concurrent job and iterating over a sequence
  // is a sequential bottle neck.  It would be possible to implement faster
  // parallel combinators.

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

module Variablish =
  // This is inspired by the Haskell version:
  //
  //   https://github.com/atemerev/skynet/blob/master/haskell/src/MVar.hs
  //
  // Compared to Actorish and Parallelish this is a bit slower due to having to
  // allocate more result cells.  This uses IVars instead of Chs or MVars (Note
  // that Hopac MVar is not the same as Concurrent Haskell MVar).  IVars are a
  // bit faster than Chs and MVars.

  let children = 10L

  let inline plus1 n rc = rc >>- (+) n

  let rec forkSkynet lvl num =
    let rc = IVar ()
    Job.queue (skynet rc lvl num) >>-. rc

  and skynet c lvl num =
    if lvl = 0 then
      c *<= num
    else
      let numFirst = num      * children
      let numLast  = numFirst + children - 1L
      let lvl1     = lvl - 1
      seq { numFirst .. numLast }
      |> Seq.mapJob (forkSkynet lvl1)
      >>= Seq.foldJob plus1 0L
      >>= IVar.fill c

  let run () =
    forkSkynet 6 0L
    |> Job.join
    |> run

module Promisish =
  // This is inspired by the Haskell version:
  //
  //   https://github.com/atemerev/skynet/blob/master/haskell/src/MVar.hs
  //
  // Compared to Actorish and Parallelish this is a bit slower due to having to
  // allocate more result cells, but promises are a bit faster than using an
  // IVar like in Variablish.

  let children = 10L

  let inline plus1 n rc = rc >>- (+) n

  let rec forkSkynet lvl num =
    Promise.queue (skynet lvl num)

  and skynet lvl num =
    if lvl = 0 then
      Job.result num
    else
      let numFirst = num      * children
      let numLast  = numFirst + children - 1L
      let lvl1     = lvl - 1
      seq { numFirst .. numLast }
      |> Seq.mapJob (forkSkynet lvl1)
      >>= Seq.foldJob plus1 0L

  let run () =
    forkSkynet 6 0L
    |> Job.join
    |> run

do for i=1 to 10 do
     time "Parallelish: " Parallelish.run

   for i=1 to 10 do
     time "Actorish:    " <| fun () ->
       Actorish.run 1000000L 10L

   for i=1 to 10 do
     time "Variablish:  " Variablish.run

   for i=1 to 10 do
     time "Promisish:   " Promisish.run
