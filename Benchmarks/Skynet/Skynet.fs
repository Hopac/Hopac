// Copyright (C) by Vesa Karvonen

module Skynet

open Hopac
open Hopac.Infixes
open System
open System.Diagnostics

// This is a quick translation from the Erlang version:
//
//   https://github.com/atemerev/skynet/blob/master/erlang/skynet.erl

let rec skynet parent num size div = Job.delay <| fun () ->
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

let start size div =
  let result = Ch ()
  Job.queue (skynet result 0L size div) >>=.
  result

let time name skynet =
  let timer = Stopwatch.StartNew ()
  let sum = skynet ()
  printfn "%s: %d %A" name sum timer.Elapsed

do for i=1 to 10 do
     time "Skynet" <| fun () ->
       start 1000000L 10L |> run
     GC.Collect ()
     Threading.Thread.Sleep 50
