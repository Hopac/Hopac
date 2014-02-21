// Copyright (C) by Housemarque, Inc.

namespace Hopac.Extra

open Hopac
open Hopac.Alt.Infixes
open Hopac.Job.Infixes

[<AutoOpen>]
module SwapChTypes =
  type SwapCh<'a> =
   | SwapCh of Ch<'a * Ch<'a>>

module SwapCh =
  let create () = Ch.create () |>> SwapCh
  let swap (SwapCh ch) (msgOut: 'a) : Alt<'a> =
    (Ch.Alt.take ch >=> fun (msgIn, outCh) -> Ch.give outCh msgOut >>% msgIn) <|>
    (Alt.delay <| fun () ->
     let inCh = Ch.Now.create ()
     Ch.Alt.give ch (msgOut, inCh) >=> fun () -> Ch.take inCh)
