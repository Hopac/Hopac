// Copyright (C) by Housemarque, Inc.

namespace Hopac.Extra

open Hopac
open Hopac.Infixes
open Hopac.Alt.Infixes
open Hopac.Job.Infixes

type SwapCh<'a> =
 | SwapCh of Ch<'a * Ch<'a>>

module SwapCh =
  module Now =
    let create () = SwapCh (ch ())
  let create () = Job.thunk Now.create
  let swap (SwapCh sCh) (msgOut: 'a) : Alt<'a> =
    (sCh >>=? fun (msgIn, outCh) -> outCh <-- msgOut >>% msgIn) <|>?
    (Alt.delay <| fun () ->
     let inCh = ch ()
     sCh <-- (msgOut, inCh) >>.? inCh)
