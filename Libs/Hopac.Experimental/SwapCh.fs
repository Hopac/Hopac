// Copyright (C) by Housemarque, Inc.

namespace Hopac.Experimental

open Hopac
open Hopac.Infixes

type SwapCh<'a> =
 | SwapCh of Ch<'a * IVar<'a>>

module SwapCh =
  module Now =
    let create () = SwapCh (Ch ())
  let create () = Job.thunk Now.create
  let swap (SwapCh sCh) (msgOut: 'a) : Alt<'a> =
        sCh ^=> fun (msgIn, outI) -> outI *<= msgOut >>-. msgIn
    <|> sCh *<-=>- fun inI -> (msgOut, inI)
