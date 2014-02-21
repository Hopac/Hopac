// Copyright (C) by Housemarque, Inc.

namespace Hopac.Extra

open Hopac
open Hopac.Job.Infixes
open Hopac.Alt.Infixes

module Alt =
  let wrapAbort (abortAct: Job<unit>) (evt: Alt<'x>) : Alt<'x> =
    Alt.withNack <| fun abortEvt ->
    Job.start (Alt.pick abortEvt >>. abortAct) >>% evt

  module Infixes =
    let (<+>) (xA: Alt<'x>) (yA: Alt<'y>) : Alt<'x * 'y> =
          (xA >=> fun x -> Alt.pick (yA >-> fun y -> (x, y)))
      <|> (yA >=> fun y -> Alt.pick (xA >-> fun x -> (x, y)))
