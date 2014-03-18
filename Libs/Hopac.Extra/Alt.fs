// Copyright (C) by Housemarque, Inc.

namespace Hopac.Extra

open Hopac
open Hopac.Job.Infixes
open Hopac.Alt.Infixes

module Alt =
  let wrapAbort (abortAct: Job<unit>) (evt: Alt<'x>) : Alt<'x> =
    Alt.withNack <| fun abortEvt ->
    Job.start (abortEvt >>. abortAct) >>% evt

  module Infixes =
    let (<+>) (xA: Alt<'x>) (yA: Alt<'y>) : Alt<'x * 'y> =
          (xA >=> fun x -> (yA >-> fun y -> (x, y)) :> Job<_>)
      <|> (yA >=> fun y -> (xA >-> fun x -> (x, y)) :> Job<_>)
