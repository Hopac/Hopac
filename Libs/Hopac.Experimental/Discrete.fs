// Copyright (C) by Vesa Karvonen

namespace Hopac.Experimental

open Hopac
open Hopac.Alt.Infixes
open Hopac.Job.Infixes
open Hopac.Infixes

module Alt =
  module Discrete =
    let start (abort2xJ: Alt<'x> -> #Job<'x>) : Alt<'x> =
      Alt.withNack <| fun nack ->
      Promise.queue (abort2xJ (nack >>=? Job.abort))

    let merge (lhs: Alt<'x>) (rhs: Alt<'x>) =
      start <| fun abort -> lhs <|>? rhs <|>? abort

    let mergeMap (x2yA: 'x -> Alt<'y>) (xA: Alt<'x>) =
      let rec loop yA = (xA >>=? fun x -> loop (x2yA x <|>? yA)) <|>? yA
      start loop

    let choose (x2yO: 'x -> option<'y>) (xA: Alt<'x>) =
      xA
      |> mergeMap (fun x ->
         match x2yO x with
          | None -> Alt.never ()
          | Some y -> Alt.always y)

    let filter x2b xA = choose (fun x -> if x2b x then Some x else None) xA

    let map x2y xA = choose (x2y >> Some) xA

    let throttle (timeout: Alt<_>) (xA: Alt<'x>) =
      let rec loop abort x = (xA >>=? loop abort) <|>? (timeout >>%? x)
      start <| fun abort -> xA >>=? loop abort <|>? abort
