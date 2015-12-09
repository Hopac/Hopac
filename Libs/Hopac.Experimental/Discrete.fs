// Copyright (C) by Vesa Karvonen

namespace Hopac.Experimental

open Hopac
open Hopac.Infixes

module Alt =
  module Discrete =
    // The following are "primitive" operations of the discrete event source
    // subset of alternatives.  Alt.once and Alt.never are also primitives, but
    // they are defined elsewhere.

    let start (abort2xJ: Alt<'x> -> #Job<'x>) : Alt<'x> =
      Alt.withNackJob <| fun nack ->
      Promise.queue (abort2xJ (nack ^=>. Job.abort))

    let merge (lhs: Alt<'x>) (rhs: Alt<'x>) =
      start <| fun abort -> lhs <|> rhs <|> abort

    let throttle (timeout: Alt<_>) (xA: Alt<'x>) =
      let rec lp abort x = xA ^=> lp abort <|> timeout ^->. x <|> abort
      start <| fun abort -> xA ^=> lp abort <|> abort

    let switchMap (x2yA: 'x -> Alt<'y>) (xA: Alt<'x>) =
      let rec lp abort yA = yA <|> xA ^=> (x2yA >> lp abort) <|> abort
      start <| fun abort -> lp abort abort

    let combineLatest (xE: Alt<'x>) (yE: Alt<'y>) : Alt<'x * 'y> =
      let rec gotX a x = xE ^=> gotX a <|> a <|> yE ^-> fun y -> (x, y)
      let rec gotY a y = yE ^=> gotY a <|> a <|> xE ^-> fun x -> (x, y)
      start <| fun a -> xE ^=> gotX a <|> yE ^=> gotY a <|> a

    // The following do not need to be primitive operations.

    let choose (x2yO: 'x -> option<'y>) (xA: Alt<'x>) =
      xA
      |> switchMap (fun x ->
         match x2yO x with
          | None -> Alt.never
          | Some y -> Alt.once y)

    let filter x2b xA =
      xA
      |> choose (fun x ->
         if x2b x then Some x else None)

    let map x2y xA =
      xA
      |> choose (x2y >> Some)
