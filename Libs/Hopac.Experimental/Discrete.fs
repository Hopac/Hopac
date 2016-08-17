// Copyright (C) by Vesa Karvonen

namespace Hopac.Experimental

open Hopac
open Hopac.Infixes

module Alt =
  module Discrete =
    let inline (^) x = x

    let inline start (abort2xJ: Alt<'x> -> #Job<'x>) : Alt<'x> =
      Alt.withNackJob ^ fun nack ->
      Promise.queue ^ abort2xJ ^ nack ^=> Job.abort

    // The following are "primitive" operations of the discrete event source
    // subset of alternatives.  Alt.once and Alt.never are also primitives, but
    // they are defined elsewhere.

    let never<'x> = Alt.never () :> Alt<'x>

    let merge lhs rhs =
      lhs <|> rhs

    let once x = Alt.once x

    let switchMap (x2yE: 'x -> Alt<'y>) (xE: Alt<'x>) =
      let rec lp abort yE = yE <|> xE ^=> (x2yE >> lp abort) <|> abort
      start ^ fun abort -> lp abort abort

    // The following do not need to be primitive operations.

    let combineLatest xE yE =
      merge (xE |> switchMap ^ fun x -> yE ^-> fun y -> (x, y))
            (yE |> switchMap ^ fun y -> xE ^-> fun x -> (x, y))

    let choose (x2yO: 'x -> option<'y>) (xE: Alt<'x>) =
      xE
      |> switchMap ^ fun x ->
           match x2yO x with
            | None   -> never
            | Some y -> once y

    let filter x2b xE =
      xE
      |> choose ^ fun x ->
           if x2b x then Some x else None

    //let map x2y xE =
    //  xE
    //  |> choose (x2y >> Some)
    let map x2y xE = Alt.afterFun x2y xE

    let debounce (timeout: Alt<_>) (xE: Alt<'x>) =
      xE
      |> switchMap ^ (^->.) timeout
