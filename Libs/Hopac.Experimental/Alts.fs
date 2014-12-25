// Copyright (C) by Vesa Karvonen

namespace Hopac.Experimental

open System
open Hopac
open Hopac.Job.Infixes
open Hopac.Alt.Infixes
open Hopac.Infixes

type AltsCons<'x> =
  | Next of 'x * Alt<AltsCons<'x>>
  | Done
and Alts<'x> = In of Alt<AltsCons<'x>>

module Alts =
  let inline out (In x) = x

  let inline Done' () = Job.result Done
  let inline Next' (x, xO) = Job.result <| Next (x, xO)

  let consume (onNext: 'x -> Job<unit>) (In xO: Alts<'x>) : Job<unit> =
    let rec loop xO =
      xO >>= function
       | Done -> Job.unit ()
       | Next (x, xO) -> onNext x >>= fun () -> loop xO
    loop xO

  // Primitives ----------------------------------------------------------------

  let zero () : Alts<'x> = In <| Alt.always Done

  let result x : Alts<'x> = In <| Alt.always (Next (x, Alt.always Done))

  let inline start f =
    Alt.withNack <| fun nack ->
    Promise.start (f (nack >>=? Job.abort))

  let rec mergeAbort yO1 yO2 abort =
    let inline case yO1 yO2 =
      yO1 >>=? function
        | Done -> yO2 <|>? abort :> Job<_>
        | Next (y, yO1) -> Next' (y, mergeOut yO1 yO2)
    case yO1 yO2 <|>? case yO2 yO1 <|>? abort

  and mergeOut xO1 xO2 =
    start <| mergeAbort xO1 xO2

  let merge (In xO1: Alts<'x>) (In xO2: Alts<'x>) : Alts<'x> =
    In <| mergeOut xO1 xO2

  let bindJob (In xO: Alts<'x>) (x2yOJ: 'x -> Job<Alts<'y>>) : Alts<'y> =
    let rec loop (xO: Alt<AltsCons<_>>) =
      start <| fun abort ->
      (xO >>=? function
        | Done -> Done' ()
        | Next (x, xO) ->
          x2yOJ x >>= fun (In yO) ->
          mergeAbort yO (loop xO) abort) <|>? abort
    In <| loop xO

  let processJob ( x2syOJ:       'x -> Job<'s * option<'y>>)
                 (sx2syOJ: 's -> 'x -> Job<'s * option<'y>>)
                 (In xO: Alts<'x>) : Alts<'y> =
   let rec choose xO abort = function
     | (s, None) -> fold xO s abort
     | (s, Some y) -> Next' (y, start <| fold xO s)
   and fold xO s abort =
     (xO >>=? function
       | Done -> Done' ()
       | Next (x, xO) ->
         sx2syOJ s x >>= choose xO abort) <|>? abort :> Job<_>
   let initial abort =
     (xO >>=? function
       | Done -> Done' ()
       | Next (x, xO) ->
         x2syOJ x >>= choose xO abort) <|>? abort
   In <| start initial

  let throttle (timeOut: Alt<unit>) (In xO: Alts<'x>) : Alts<'x> =
    let rec stabilize x xO abort =
      (xO >>=? function
        | Done -> Job.result Done
        | Next (x, xO) -> stabilize x xO abort) <|>?
      (timeOut |>>? fun () -> Next (x, initial xO)) <|>? abort :> Job<_>
    and initial xO =
      start <| fun abort ->
      xO >>= function
       | Done -> Job.result Done
       | Next (x, xO) -> stabilize x xO abort
    In <| initial xO

  // User definable ------------------------------------------------------------

  let scanJob (sx2sJ: 's -> 'x -> Job<'s>) (s: 's) (xO: Alts<'x>) : Alts<'s> =
    let inline sx2ssOJ s x = sx2sJ s x |>> fun s -> (s, Some s)
    processJob (sx2ssOJ s) sx2ssOJ xO

  let chooseJob (x2yOJ: 'x -> Job<option<'y>>) (xO: Alts<'x>) : Alts<'y> =
    let inline x2syOJ x = x2yOJ x >>= fun yO -> Job.result ((), yO)
    processJob x2syOJ (fun _ -> x2syOJ) xO

  let mapJob (x2yJ: 'x -> Job<'y>) (xO: Alts<'x>) : Alts<'y> =
    chooseJob (fun x -> x2yJ x |>> Some) xO

  let scanFun (sx2s: 's -> 'x -> 's) (s: 's) (xO: Alts<'x>) : Alts<'s> =
    scanJob (fun s x -> sx2s s x |> Job.result) s xO

  let mapFun (x2y: 'x -> 'y) (xO: Alts<'x>) : Alts<'y> =
    mapJob (x2y >> Job.result) xO

  let bindFun (xO: Alts<'x>) (x2yO: 'x -> Alts<'y>) : Alts<'y> =
    bindJob xO (x2yO >> Job.result)

  let chooseFun (x2yO: 'x -> option<'y>) (xO: Alts<'x>) : Alts<'y> =
    chooseJob (x2yO >> Job.result) xO

  let noDups (xO: Alts<'x>) : Alts<'x> =
    processJob
      <| fun x -> Job.result (x, Some x)
      <| fun x' x -> Job.result (x, if x' = x then None else Some x)
      <| xO
