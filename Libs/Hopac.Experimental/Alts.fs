// Copyright (C) by Vesa Karvonen

namespace Hopac.Experimental

open System
open Hopac
open Hopac.Job.Infixes
open Hopac.Alt.Infixes
open Hopac.Infixes

type AltsCons<'x> =
  | Next of 'x * Alt<AltsCons<'x>>
  | Error of exn
  | Done
and Alts<'x> = In of Alt<AltsCons<'x>>

module Alts =
  let inline out (In x) = x

  let consume (onNext: 'x -> Job<unit>)
              (onError: exn -> Job<unit>)
              (onDone: Job<unit>)
              (In xO: Alts<'x>) : Job<unit> =
    let rec loop xO =
      xO >>= function
       | Done -> onDone
       | Error e -> onError e
       | Next (x, xO) -> onNext x >>= fun () -> loop xO
    loop xO

  let zero () : Alts<'x> = In <| Alt.always Done

  let result x : Alts<'x> = In <| Alt.always (Next (x, Alt.always Done))

  let inline start f =
    Alt.withNack <| fun nack ->
    let rI = ivar ()
    Job.start (f nack rI) >>%
    upcast rI

  let rec mergeTo yO1 yO2 nack yI =
    let inline case yO1 yO2 =
      yO1 >>=? function
        | Error e ->
          yI <-= Error e
        | Done ->
          (yO2 >>=? fun y -> yI <-= y) <|> nack
        | Next (y, yO1) ->
          yI <-= Next (y, merge' yO1 yO2)
    case yO1 yO2 <|>? case yO2 yO1 <|> nack

  and merge' xO1 xO2 =
    start (mergeTo xO1 xO2)

  let merge (In xO1: Alts<'x>) (In xO2: Alts<'x>) : Alts<'x> =
    In <| merge' xO1 xO2

  let bindFun (In xO: Alts<'x>) (x2yO: 'x -> Alts<'y>) : Alts<'y> =
    let rec loop xO =
      start <| fun nack yI ->
      (xO >>=? function
        | Done -> yI <-= Done
        | Error e -> yI <-= Error e
        | Next (x, xO) ->
          mergeTo (x2yO x |> out) (loop xO) nack yI) <|> nack
    In <| loop xO

  let throttle (span: TimeSpan) (In xO: Alts<'x>) : Alts<'x> =
    let spanAlt = Timer.Global.timeOut span
    let rec stabilize x xO nack xI =
      (xO >>=? function
        | (Done | Error _) as x -> xI <-= x
        | Next (x, xO) -> stabilize x xO nack xI) <|>?
      (spanAlt >>=? fun () -> xI <-= Next (x, initial xO)) <|> nack
    and initial xO =
      start <| fun nack xI ->
      xO >>= function
       | (Done | Error _) as x -> xI <-= x
       | Next (x, xO) -> stabilize x xO nack xI
    In <| initial xO

  let noDups (In xO: Alts<'x>) : Alts<'x> =
    let rec lastWas x' xO nack xI =
      (xO >>=? function
        | (Done | Error _) as x -> xI <-= x
        | Next (x, xO) ->
          if x' = x then
            lastWas x xO nack xI
          else
            xI <-= produce x xO) <|> nack
    and produce x xO =
      Next (x, start (lastWas x xO))
    In (xO |>>? function
         | (Done | Error _) as x -> x
         | Next (x, xO) -> produce x xO)

  let mapJob (x2yJ: 'x -> Job<'y>) (In xO: Alts<'x>) : Alts<'y> =
    let rec loop xO =
      start <| fun nack yI ->
      (xO >>=? function
        | Done -> yI <-= Done
        | Error e -> yI <-= Error e
        | Next (x, xO) ->
          x2yJ x >>= fun y ->
          yI <-= Next (y, loop xO)) <|> nack
    In <| loop xO

  let mapFun (x2y: 'x -> 'y) (In xO: Alts<'x>) : Alts<'y> =
    let rec loop xO =
      xO |>>? function
       | Done -> Done
       | Error e -> Error e
       | Next (x, xO) -> Next (x2y x, loop xO)
    In <| loop xO

  let foldFun (sx2s: 's -> 'x -> 's) (s: 's) (In xO: Alts<'x>) : Alts<'s> =
    let rec loop s xO =
      xO |>>? function
       | Done -> Done
       | Error e -> Error e
       | Next (x, xO) ->
         let s = sx2s s x
         Next (s, loop s xO)
    In <| Alt.always (Next (s, loop s xO))
