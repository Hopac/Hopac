// Copyright (C) by Housemarque, Inc.

namespace Hopac.Extra

open Hopac
open Hopac.Infixes
open Hopac.Extra.Alt.Infixes
open Hopac.Alt.Infixes
open Hopac.Job.Infixes

module Stream =
  type In<'x> = Alt<'x>
  type Out<'x> = 'x -> Job<unit>

  let inline imp (mk: Out<_> -> #Job<unit>) = Job.delay <| fun () ->
    let ch = ch ()
    mk (fun x -> ch <-- x :> Job<_>) >>% (ch :> Alt<_>)

  let filterFun x2b (xIn: In<_>) (xOut: Out<_>) =
    Job.foreverServer
     (xIn >>= fun x -> if x2b x then xOut x else Job.unit ())

  let filterJob (x2bJ: 'x -> #Job<_>) (xIn: In<_>) (xOut: Out<_>) =
    Job.foreverServer
     (xIn >>= fun x ->
      x2bJ x >>= fun b ->
      if b then xOut x else Job.unit ())

  let iterateFun x x2x (xOut: Out<_>) =
    Job.iterateServer x (fun x -> xOut x >>% x2x x)

  let iterateJob x (x2xJ: _ -> #Job<_>) (xOut: Out<_>) =
    Job.iterateServer x (fun x -> xOut x >>. x2xJ x)

  let mapFun x2y (xIn: In<_>) (yOut: Out<_>) =
    Job.foreverServer (xIn >>= fun x -> yOut (x2y x))

  let mapJob (x2yJ: _ -> #Job<_>) (xIn: In<_>) (yOut: Out<_>) =
    Job.foreverServer (xIn >>= fun x -> x2yJ x >>= yOut)

  let sumWithFun xy2z (xIn: In<_>) (yIn: In<_>) (zOut: Out<_>) =
    Job.foreverServer (xIn <+> yIn >>= fun (x, y) -> zOut (xy2z x y))

  let sumWithJob (xy2zJ: _ -> _ -> #Job<_>) xIn yIn (zOut: Out<_>) =
    Job.foreverServer (xIn <+> yIn >>= fun (x, y) -> xy2zJ x y >>= zOut)
