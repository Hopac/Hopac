// Copyright (C) by Housemarque, Inc.

namespace Hopac.Extra

open Hopac
open Hopac.Extra.Alt.Infixes
open Hopac.Alt.Infixes
open Hopac.Job.Infixes

module Ch =
  module Stream =
    let inline imp (mk: Ch<_> -> Job<unit>) = Job.delay <| fun () ->
      let ch = Ch.Now.create ()
      mk ch >>% ch

    let sumWith (xy2zJ: _ -> _ -> Job<_>) xCh yCh zCh =
      let xAlt = Ch.Alt.take xCh
      let yAlt = Ch.Alt.take yCh
      Alt.pick (xAlt <+> yAlt >=> fun (x, y) -> xy2zJ x y >>= Ch.give zCh)
      |> Job.forever |> Job.start

    let iterate init (step: _ -> Job<_>) outCh =
      Job.iterate init (fun x -> Ch.give outCh x >>. step x)
      |> Job.start

    let filter (pred: _ -> Job<_>) inCh outCh =
      (Ch.take inCh >>= fun x ->
       pred x >>= fun b ->
       Job.whenDo b (Ch.give outCh x))
      |> Job.forever |> Job.start
