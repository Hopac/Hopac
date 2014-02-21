// Copyright (C) by Housemarque, Inc.

namespace Hopac.Extra

open Hopac

[<AutoOpen>]
module SerializerTypes =
  type Serializer = Serializer of Ch<Job<unit>>

module Serializer =
  open Hopac
  open Hopac.Job.Infixes

  let create =
    Ch.create () >>= fun reqCh ->
    Ch.take reqCh |>> Job.forever |> Job.start >>%
    Serializer reqCh
  let serialize (Serializer reqCh) job =
    IVar.create () >>= fun reply ->
    job >>= IVar.fill reply
    |> Ch.give reqCh >>. IVar.read reply
