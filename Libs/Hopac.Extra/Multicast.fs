// Copyright (C) by Housemarque, Inc.

namespace Hopac.Extra

open Hopac
open Hopac.Infixes
open Hopac.Job.Infixes

type MPort<'a> =
 | MPort of Ch<'a>

type Req<'a> =
 | Multicast of 'a
 | NewPort

type MChan<'a> = {
  ReqCh: Ch<Req<'a>>
  RepCh: Ch<MPort<'a>>
}

type Stream<'a> = {
  Value: 'a
  Next: IVar<Stream<'a>>
}

module Multicast =
  let create () : Job<MChan<'a>> = Job.delay <| fun () ->
    let mc = {ReqCh = ch (); RepCh = ch ()}
    let newPort v =
      let outCh = ch ()
      let tee v = v >>= fun r -> outCh <-- r.Value >>% r.Next
      Job.server (Job.iterate v tee) >>%
      MPort outCh
    let server v =
      mc.ReqCh >>= function
       | NewPort ->
         newPort v >>= Ch.give mc.RepCh >>% v
       | Multicast x ->
         let nV = ivar ()
         v <-= {Value = x; Next = nV} >>% nV
    Job.server (Job.iterate (ivar ()) server) >>% mc

  let port (mc: MChan<'a>) : Job<MPort<'a>> =
    mc.ReqCh <-+ NewPort >>.
    asJob mc.RepCh

  let multicast (mc: MChan<'a>) (x: 'a) : Job<unit> =
    mc.ReqCh <-- Multicast x

  let recv (MPort port) =
    asJob port

  module Alt =
    let recv (MPort port) =
      asAlt port
