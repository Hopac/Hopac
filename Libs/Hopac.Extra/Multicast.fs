// Copyright (C) by Housemarque, Inc.

namespace Hopac.Extra

open Hopac
open Hopac.Infixes
open Hopac.Job.Infixes
open Hopac.Alt.Infixes


type MPort<'a> =
 | MPort of Ch<'a>

type Req<'a> =
 | Multicast of 'a
 | NewPort

type MChan<'a> = {
  ReqCh: Ch<Req<'a>>
  RepCh: Ch<MPort<'a>>
}

module Multicast =
  type Stream<'a> = {
    Value: 'a
    Next: IVar<Stream<'a>>
  }

  let create () : Job<MChan<'a>> = Job.delay <| fun () ->
    let mc = {ReqCh = ch (); RepCh = ch ()}
    let newPort v =
      let outCh = ch ()
      let tee v = v >>= fun r -> outCh <-- r.Value >>% r.Next
      Job.iterateServer v tee >>%
      MPort outCh
    let server v =
      mc.ReqCh >>= function
       | NewPort ->
         newPort v >>= Ch.give mc.RepCh >>% v
       | Multicast x ->
         let nV = ivar ()
         v <-= {Value = x; Next = nV} >>% nV
    Job.iterateServer (ivar ()) server >>% mc

  let port (mc: MChan<'a>) : Job<MPort<'a>> =
    mc.ReqCh <-+ NewPort >>.
    asJob mc.RepCh

  let multicast (mc: MChan<'a>) (x: 'a) : Alt<unit> =
    mc.ReqCh <-- Multicast x

  let recv (MPort port) =
    asAlt port

(*
type Stream<'x> = {
  Value: 'x
  Next: IVar<Stream<'x>>
}

type MChan<'x> = MChan of MVar<IVar<Stream<'x>>>
type MPort<'x> = MPort of Ch<'x>

module Multicast =
  module Now =
    let create () = MChan (mvarFull (ivar ()))

  let create () = Job.thunk Now.create

  let port (MChan xMc) =
    MVar.read xMc >>= fun v ->
    let outCh = ch ()
    let tee v = v >>= fun r -> outCh <-- r.Value >>% r.Next
    Job.iterateServer v tee >>%
    MPort outCh

  let multicast (MChan xMc) x =
    xMc >>= fun v ->
    let nV = ivar ()
    (xMc <<-= nV) >>.
    (v <-= {Value = x; Next = nV})

  let recv (MPort xMp) = asJob xMp

  module Alt =
    let recv (MPort xMp) = asAlt xMp
*)
