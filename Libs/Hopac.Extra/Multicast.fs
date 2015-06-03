// Copyright (C) by Housemarque, Inc.

namespace Hopac.Extra

open Hopac
open Hopac.Infixes
open Hopac.Job.Infixes
open Hopac.Alt.Infixes

type MPort<'a> =
 | MPort of Alt<'a>

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

  let create () = Job.delay <| fun () ->
    let mc = {ReqCh = Ch (); RepCh = Ch ()}
    let newPort v =
      let outCh = Ch ()
      let tee v = v >>= fun r -> outCh *<- r.Value >>% r.Next
      Job.iterateServer v tee >>%
      MPort outCh
    let server v =
      mc.ReqCh >>= function
       | NewPort ->
         newPort v >>= Ch.give mc.RepCh >>% v
       | Multicast x ->
         let nV = IVar ()
         v *<= {Value = x; Next = nV} >>% nV
    Job.iterateServer (IVar ()) server >>% mc

  let port mc = mc.ReqCh *<+ NewPort >>. mc.RepCh
  let multicast mc x = mc.ReqCh *<- Multicast x :> Job<_>
  let recv (MPort port) = port

(*
type Stream<'x> = {
  Value: 'x
  Next: IVar<Stream<'x>>
}

type MChan<'x> = MChan of MVar<IVar<Stream<'x>>>
type MPort<'x> = MPort of Alt<'x>

module Multicast =
  module Now =
    let create () = MChan (MVar (IVar ()))

  let create () = Job.thunk Now.create

  let port (MChan xMc) =
    MVar.read xMc >>= fun v ->
    let outCh = Ch ()
    Job.iterateServer v <| fun v ->
         v >>= fun r -> outCh *<- r.Value >>% r.Next
    >>% MPort outCh

  let multicast (MChan xMc) x =
    xMc >>= fun v ->
    let nV = IVar ()
    xMc *<<= nV >>.
    v *<= {Value = x; Next = nV}

  let recv (MPort xMp) = xMp
*)