// Copyright (C) by Housemarque, Inc.

namespace Hopac.Extra

open Hopac

[<AutoOpen>]
module MulticastTypes =
  type MPort<'a> =
   | MPort of Ch<'a>

  type Request<'a> =
   | Multicast of 'a
   | NewPort

  type MChan<'a> = {
    RequestCh: Ch<Request<'a>>
    ReplyCh: Ch<MPort<'a>>
  }

  type Stream<'a> = {
    Value: 'a
    Next: IVar<Stream<'a>>
  }

module Multicast =
  open Hopac.Job.Infixes

  let create () : Job<MChan<'a>> = Job.delay <| fun () ->
    let requestCh = Ch.Now.create ()
    let replyCh = Ch.Now.create ()
    let newPort v =
      let outCh = Ch.Now.create ()
      let rec tee v =
        IVar.read v >>= fun r ->
        Ch.give outCh r.Value >>= fun () ->
        tee r.Next
      Job.server (tee v) >>%
      MPort outCh
    let rec server v =
      Ch.take requestCh >>= function
       | NewPort ->
         newPort v >>=
         Ch.give replyCh >>= fun () ->
         server v
       | Multicast x ->
         let nV = IVar.Now.create ()
         IVar.fill v {Value = x; Next = nV} >>= fun () ->
         server nV
    Job.server (IVar.create () >>= server) >>%
    {RequestCh = requestCh; ReplyCh = replyCh}

  let port (mc: MChan<'a>) : Job<MPort<'a>> =
    Ch.give mc.RequestCh NewPort >>.
    Ch.take mc.ReplyCh

  let multicast (mc: MChan<'a>) (x: 'a) : Job<unit> =
    Ch.give mc.RequestCh (Multicast x)

  let recv (MPort port) =
    Ch.take port

  module Alt =
    let recv (MPort port) =
      Ch.Alt.take port
