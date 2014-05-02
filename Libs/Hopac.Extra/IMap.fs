// Copyright (C) by Housemarque, Inc.

namespace Hopac.Extra

open System.Collections.Generic
open Hopac
open Hopac.Infixes

type IMap<'k, 'v> = {Map: Dictionary<'k, IVar<'v>>}

module IMap =
  let create () = Job.thunk <| fun () ->
    {Map = Dictionary<_, _>()}

  let fill kvM k v = Job.delay <| fun () ->
    let k2vI = kvM.Map
    lock k2vI <| fun () ->
    match k2vI.TryGetValue k with
     | (false, _) -> k2vI.Add (k, IVar.Now.createFull v) ; Job.unit ()
     | (true, vI) -> vI <-= v

  module Alt =
    let read kvM k = Alt.delay <| fun () ->
      let k2vI = kvM.Map
      lock k2vI <| fun () ->
      match k2vI.TryGetValue k with
       | (false, _) -> let vI = ivar () in k2vI.Add (k, vI) ; upcast vI
       | (true, vI) -> upcast vI
