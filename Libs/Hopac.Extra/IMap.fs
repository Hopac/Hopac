// Copyright (C) by Housemarque, Inc.

namespace Hopac.Extra

open System.Collections.Generic
open Hopac
open Hopac.Infixes
open Hopac.Extensions

type IMap<'k, 'v> = {
    mutable Closed: Alt<option<'v>>
    Map: Dictionary<'k, IVar<option<'v>>>
  }

module IMap =
  module Now = 
    let create () = {Closed = null; Map = Dictionary<_, _>()}

  let create () = Job.thunk Now.create

  let close kvM = Job.delay <| fun () ->
    let k2vI = kvM.Map
    let flushed = ResizeArray<_> ()
    lock k2vI <| fun () ->
      match kvM.Closed with
       | null ->
         for kvI in k2vI do
           let vI = kvI.Value
           if not (IVar.Now.isFull vI) then
             flushed.Add kvI
         for kvI in flushed do
           k2vI.Remove kvI.Key |> ignore
         // Must be last as queries may interfere.
         kvM.Closed <- Alt.always None
       | _ ->
         ()
    flushed
    |> Seq.iterJob (fun kvI ->
       kvI.Value <-= None)

  let fill kvM k v = Job.delay <| fun () ->
    let k2vI = kvM.Map
    lock k2vI <| fun () ->
    match kvM.Closed with
     | null -> ()
     | _ -> failwith "Map is closed."
    match k2vI.TryGetValue k with
     | (false, _) ->
       k2vI.Add (k, IVar.Now.createFull (Some v))
       Job.unit ()
     | (true, vI) ->
       if IVar.Now.isFull vI then
         failwithf "Tried to fill item %A twice." k
       vI <-= Some v

  let query kvM k =
    match kvM.Closed with
     | null -> Alt.delay <| fun () ->
       let k2vI = kvM.Map
       lock k2vI <| fun () ->
       match k2vI.TryGetValue k with
        | (false, _) ->
          match kvM.Closed with
           | null ->
             let vI = ivar ()
             k2vI.Add (k, vI)
             vI :> Alt<_>
           | vA ->
             vA
        | (true, vI) ->
          vI :> Alt<_>
     | vI ->
       match kvM.Map.TryGetValue k with
        | (false, _) -> vI
        | (true, vI) -> upcast vI
