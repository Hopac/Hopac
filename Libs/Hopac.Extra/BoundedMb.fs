// Copyright (C) by Vesa Karvonen

namespace Hopac.Extra

open System.Collections.Generic
open Hopac
open Hopac.Infixes
open Hopac.Job.Infixes
open Hopac.Alt.Infixes

type BoundedMb<'x> = {putCh: Ch<'x>; takeCh: Ch<'x>}

module BoundedMb =
  let create capacity =
    if capacity <= 0 then
      if capacity < 0 then
        failwithf "Negative capacity"
      else
        Job.thunk <| fun () ->
        let theCh = ch ()
        {putCh = theCh; takeCh = theCh}
    else
      Job.delay <| fun () ->
      let self = {putCh = ch (); takeCh = ch ()}
      let queue = Queue<_>()
      let put = self.putCh |>>? queue.Enqueue
      let take () = self.takeCh <-- queue.Peek () |>>? (queue.Dequeue >> ignore)
      let proc = Job.delay <| fun () ->
        match queue.Count with
         | 0 -> put
         | n when n = capacity -> take ()
         | _ -> take () <|>? put
      Job.foreverServer proc >>% self

  let put xB x = xB.putCh <-- x
  let take xB = xB.takeCh :> Alt<_>
