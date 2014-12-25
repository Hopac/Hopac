// Copyright (C) by Housemarque, Inc.

namespace Models

module ActorModel =
  open Hopac
  open Hopac.Job.Infixes

  type ActorThread<'a, 'x> = AT of (Ch<'a> -> Job<'x>)
  let unAT (AT x) = x
  let (>>=) (xA: ActorThread<'a, 'x>) (x2yA: 'x -> ActorThread<'a, 'y>) : ActorThread<'a, 'y> =
    AT (fun aCh -> unAT xA aCh >>= fun x -> unAT (x2yA x) aCh)
  let result (x: 'x) : ActorThread<'a, 'x> =
    AT (fun aCh -> Job.result x)
  let receive : ActorThread<'a, 'a> =
    AT (fun aCh -> aCh :> Job<_>)
  type Actor<'a> = A of Ch<'a>
  let unA (A aCh) = aCh
  let self : ActorThread<'a, Actor<'a>> =
    AT (fun aCh -> Job.result (A aCh))
  let start (uA: ActorThread<'a, unit>) : Actor<'a> =
    let aCh = Ch.Now.create ()
    Job.Global.start (unAT uA aCh)
    A aCh
  let send (aA: Actor<'a>) (a: 'a) : unit =
    Job.Global.start (Ch.give (unA aA) a)

module HopacModel =
  open System.Collections.Generic
  open ActorModel

  type Job<'x> = J of ActorThread<unit, 'x>
  let unJ (J x) = x
  type Msg<'x> =
    | Take of Actor<unit> * ref<option<'x>>
    | Give of Actor<unit> * 'x
  type Ch<'x> = C of Actor<Msg<'x>>
  let unC (C x) = x
  let ch () : Ch<'x> =
    let givers = Queue<Actor<unit> * 'x>()
    let takers = Queue<Actor<unit> * ref<option<'x>>>()
    let rec loop () =
      receive >>= function
       | Give (giver, x) ->
         if takers.Count > 0 then
           let (taker, r) = takers.Dequeue ()
           r := Some x
           send giver ()
           send taker ()
           loop ()
         else
           givers.Enqueue (giver, x)
           loop ()
       | Take (taker, r) ->
         if givers.Count > 0 then
           let (giver, x) = givers.Dequeue ()
           r := Some x
           send giver ()
           send taker ()
           loop ()
         else
           takers.Enqueue (taker, r)
           loop ()
    C (start (loop ()))
  let give (xCh: Ch<'x>) (x: 'x) : Job<unit> =
    J (self >>= fun giver ->
       send (unC xCh) (Give (giver, x))
       receive)
  let take (xCh: Ch<'x>) : Job<'x> =
    J (self >>= fun taker ->
       let r = ref None
       send (unC xCh) (Take (taker, r))
       receive >>= fun () ->
       match !r with
        | None -> failwith "Impossible"
        | Some x -> result x)
  let (>>=) (xJ: Job<'x>) (x2yJ: 'x -> Job<'y>) : Job<'y> =
    J (unJ xJ >>= fun x -> unJ (x2yJ x))
  let result (x: 'x) : Job<'x> =
    J (result x)
  let start (uJ: Job<unit>) : unit =
    start (unJ uJ) |> ignore
