module Kismet

open System
open System.Collections.Generic
open Hopac
open Hopac.Infixes
open Hopac.Alt
open Hopac.Alt.Infixes
open Hopac.Job
open Hopac.Job.Infixes
open Hopac.Extensions

// Inspired by http://upload.wikimedia.org/wikipedia/en/e/e6/Kismet_Roboblitz.PNG

module GameTime =
  type Ticks = int64
  let mutable internal currentTime : Ticks = 0L

  let internal timerReqCh : Ch<Ticks * IVar<unit>> = ch ()

  let atTime (atTime: Ticks) : Alt<unit> =
    Alt.guard << Job.delay <| fun () ->
    let replyI = ivar ()
    timerReqCh <-+ (atTime, replyI) >>%
    IVar.read replyI

  let timeOut (afterTicks: Ticks) : Alt<unit> =
    assert (0L <= afterTicks)
    Alt.delay <| fun () ->
    atTime (currentTime + afterTicks)

  let internal requests = Dictionary<Ticks, ResizeArray<IVar<unit>>> ()

  let internal timeReqServer =
    timerReqCh >>= fun (atTime, replyI) ->
    if currentTime <= atTime then
      replyI <-= ()
    else
      let replyIs =
        match requests.TryGetValue atTime with
         | (true, replyIs) -> replyIs
         | _ ->
           let replyIs = ResizeArray<_>()
           requests.Add (atTime, replyIs)
           replyIs
      replyIs.Add replyI
      Job.unit ()

  let tick = Job.delay <| fun () ->
    currentTime <- currentTime + 1L
    match requests.TryGetValue currentTime with
     | (true, replyIs) ->
       requests.Remove currentTime |> ignore
       replyIs
       |> Seq.iterJob (fun replyI -> replyI <-= ())
     | _ ->
       Job.unit ()

let CompareBool comparand input onTrue onFalse =
  input >>= fun x ->
  if !comparand then onTrue x else onFalse x

let Delay duration start stop finished aborted =
  start >>= fun x ->
  choose [stop                         >>=? aborted
          GameTime.timeOut (!duration) >>=? fun () -> finished x]

let Set value target input output =
  input >>= fun x ->
  target := value
  output x

let setup () = job {
  let ch_1 = ch ()
  let ch_2 = ch ()
  let ch_3 = ch ()
  // ...
  let bMoved = ref false
  // ...
  do! CompareBool bMoved
                  ch_1
                  (fun x -> ch_2 <-- x :> Job<_>)
                  (fun _ -> Job.unit ())
      |> Job.foreverServer
  do! Delay (ref 314L)
            ch_2
            (Alt.never ())
            (Ch.give ch_3)
            (fun _ -> Job.unit ())
      |> Job.foreverServer
  // ...
}
