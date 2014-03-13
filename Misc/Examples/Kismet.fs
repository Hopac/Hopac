module Kismet

open System
open Hopac
open Hopac.Alt
open Hopac.Alt.Infixes
open Hopac.Job
open Hopac.Job.Infixes

// Inspired by http://upload.wikimedia.org/wikipedia/en/e/e6/Kismet_Roboblitz.PNG

let CompareBool comparand input onTrue onFalse =
  pick input >>= fun x ->
  if !comparand then onTrue x else onFalse x

let Delay duration start stop finished aborted =
  pick start >>= fun x ->
  select [stop                >=> fun y -> aborted y
          timeOut (!duration) >=> fun () -> finished x]

let Set value target input output =
  pick input >>= fun x ->
  target := value
  output x

let setup () = job {
  let ch_1 = Ch.Now.create ()
  let ch_2 = Ch.Now.create ()
  let ch_3 = Ch.Now.create ()
  // ...
  let bMoved = ref false
  // ...
  do! CompareBool bMoved
                  (Ch.Alt.take ch_1)
                  (Ch.give ch_2)
                  (fun _ -> Job.unit)
      |> Job.forever |> Job.server
  do! Delay (ref (TimeSpan.FromSeconds 3.14))
            (Ch.Alt.take ch_2)
            (Alt.never ())
            (Ch.give ch_3)
            (fun _ -> Job.unit)
      |> Job.forever |> Job.server
  // ...
}
