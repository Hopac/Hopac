module Kismet

open Hopac
open Hopac.Alt.Infixes
open Hopac.Job.Infixes

// Inspired by http://upload.wikimedia.org/wikipedia/en/e/e6/Kismet_Roboblitz.PNG

let CompareBool comparand inCh onTrueCh onFalseCh =
  Job.forever
   (Ch.take inCh >>= fun x ->
    Ch.give (if !comparand then onTrueCh else onFalseCh) x)

let Delay duration startCh stopCh finishedCh abortedCh =
  let rec initial () = Ch.take startCh >>= started
  and started x =
    Alt.pick (Ch.Alt.take stopCh      >=> Ch.give abortedCh
          <|> Alt.timeOut (!duration) >=> fun () -> Ch.give finishedCh x) >>= initial
  initial ()

let Sink inCh = Job.forever (Ch.take inCh)

let Set value target inCh outCh =
  Job.forever
   (Ch.take inCh >>= fun x ->
    target := value
    Ch.give outCh x)
