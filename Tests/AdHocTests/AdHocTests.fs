// Copyright (C) by Housemarque, Inc.

module AdHocTests

open System
open Hopac
open Hopac.Job
open Hopac.Job.Infixes

module Exn =
  let reflect = function
    | Choice1Of2 x -> x
    | Choice2Of2 e -> raise e

type Recorder () =
  let mutable results : list<string * obj> = []
  member self.Got (tag: string, value: 'a) =
   lock self <| fun () ->
   results <- (tag, box value)::results

let test (r: Recorder) c1 c2 c3 =
  tryIn (delay <| fun () ->
         r.Got ("b", ())
         result (Exn.reflect c1))
   <| fun x -> r.Got ("r", x) ; Exn.reflect c2
   <| fun e -> r.Got ("e", e) ; Exn.reflect c3
