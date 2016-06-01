// Copyright (C) by Vesa Karvonen

[<AutoOpen>]
module Util

open System
open System.Collections.Generic

let inline (^) x = x

let testEq exp act =
  if exp <> act
  then printfn "Expected %A, but got %A" exp act
  else printfn "OK"

exception Expected of int

let rec getRootCauses (d: Dictionary<_, _>)  (e: exn) =
  match e with
   | :? AggregateException as e ->
     e.InnerExceptions
     |> Seq.iter ^ getRootCauses d
   | _ ->
     match e.InnerException with
      | null ->
        match d.TryGetValue e with
         | true, n -> d.[e] <- n + 1
         | _ -> d.Add (e, 1) |> ignore
      | e ->
        getRootCauses d e

let testExpected expected = function
  | Choice1Of2 _ -> printfn "Unexpected success"
  | Choice2Of2 e ->
    let d = Dictionary ()
    getRootCauses d e
    for e in expected do
      match d.TryGetValue e with
       | true, n when n > 0 -> d.[e] <- n - 1
       | _ -> printfn "Expected %A, but got none" e
    for kv in d do
      if kv.Value <> 0 then
        printfn "Didn't expect, but got %A" kv.Key
