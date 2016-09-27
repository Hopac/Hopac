// Copyright (C) by Vesa Karvonen

[<AutoOpen>]
module Util

open System
open System.Collections.Generic

let inline (^) f x = f x

let mutable exitCode = 0

let testEq exp act =
  if exp <> act
  then printfn "Expected %A, but got %A" exp act ; exitCode <- 1
  else printfn "Ok"

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
  | Choice1Of2 _ -> printfn "Unexpected success" ; exitCode <- 1
  | Choice2Of2 e ->
    let thrown = Dictionary ()
    getRootCauses thrown e
    let mutable failures = false
    expected
    |> Seq.iter ^ fun e ->
         if thrown.Keys
            |> Seq.exists ^ fun t ->
                 let n = thrown.[t]
                 if n <= 0 then
                   false
                 elif e = t || e.Message = t.Message then
                   thrown.[t] <- n-1
                   true
                 else
                   false
            |> not
          then
            printfn "Expected %A, but got none" e ; failures <- true
    for kv in thrown do
      if kv.Value <> 0 then
        printfn "Didn't expect, but got %A" kv.Key ; failures <- true
    if not failures then
      printfn "Ok"
    else
      exitCode <- 1
