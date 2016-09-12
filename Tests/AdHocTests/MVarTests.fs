// Copyright (C) by Vesa Karvonen

module MVarTests

open FsCheck
open System
open System.Numerics
open Hopac
open Hopac.Infixes

exception Known

let run () =
  do let m = MVar 1

     MVar.read m |> run |> testEq 1
     MVar.read m <|> Alt.always 2 |> run |> testEq 1
     m <|> Alt.always 2 |> run |> testEq 1

     let n = MVar 0

     MVar.mutateFun ((+) 1) m ^->. 0 <|> MVar.modifyFun (fun n -> (n-1, n-2)) n |> run |> testEq -2

     MVar.tryMutateFun ((+) 1) m ^->. 0 <|> MVar.tryModifyFun (fun n -> (n-1, n-2)) n |> run |> testEq -3

     MVar.tryModifyFun (fun _ -> raise Known) n |> Job.catch <*> MVar.read n
     |> run |> testEq (Choice2Of2 Known, -2)

  do let m = MVar 1
     m *<<= 2 |> Job.catch |> run |> testExpected [Exception "MVar full"]
     m |> MVar.read |> run |> testEq 1
