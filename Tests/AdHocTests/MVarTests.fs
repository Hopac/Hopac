// Copyright (C) by Vesa Karvonen

module MVarTests

open FsCheck
open System
open System.Numerics
open Hopac
open Hopac.Infixes

let run () =
  let m = MVar 1

  MVar.read m |> run |> testEq 1
  MVar.read m <|> Alt.always 2 |> run |> testEq 1
  m <|> Alt.always 2 |> run |> testEq 1

  let n = MVar 0

  MVar.mutateFun ((+) 1) m ^->. 0 <|> MVar.modifyFun (fun n -> (n-1, n-2)) n |> run |> testEq -2
