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
