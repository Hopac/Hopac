// Copyright (C) by Vesa Karvonen

module IVarTests

open FsCheck
open System
open System.Numerics
open Hopac
open Hopac.Infixes

let run () =
  do let v = IVar 1
     IVar.fill v 2 |> Job.catch |> run |> testExpected [Exception "IVar full"]
     v |> run |> testEq 1

  do let v = IVar<int> (Exception "a")
     IVar.fillFailure v (Exception "b") |> Job.catch |> run |> testExpected [Exception "IVar full"]
     v |> Job.catch |> run |> testExpected [Exception "a"]
