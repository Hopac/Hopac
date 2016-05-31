// Copyright (C) by Vesa Karvonen

module JobTests

open FsCheck
open System
open System.Numerics
open Hopac
open Hopac.Infixes

type [<Struct>] Dummy (x: int) =
  member this.X = x
  interface IDisposable with
    member this.Dispose () = ()

let run () =
  let dummy = new Dummy (1)
  Job.using dummy Job.result |> run |> testEq dummy
  Job.using (null : IDisposable) Job.result |> run |> testEq null
