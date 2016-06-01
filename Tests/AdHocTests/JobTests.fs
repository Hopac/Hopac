// Copyright (C) by Vesa Karvonen

module JobTests

open FsCheck
open System
open System.Numerics
open Hopac
open Hopac.Infixes

type [<Struct>] Dummy (x: ref<int>) =
  interface IDisposable with
    member this.Dispose () = x := !x + 1

let run () =
  let x = ref 0
  let dummy = new Dummy (x)
  Job.using dummy Job.result |> run |> testEq dummy
  testEq 1 !x
  Job.using (null : IDisposable) Job.result |> run |> testEq null

  Job.delay (fun () -> raise ^ Expected 1) <*>
  Job.delay (fun () -> raise ^ Expected 2)
  |> Job.catch |> run
  |> testExpected [Expected 1; Expected 2]
