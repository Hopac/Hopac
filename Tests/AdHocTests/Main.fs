// Copyright (C) by Vesa Karvonen

module Main

open Hopac
open System

[<EntryPoint>]
let main _ =
  Scheduler.Global.setCreate
    {Scheduler.Create.Def with
      TopLevelHandler = Some <| fun e ->
        Ch.send topLevelExns e}

  printfn "ProcessorCount = %d" Environment.ProcessorCount
  JobTests.run ()
  AltTests.run ()
  IVarTests.run ()
  MVarTests.run ()
  TaskTests.run ()
  StreamTests.run ()
  exitCode
