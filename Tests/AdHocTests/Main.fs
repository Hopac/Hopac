// Copyright (C) by Vesa Karvonen

module Main

open System

[<EntryPoint>]
let main _ =
  printfn "ProcessorCount = %d" Environment.ProcessorCount
  JobTests.run ()
  MVarTests.run ()
  TaskTests.run ()
  StreamTests.run ()
  exitCode
