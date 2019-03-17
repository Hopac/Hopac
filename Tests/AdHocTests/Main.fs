// Copyright (C) by Vesa Karvonen

module Main

open System

[<EntryPoint>]
let main _ =
  printfn "ProcessorCount = %d" Environment.ProcessorCount
  printfn "Run Job tests"
  JobTests.run ()
  printfn "Run Alt tests"
  AltTests.run ()
  printfn "Run IVar tests"
  IVarTests.run ()
  printfn "Run MVar tests"
  MVarTests.run ()
  printfn "Run Task tests"
  TaskTests.run ()
  printfn "Run Stream tests"
  StreamTests.run ()
  exitCode
