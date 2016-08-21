// Copyright (C) by Vesa Karvonen

module Main

[<EntryPoint>]
let main _ =
  JobTests.run ()
  MVarTests.run ()
  TaskTests.run ()
  StreamTests.run ()
  exitCode
