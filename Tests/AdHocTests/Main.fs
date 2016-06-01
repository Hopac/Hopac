// Copyright (C) by Vesa Karvonen

module Main

[<EntryPoint>]
let main _ =
  JobTests.run ()
  StreamTests.run ()
  exitCode
