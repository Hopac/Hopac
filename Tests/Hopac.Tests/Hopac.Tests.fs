module Hopac.Tests.Main

open Expecto

[<EntryPoint>]
let main argv =
    let exitCode = runTestsInAssemblyWithCLIArgs [] argv
    runCleanupInAssembly ()
    exitCode