[<AutoOpen>]
module Hopac.Tests.Util

open System
open System.Collections.Generic
open System.Reflection
open FSharp.Reflection
open Expecto
open Expecto.ExpectoFsCheck


/// Defines cleanup logic that should be run after testing
type CleanupAttribute() = inherit Attribute()

/// Runs all module-scope functions (without any parameters)
/// that are marked with the Cleanup attribute
let runCleanupInAssembly () =
  Assembly.GetEntryAssembly().GetTypes()
  |> Seq.filter FSharpType.IsModule
  |> Seq.collect (fun t -> t.GetMethods())
  |> Seq.filter (fun m ->
      m.CustomAttributes |> Seq.exists (fun a ->
        a.AttributeType = typeof<CleanupAttribute>)
      && m.GetParameters().Length = 0)
  |> Seq.iter (fun m -> m.Invoke(null, [||]) |> ignore)


/// Defines an FsCheck property test
let testProp name property =
  testPropertyWithConfig
    { FsCheckConfig.defaultConfig with arbitrary = genTypes }
    name property

/// Defines a focused FsCheck property test
let ftestProp name property =
  ftestPropertyWithConfig
    { FsCheckConfig.defaultConfig with arbitrary = genTypes }
    name property

/// Defines an errored FsCheck property test
let etestProp seed name property =
  etestPropertyWithConfig seed
    { FsCheckConfig.defaultConfig with arbitrary = genTypes }
    name property


/// Defines an FsCheck property test, and how many times it should run
let testPropN numberOfTests name property =
  testPropertyWithConfig
    { FsCheckConfig.defaultConfig with
        arbitrary = genTypes
        maxTest = numberOfTests }
    name property

/// Defines a focused FsCheck property test, and how many times it should run
let ftestPropN numberOfTests name property =
  ftestPropertyWithConfig
    { FsCheckConfig.defaultConfig with
        arbitrary = genTypes
        maxTest = numberOfTests }
    name property

/// Defines an errored FsCheck property test, and how many times it should run
let etestPropN seed numberOfTests name property =
  etestPropertyWithConfig seed
    { FsCheckConfig.defaultConfig with
        arbitrary = genTypes
        maxTest = numberOfTests }
    name property
