[<AutoOpen>]
module Hopac.Tests.Util

open System.Reflection
open FSharp.Reflection


/// Defines cleanup logic that should be run after testing
type CleanupAttribute() = inherit System.Attribute()

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

