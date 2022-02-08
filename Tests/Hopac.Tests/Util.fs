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


/// An expected exception with structural equality
exception ExpectedExn of Id: int

/// A known exception with no value
exception KnownExn

/// Traverses inner exceptions to find root cause exceptions and adds them to
/// the dictionary, with a count of how many identical root causes were found
let rec getRootCauses (dict: Dictionary<exn, int>)  (ex: exn) =
  match ex with
  | :? AggregateException as ex ->
      ex.InnerExceptions
      |> Seq.iter (getRootCauses dict)
  | _ ->
    match ex.InnerException with
    | null ->
        match dict.TryGetValue ex with
        | true, n -> dict.[ex] <- n + 1
        | _ -> dict.Add (ex, 1) |> ignore
    | ex ->
        getRootCauses dict ex

/// Checks that the root causes nested inside an actual exception
/// are equivalent to the sequence of expected exceptions.
/// Returns a string list of any differences found
let getExnDiff expected actual =
  let rootCauses = Dictionary()
  getRootCauses rootCauses actual
  let diffs = ResizeArray<string>()
  let wasExnFound want =
    rootCauses.Keys
    |> Seq.exists (fun have ->
        let n = rootCauses.[have]
        if n <= 0 then false
        elif want = have || want.Message = have.Message then
          rootCauses.[have] <- n - 1; true
        else false)
  expected
  |> Seq.iter (fun ex ->
      if not <| wasExnFound ex
      then diffs.Add (sprintf "Didn't find expected %A" ex))
  for kv in rootCauses do
    if kv.Value <> 0 then
      diffs.Add (sprintf "Found unexpected %A" kv.Key)
  List.ofSeq diffs

/// Checks that the root causes nested inside a Job.catch result
/// are equivalent to the sequence of expected exceptions.
/// Returns a string list of any differences found
let getCaughtExnDiff expected actual =
  match actual with
  | Choice1Of2 _ -> [ "No exceptions caught" ]
  | Choice2Of2 ex -> getExnDiff expected ex