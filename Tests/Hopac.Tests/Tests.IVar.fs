module Hopac.Tests.IVar

open System
open Expecto
open Expecto.Flip
open Hopac
open Hopac.Infixes

[<Tests>]
let __ = testList "IVar" [

  testList "values" [

    testCase "filled value can be read" <| fun _ ->
      let v = IVar()
      IVar.fill v 1 |> run
      IVar.read v |> run |> Expect.equal "Filled value" 1

    testCase "initial value is treated as filled" <| fun _ ->
      let v = IVar 1
      IVar.read v |> run |> Expect.equal "Initial value" 1

    testCase "casting to Job is equivalent to IVar.read" <| fun _ ->
      let v = IVar 1
      v |> run |> Expect.equal "Value read by casting" 1

    testCase "IVar.tryFill won't overwrite an initial value" <| fun _ ->
      let v = IVar 1
      IVar.tryFill v 2 |> run
      v |> run |> Expect.equal "Initial value" 1

    testCase "IVar.tryFill won't overwrite a filled value" <| fun _ ->
      let v = IVar()
      IVar.fill v 1 |> run
      IVar.tryFill v 2 |> run
      v |> run |> Expect.equal "Initial value" 1

    testProp "can read n IVars correctly" <|
      fun (n: SmallInt) ->
        let n = n.si
        seq { for i in 1..n -> IVar i }
        |> Seq.map IVar.read
        |> Job.conCollect |> run |> List.ofSeq |> List.sort
        |> Expect.equal (sprintf "All %i values" n) [ 1..n ]
  ]

  testList "exceptions" [

    testCase "IVar.fill on a full IVar raises an exception" <| fun _ ->
      let v = IVar 1
      IVar.fill v 2 |> Job.catch
      >>- getCaughtExnDiff [ Exception "IVar full" ] |> run
      |> Expect.equal "Already full exception caught" []

    testCase "full IVar can't have its value overwritten" <| fun _ ->
      let v = IVar 1
      IVar.fill v 2 |> Job.catch >>- ignore |> run
      IVar.read v |> run |> Expect.equal "IVar's original value" 1

    testCase "initial exception in an IVar is thrown when read" <| fun _ ->
      let v = IVar<int> (ExpectedExn 1)
      v |> Job.catch
      >>- getCaughtExnDiff [ ExpectedExn 1 ] |> run
      |> Expect.equal "Initial exception caught" []

    testCase "IVar.fillFailure exception is thrown when read" <| fun _ ->
      let v = IVar<int>()
      IVar.fillFailure v (ExpectedExn 1) |> run
      v |> Job.catch
      >>- getCaughtExnDiff [ ExpectedExn 1 ] |> run
      |> Expect.equal "Filled exception caught" []

    testCase "exception can't be overwritten with a value" <| fun _ ->
      let v = IVar (ExpectedExn 1)
      IVar.fill v 1 |> Job.catch >>- ignore |> run
      v |> Job.catch
      >>- getCaughtExnDiff [ ExpectedExn 1 ] |> run
      |> Expect.equal "Original exception caught" []

    testCase "value can't be overwritten with an exception" <| fun _ ->
      let v = IVar 1
      IVar.fillFailure v (Exception "won't fill")
      |> Job.catch >>- ignore |> run
      v |> run |> Expect.equal "Original value" 1

    testCase "exception can't be overwritten with an exception" <| fun _ ->
      let v = IVar (ExpectedExn 1)
      IVar.fillFailure v (Exception "won't fill")
      |> Job.catch >>- ignore |> run
      v |> Job.catch >>- getCaughtExnDiff [ ExpectedExn 1 ] |> run
      |> Expect.equal "Original exception caught" []
  ]

  testList "selective" [

    testCase "filled IVar is available" <| fun _ ->
      let v = IVar true
      IVar.read v <|> Alt.always false
      |> run |> Expect.equal "Filled value available" true

    testCase "unfilled IVar is unavailable" <| fun _ ->
      let v = IVar()
      IVar.read v <|> (timeOutMillis 100 ^->. true)
      |> run |> Expect.equal "Unfilled value unavailable" true
  ]

]