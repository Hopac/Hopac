module Hopac.Tests.MVar

open System
open Expecto
open Expecto.Flip
open Hopac
open Hopac.Infixes

[<Tests>]
let __ = testList "MVar" [

  testList "basics" [

    testCase "filled value can be read" <| fun _ ->
      let m = MVar()
      MVar.fill m 1 |> run
      MVar.read m |> run |> Expect.equal "Filled value" 1

    testCase "filled value can be taken" <| fun _ ->
      let m = MVar()
      MVar.fill m 1 |> run
      MVar.take m |> run |> Expect.equal "Filled value" 1

    testCase "initial value is treated as filled" <| fun _ ->
      let m = MVar 1
      MVar.read m |> run |> Expect.equal "Initial value" 1

    testCase "casting to Job takes the MVar's value" <| fun _ ->
      let m = MVar 1
      m |> run |> Expect.equal "Initial value" 1

    testCase "*<<= fills the MVar's value" <| fun _ ->
      let m = MVar()
      m *<<= 1 |> run
      m |> run |> Expect.equal "Filled value" 1

    testCase "MVar.mutateFun updates a value" <| fun _ ->
      let m = MVar 1
      m |> MVar.mutateFun ((+) 1) |> run
      m |> run |> Expect.equal "Mutated value" 2

    testCase "MVar.mutateJob updates a value" <| fun _ ->
      let m = MVar 1
      m |> MVar.mutateJob (fun x -> Job.result (x + 1)) |> run
      m |> run |> Expect.equal "Mutated value" 2

    testCase "MVar.modifyFun updates a value" <| fun _ ->
      let m = MVar 1
      m |> MVar.modifyFun (fun x -> x + 1, true) |> run |> ignore
      m |> run |> Expect.equal "Modified value" 2

    testCase "MVar.modifyJob updates a value" <| fun _ ->
      let m = MVar 1
      MVar.modifyJob (fun x ->
        Job.result (x + 1, true)) m |> run |> ignore
      m |> run |> Expect.equal "Modified value" 2

    testCase "MVar.tryMutateFun updates a value" <| fun _ ->
      let m = MVar 1
      m |> MVar.tryMutateFun ((+) 1) |> run
      m |> run |> Expect.equal "Mutated value" 2

    testCase "MVar.tryMutateJob updates a value" <| fun _ ->
      let m = MVar 1
      m |> MVar.tryMutateJob (fun x -> Job.result (x + 1)) |> run
      m |> run |> Expect.equal "Mutated value" 2

    testCase "MVar.tryModifyFun updates a value" <| fun _ ->
      let m = MVar 1
      m |> MVar.tryModifyFun (fun x -> x + 1, true) |> run |> ignore
      m |> run |> Expect.equal "Modified value" 2

    testCase "MVar.tryModifyJob updates a value" <| fun _ ->
      let m = MVar 1
      MVar.tryModifyJob (fun x ->
        Job.result (x + 1, true)) m |> run |> ignore
      m |> run |> Expect.equal "Modified value" 2

    testCase "MVar.modifyFun returns a state variable" <| fun _ ->
      let m = MVar 1
      m |> MVar.modifyFun (fun x -> x, true)
      |> run |> Expect.equal "State variable" true

    testCase "MVar.modifyJob returns a state variable" <| fun _ ->
      let m = MVar 1
      m |> MVar.modifyJob (fun x -> Job.result (x, true))
      |> run |> Expect.equal "State variable" true

    testCase "MVar.tryModifyFun returns a state variable" <| fun _ ->
      let m = MVar 1
      m |> MVar.tryModifyFun (fun x -> x, true)
      |> run |> Expect.equal "State variable" true

    testCase "MVar.tryModifyJob returns a state variable" <| fun _ ->
      let m = MVar 1
      m |> MVar.tryModifyJob (fun x -> Job.result (x, true))
      |> run |> Expect.equal "State variable" true

    testProp "can mutate then read n MVars concurrently" <|
      fun (n: SmallInt) ->
        let n = n.si
        seq { for i in 0 .. n - 1 -> MVar i }
        |> Seq.map (fun m ->
            m |> MVar.mutateFun ((+) 1) >>=. MVar.read m)
        |> Job.conCollect |> run |> List.ofSeq |> List.sort
        |> Expect.equal (sprintf "All %i values" n) [ 1..n ]
  ]

  testList "exceptions" [

    testCase "MVar.fill on a full MVar raises an exception" <| fun _ ->
      let m = MVar 1
      MVar.fill m 2 |> Job.catch
      >>- getCaughtExnDiff [ Exception "MVar full" ] |> run
      |> Expect.equal "'Already full' exception caught" []

    testCase "*<<= on a full MVar raises an exception" <| fun _ ->
      let m = MVar 1
      m *<<= 2 |> Job.catch
      >>- getCaughtExnDiff [ Exception "MVar full" ] |> run
      |> Expect.equal "'Already full' exception caught" []

    testCase "full MVar can't have its value overwritten" <| fun _ ->
      let m = MVar 1
      MVar.fill m 2 |> Job.catch |> run |> ignore
      MVar.read m |> run |> Expect.equal "Initial value" 1

    testCase "MVar.tryMutateFun leaves the old value if it throws" <| fun _ ->
      let m = MVar 1
      MVar.tryMutateFun (fun _ -> raise KnownExn) m
      |> Job.catch |> run |> ignore
      m |> run |> Expect.equal "Initial value" 1

    testCase "MVar.tryMutateJob leaves the old value if it throws" <| fun _ ->
      let m = MVar 1
      MVar.tryMutateJob (fun _ -> Job.raises KnownExn) m
      |> Job.catch |> run |> ignore
      m |> run |> Expect.equal "Initial value" 1
  ]

  testList "alts" [

    testCase "full MVar is available" <| fun _ ->
      let m = MVar true
      MVar.read m <|> Alt.always false
      |> run |> Expect.equal "Filled value available" true

    testCase "empty MVar is unavailable" <| fun _ ->
      let m = MVar()
      MVar.read m ^->. false <|> timeOutMillis 100 ^->. true
      |> run |> Expect.equal "Unfilled value unavailable" true

    testCase "value taken from MVar is no longer available" <| fun _ ->
      let m = MVar false
      MVar.take m |> run |> ignore
      MVar.read m <|> Alt.always true
      |> run |> Expect.equal "Taken value unavailable" true

    testCase "full MVar is available for mutation" <| fun _ ->
      let m = MVar 1
      MVar.mutateFun id m ^->. true
      <|> Alt.always false
      |> run |> Expect.equal "Mutated full MVar" true

    testCase "empty MVar isn't available for mutation" <| fun _ ->
      let m = MVar()
      MVar.mutateFun id m ^->. false
      <|> Alt.always true
      |> run |> Expect.equal "Didn't mutate empty MVar" true

    testCase "MVar.read doesn't clear the MVar's value" <| fun _ ->
      let m = MVar 1
      MVar.read m |> run |> ignore
      MVar.read m ^->. true <|> Alt.always false
      |> run |> Expect.equal "MVar was full after reading" true

    testCase "MVar.take clears the MVar's value" <| fun _ ->
      let m = MVar 1
      MVar.take m |> run |> ignore
      MVar.read m ^->. false <|> Alt.always true
      |> run |> Expect.equal "MVar was empty after taking" true

    testCase "casting to Job clears the MVar's value" <| fun _ ->
      let m = MVar 1
      m |> run |> ignore
      MVar.read m ^->. false <|> Alt.always true
      |> run |> Expect.equal "MVar was empty after taking" true

    testCase "casting to Alt clears the MVar's value" <| fun _ ->
      let m = MVar 1
      m <|> Alt.always -1 |> run |> ignore
      MVar.read m ^->. false <|> Alt.always true
      |> run |> Expect.equal "MVar was empty after taking" true
  ]
]