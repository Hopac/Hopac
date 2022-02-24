module Hopac.Tests.Async

open Expecto
open Expecto.Flip
open Hopac
open Hopac.Infixes
open Hopac.Extensions


[<Tests>]
let __ = testList "Async" [

  testList "basics" [

    testCase "Job.fromAsync gets result from async" <| fun _ ->
      async {
        do! Async.Sleep 100
        return true }
      |> Job.fromAsync
      |> run |> Expect.equal "Async completed as job" true

    testCase "Job.fromAsync completes with side effect" <| fun _ ->
      let mutable ok = false
      async {
        do! Async.Sleep 100
        ok <- true }
      |> Job.fromAsync |> run
      Expect.equal "Side effect occurred" true ok

    testCase "Job.toAsync gets result from job" <| fun _ ->
      timeOutMillis 100 >>-. true
      |> Job.toAsync |> Async.RunSynchronously
      |> Expect.equal "Job completed as async" true

    testCase "Job.toAsync completes with side effect" <| fun _ ->
      let mutable ok = false
      timeOutMillis 100 >>- fun () -> ok <- true
      |> Job.toAsync |> Async.RunSynchronously
      Expect.equal "Side effect occurred" true ok

    testPropN 20 "Job.bindAsync gets result from async" <|
      fun (c:AlphaChar) ->
        async {
          do! Async.Sleep 10
          return c.ac }
        |> Job.bindAsync Job.result
        |> run |> Expect.equal "Async completed as job" c.ac

    testPropN 20 "Job.bindAsync completes after side effect" <|
      fun (n1: int) (n2: int) ->
        let mutable r = None
        async {
          do! Async.Sleep 10
          r <- Some n1 }
        |> Job.bindAsync (fun _ ->
            r |> Option.map ((+) n2) |> Job.result)
        |> run |> Option.exists ((=) (n1 + n2))
  ]

  testList "exceptions" [

    testCase "Job.fromAsync can catch raised exception" <| fun _ ->
      async { raise KnownExn }
      |> Job.fromAsync
      |> Job.catch >>- getCaughtExnDiff [ KnownExn ]
      |> run |> Expect.equal "Exception caught" []

    testCase "Job.toAsync can catch raised exception" <| fun _ ->
      async {
        try
          do! Job.raises KnownExn |> Job.toAsync
          return false
        with _ -> return true }
      |> Async.RunSynchronously
      |> Expect.equal "Exception caught" true

    testCase "Job.bindAsync can catch raised exception" <| fun _ ->
      async { raise KnownExn }
      |> Job.bindAsync Job.result
      |> Job.catch >>- getCaughtExnDiff [ KnownExn ]
      |> run |> Expect.equal "Exception caught" []

    testCase "Alt.fromAsync can catch raised exception" <| fun _ ->
      async { raise KnownExn }
      |> Alt.fromAsync
      |> Job.catch >>- getCaughtExnDiff [ KnownExn ]
      |> run |> Expect.equal "Exception caught" []

    testCase "Alt.toAsync can catch raised exception" <| fun _ ->
      async {
        try
          do! Alt.unit () ^=>. Job.raises KnownExn |> Alt.toAsync
          return false
        with _ -> return true }
      |> Async.RunSynchronously
      |> Expect.equal "Exception caught" true
  ]

  testList "alts" [

    testCase "Alt.fromAsync commits when immediately available" <| fun _ ->
      async { return true } |> Alt.fromAsync
      <|> timeOutMillis 1000 ^->. false
      |> run |> Expect.equal "1st branch was available" true

    testCase "Alt.fromAsync commits after a delay" <| fun _ ->
      async {
        do! Async.Sleep 100
        return true } |> Alt.fromAsync
      <|> Alt.never ()
      |> run |> Expect.equal "1st branch was available" true

    testCase "Alt.toAsync inner alt commits (immediately)" <| fun _ ->
      async {
        return!
          Alt.always true <|> timeOutMillis 1000 ^->. false
          |> Alt.toAsync }
      |> Async.RunSynchronously
      |> Expect.equal "1st inner branch was available" true

    testCase "Alt.toAsync inner alt commits (delayed)" <| fun _ ->
      async {
        return!
          timeOutMillis 100 ^->. true <|> Alt.never ()
          |> Alt.toAsync }
      |> Async.RunSynchronously
      |> Expect.equal "1st inner branch was available" true
  ]
]