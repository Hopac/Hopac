module Tests

open System
open System.Threading.Tasks
open Expecto
open Expecto.Logging
open Expecto.Logging.Message
open Hopac

let logger = Log.create "Hopac.Tests"

[<RequireQualifiedAccess>]
type Monad =
  | Job
  | Async

type Behaviour =
  | RetUnit
  | Raise

type Nesting =
  | Leaf of target:Monad * behave:Behaviour
  | Node of outer:Monad * inner:Nesting

let e = InvalidOperationException "bad bad"

let rec buildJob = function
  | Node (Monad.Async, inner) ->
    async {
      return! buildJob inner |> Job.toAsync
    }
    |> Job.fromAsync

  | Node (Monad.Job, inner) ->
    job {
      return! buildJob inner
    }

  | Leaf (Monad.Job, RetUnit) ->
    job {
      return ()
    }

  | Leaf (Monad.Job, Raise) ->
    job {
      return raise e
    }

  | Leaf (Monad.Async, RetUnit) ->
    async {
      return ()
    }
    |> Job.fromAsync

  | Leaf (Monad.Async, Raise) ->
    async {
      return raise e
    }
    |> Job.fromAsync

let rec buildAsync = function
  | Node (Monad.Async, inner) ->
    async {
      return! buildAsync inner
    }

  | Node (Monad.Job, inner) ->
    job {
      return! buildAsync inner
    }
    |> Job.toAsync

  | Leaf (Monad.Job, RetUnit) ->
    job {
      return ()
    }
    |> Job.toAsync

  | Leaf (Monad.Job, Raise) ->
    job {
      return raise e
    }
    |> Job.toAsync

  | Leaf (Monad.Async, RetUnit) ->
    async {
      return ()
    }

  | Leaf (Monad.Async, Raise) ->
    async {
      return raise e
    }

let rec getBehaviour = function
  | Node (_, i) -> getBehaviour i
  | Leaf (_, b) -> b

[<Tests>]
let tests =
  let erroring: Job<int> =
    job {
      if 1 = 1 then return raise e
      else return 1
    }
  let erroringU: Job<unit> =
    job {
      if 1 = 1 then return raise e
      else return ()
    }

  testList "properties" [
    testList "async and job" [
      testList "manual" [
        testCase "roundtrip exception" <| fun _ ->
          let catcher =
            job {
              try return! erroring
              with e -> return 10
            }
          Expect.equal (run catcher) 10 "Should return 10 from the with e-> handler"

        testCase "roundtrip exception w async" <| fun _ ->
          let catcher =
            job {
              try return! Job.toAsync erroring
              with e -> return 10
            }
          Expect.equal (run catcher) 10 "Should return 10 from the with e-> handler"

        testCase "roundtrip exception w async 2" <| fun _ ->
          let catcher =
            async {
              try return! Job.toAsync erroring
              with e -> return 10
            }
          Expect.equal (Async.RunSynchronously catcher) 10 "Should return 10 from the with e-> handler"

        testCase "roundtrip exception w async 3" <| fun _ ->
          let catcher =
            async {
              try return! Job.toAsync erroring
              with e -> return 10
            }
          Expect.equal (Async.RunSynchronously catcher) 10 "Should return 10 from the with e-> handler"

        testCase "roundtrip exception w async unit" <| fun _ ->
          let catcher =
            job {
              try return! Job.toAsync erroringU
              with e -> return ()
            }
          Expect.equal (run catcher) () "Should return 10 from the with e-> handler"

        testCase "roundtrip exception w async unit 2" <| fun _ ->
          let catcher =
            async {
              try return! Job.toAsync erroringU
              with e -> return ()
            }
          Expect.equal (Async.RunSynchronously catcher) () "Should return 10 from the with e-> handler"

        testCase "roundtrip exception w async unit 3" <| fun _ ->
          let catcher =
            async {
              try return! Job.toAsync erroringU
              with e -> return ()
            }
          Expect.equal (Async.RunSynchronously catcher) () "Should return 10 from the with e-> handler"
      ]

      testSequencedGroup "using run" <| testList "generated" [
        let printArgs _ no name args =
          logger.verboseWithBP (
            eventX "Test {name} {no} got args {args}"
            >> setField "name" name
            >> setField "no" no
            >> setField "args" args)

        let cfg =
          { FsCheckConfig.defaultConfig with
              maxTest = 100
              startSize = 10
              receivedArgs = printArgs }

        yield testPropertyWithConfig cfg "nesting, outer is job" <| fun (sample: Nesting) ->
          let runnable = buildJob sample
          let op () = try run runnable with e -> raise e.InnerException
          if getBehaviour sample = Behaviour.Raise then
            Expect.throwsC op (function
              | :? InvalidOperationException as ioe -> ()
              | ow -> Tests.failtestf "Got %A" ow)
          else
            let res = op ()
            Expect.equal res () "Should return unit"

        yield testPropertyWithConfig cfg "nesting, outer is async" <| fun (sample: Nesting) ->
          let runnable = buildAsync sample
          let op () = Async.RunSynchronously runnable
          if getBehaviour sample = Behaviour.Raise then
            Expect.throwsC op (function
              | :? InvalidOperationException as ioe -> ()
              | ow -> Tests.failtestf "Got %A" ow)
          else
            let res = op ()
            Expect.equal res () "Should return unit"
      ]
    ]
  ]