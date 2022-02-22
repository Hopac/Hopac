module Hopac.Tests.Task

open System.Threading.Tasks
open Expecto
open Expecto.Flip
open Hopac
open Hopac.Infixes
open Hopac.Extensions


/// A default TaskCanceledException for comparison
let cancelledExn = TaskCanceledException()


[<Tests>]
let __ = testList "Task" [

  testList "basics" [

    testProp "Job.fromTask has the expected result" <|
      fun (n: int) ->
        Job.fromTask <| fun () -> Task.FromResult n
        |> run |> Expect.equal "Correct result returned" n

    testProp "Job.fromUnitTask has the expected side effect" <|
      fun (n: int) ->
        let mutable r = None
        Job.fromUnitTask (fun () -> Task.Factory.StartNew(fun () ->
          r <- Some n)) |> run
        Expect.equal "Correct side effect occurred" (Some n) r

    testProp "Job.liftTask passes values correctly" <|
      fun (n1: int) (n2: int) ->
        Job.result n1
        >>= Job.liftTask (fun x -> Task.FromResult (x + n2))
        |> run |> Expect.equal "Correct result returned" (n1 + n2)

    testProp "Job.liftUnitTask has the expected side effect" <|
      fun (n1: int) (n2: int) ->
        let mutable r = None
        Job.result n1
        >>= Job.liftUnitTask (fun x -> Task.Factory.StartNew(fun () ->
          r <- Some (x + n2))) |> run
        Expect.equal "Correct side effect occurred" (Some (n1 + n2)) r

    testProp "Alt.fromTask has the expected result" <|
      fun (n: int) ->
        Alt.fromTask <| fun _ -> Task.FromResult n
        |> run |> Expect.equal "Correct result returned" n

    testProp "Alt.fromUnitTask has the expected side effect" <|
      fun (n: int) ->
        let mutable r = None
        Alt.fromUnitTask (fun _ -> Task.Factory.StartNew(fun () ->
          r <- Some n)) |> run
        Expect.equal "Correct side effect occurred" (Some n) r
  ]

  testList "exceptions" [

    testCase "Job.fromTask raises a set exception" <| fun _ ->
      Job.fromTask <| fun () -> Task.FromException<int> KnownExn
      |> Job.catch >>- getCaughtExnDiff [ KnownExn ]
      |> run |> Expect.equal "Set exception returned" []

    testCase "Job.fromUnitTask raises a set exception" <| fun _ ->
      Job.fromUnitTask <| fun () -> Task.FromException KnownExn
      |> Job.catch >>- getCaughtExnDiff [ KnownExn ]
      |> run |> Expect.equal "Set exception returned" []

    testCase "Job.liftTask raises a set exception" <| fun _ ->
      Job.result 0 >>= Job.liftTask (fun _ -> Task.FromException<int> KnownExn)
      |> Job.catch >>- getCaughtExnDiff [ KnownExn ]
      |> run |> Expect.equal "Set exception returned" []

    testCase "Job.liftUnitTask raises a set exception" <| fun _ ->
      Job.result 0 >>= Job.liftUnitTask (fun _ -> Task.FromException KnownExn)
      |> Job.catch >>- getCaughtExnDiff [ KnownExn ]
      |> run |> Expect.equal "Set exception returned" []

    testCase "Alt.fromTask raises a set exception" <| fun _ ->
      Alt.fromTask <| fun _ -> Task.FromException<int> KnownExn
      |> Job.catch >>- getCaughtExnDiff [ KnownExn ]
      |> run |> Expect.equal "Set exception returned" []

    testCase "Alt.fromUnitTask raises a set exception" <| fun _ ->
      Alt.fromUnitTask <| fun _ -> Task.FromException KnownExn
      |> Job.catch >>- getCaughtExnDiff [ KnownExn ]
      |> run |> Expect.equal "Set exception returned" []


    testCase "Job.fromTask raises TaskCanceled on cancellation" <| fun _ ->
      let tcs = TaskCompletionSource<int>()
      Job.fromTask <| fun () -> tcs.SetCanceled(); tcs.Task
      |> Job.catch >>- getCaughtExnDiff [ cancelledExn ]
      |> run |> Expect.equal "TaskCanceledException" []

    testCase "Job.fromUnitTask raises TaskCanceled on cancellation" <| fun _ ->
      let tcs = TaskCompletionSource()
      Job.fromUnitTask <| fun () -> tcs.SetCanceled(); tcs.Task :> Task
      |> Job.catch >>- getCaughtExnDiff [ cancelledExn ]
      |> run |> Expect.equal "TaskCanceledException" []

    testCase "Job.liftTask raises TaskCanceled on cancellation" <| fun _ ->
      let tcs = TaskCompletionSource<int>()
      Job.result 0 >>= Job.liftTask (fun _ -> tcs.SetCanceled(); tcs.Task)
      |> Job.catch >>- getCaughtExnDiff [ cancelledExn ]
      |> run |> Expect.equal "TaskCanceledException" []

    testCase "Job.liftUnitTask raises TaskCanceled on cancellation" <| fun _ ->
      let tcs = TaskCompletionSource()
      Job.result 0 >>= Job.liftUnitTask (fun _ ->
        tcs.SetCanceled(); tcs.Task :> Task)
      |> Job.catch >>- getCaughtExnDiff [ cancelledExn ]
      |> run |> Expect.equal "TaskCanceledException" []

    testCase "Alt.fromTask raises TaskCanceled on cancellation" <| fun _ ->
      let tcs = TaskCompletionSource()
      Alt.fromTask <| fun _ -> tcs.SetCanceled(); tcs.Task
      |> Job.catch >>- getCaughtExnDiff [ cancelledExn ]
      |> run |> Expect.equal "TaskCanceledException" []

    testCase "Alt.fromUnitTask raises TaskCanceled on cancellation" <| fun _ ->
      let tcs = TaskCompletionSource()
      Alt.fromUnitTask <| fun _ -> tcs.SetCanceled(); tcs.Task :> Task
      |> Job.catch >>- getCaughtExnDiff [ cancelledExn ]
      |> run |> Expect.equal "TaskCanceledException" []


    testCase "job chain halts if Job.fromTask raises" <| fun _ ->
      Job.fromTask (fun () -> Task.FromException<bool> KnownExn)
      >>=. Job.result false
      |> Job.catch >>- getCaughtExnDiff [ KnownExn ]
      |> run |> Expect.equal "Job chain halted by exception" []

    testCase "job chain halts if Job.fromUnitTask raises" <| fun _ ->
      Job.fromUnitTask (fun () -> Task.FromException KnownExn)
      >>=. Job.result false
      |> Job.catch >>- getCaughtExnDiff [ KnownExn ]
      |> run |> Expect.equal "Job chain halted by exception" []

    testCase "job chain halts if Job.liftTask raises" <| fun _ ->
      Job.result 0 >>= Job.liftTask (fun _ -> Task.FromException<int> KnownExn)
      >>=. Job.result false
      |> Job.catch >>- getCaughtExnDiff [ KnownExn ]
      |> run |> Expect.equal "Job chain halted by exception" []

    testCase "job chain halts if Job.liftUnitTask raises" <| fun _ ->
      Job.result 0 >>= Job.liftUnitTask (fun _ -> Task.FromException KnownExn)
      >>=. Job.result false
      |> Job.catch >>- getCaughtExnDiff [ KnownExn ]
      |> run |> Expect.equal "Job chain halted by exception" []

    testCase "job chain halts if Alt.fromTask raises" <| fun _ ->
      Alt.fromTask (fun _ -> Task.FromException<int> KnownExn)
      >>=. Job.result false
      |> Job.catch >>- getCaughtExnDiff [ KnownExn ]
      |> run |> Expect.equal "Job chain halted by exception" []

    testCase "job chain halts if Alt.fromUnitTask raises" <| fun _ ->
      Alt.fromUnitTask (fun _ -> Task.FromException KnownExn)
      >>=. Job.result false
      |> Job.catch >>- getCaughtExnDiff [ KnownExn ]
      |> run |> Expect.equal "Job chain halted by exception" []


    testCase "Job.fromTask won't raise after job chain halts" <| fun _ ->
      Job.raises <| ExpectedExn 1
      >>=. Job.fromTask (fun () -> Task.FromException<int> <| ExpectedExn 2)
      |> Job.catch >>- getCaughtExnDiff [ ExpectedExn 1 ]
      |> run |> Expect.equal "1st exception caught" []

    testCase "Job.fromUnitTask won't raise after job chain halts" <| fun _ ->
      Job.raises <| ExpectedExn 1
      >>=. Job.fromUnitTask (fun () -> Task.FromException <| ExpectedExn 2)
      |> Job.catch >>- getCaughtExnDiff [ ExpectedExn 1 ]
      |> run |> Expect.equal "1st exception caught" []

    testCase "Job.liftTask won't raise after job chain halts" <| fun _ ->
      Job.raises <| ExpectedExn 1
      >>= Job.liftTask (fun () -> Task.FromException<int> <| ExpectedExn 2)
      |> Job.catch >>- getCaughtExnDiff [ ExpectedExn 1 ]
      |> run |> Expect.equal "1st exception caught" []

    testCase "Job.liftUnitTask won't raise after job chain halts" <| fun _ ->
      Job.raises <| ExpectedExn 1
      >>= Job.liftUnitTask (fun () -> Task.FromException <| ExpectedExn 2)
      |> Job.catch >>- getCaughtExnDiff [ ExpectedExn 1 ]
      |> run |> Expect.equal "1st exception caught" []

    testCase "Alt.fromTask won't raise after job chain halts" <| fun _ ->
      Job.raises <| ExpectedExn 1
      >>=. Alt.fromTask (fun _ -> Task.FromException<int> <| ExpectedExn 2)
      |> Job.catch >>- getCaughtExnDiff [ ExpectedExn 1 ]
      |> run |> Expect.equal "1st exception caught" []

    testCase "Alt.fromUnitTask won't raise after job chain halts" <| fun _ ->
      Job.raises <| ExpectedExn 1
      >>=. Alt.fromUnitTask (fun _ -> Task.FromException <| ExpectedExn 2)
      |> Job.catch >>- getCaughtExnDiff [ ExpectedExn 1 ]
      |> run |> Expect.equal "1st exception caught" []
  ]

  testList "alts" [

    testCase "Alt.fromTask token starts not cancelled" <| fun _ ->
      Alt.fromTask (fun ct ->
        Task.FromResult (not ct.IsCancellationRequested))
      |> run |> Expect.equal "Token not cancelled" true

    testCase "Alt.fromUnitTask token starts not cancelled" <| fun _ ->
      let mutable ctOpt = None
      Alt.fromUnitTask (fun ct -> Task.Factory.StartNew(fun () ->
        ctOpt <- Some ct)) |> run
      ctOpt |> Option.exists (fun ct ->
        not ct.IsCancellationRequested)
      |> Expect.equal "Token not cancelled" true

    testCase "Alt.fromTask completes when not cancelled" <| fun _ ->
      Alt.fromTask (fun ct ->
        Task.Delay(100).ContinueWith((fun _ -> true), ct))
      <|> Alt.never () ^->. false
      |> run |> Expect.equal "Task completed" true

    testCase "Alt.fromUnitTask completes when not cancelled" <| fun _ ->
      let mutable ok = false
      (Alt.fromUnitTask (fun ct ->
        Task.Delay(100).ContinueWith((fun _ ->
          ok <- true), ct))
      <|> Alt.never ()) |> run
      Expect.equal "Task completed" true ok

    testPropN 10 "Alt.fromTask cancels when another branch commits" <| fun _ ->
      let mutable ok = true
      (Alt.fromTask (fun ct ->
        Task.Delay(200).ContinueWith((fun _ ->
          ok <- false; false), ct))
      <|> timeOutMillis 10 ^->. true) |> run |> ignore
      Expect.equal "Task cancelled" true ok

    testPropN 10 "Alt.fromUnitTask cancels when another branch commits" <| fun _ ->
      let mutable ok = true
      (Alt.fromUnitTask (fun ct ->
        Task.Delay(200).ContinueWith((fun _ ->
          ok <- false), ct))
      <|> timeOutMillis 10) |> run
      Expect.equal "Task cancelled" true ok
  ]

  testList "additional interop" [

    testCase "Job.awaitTask awaits its task to completion" <| fun _ ->
      Task.Delay(100).ContinueWith(fun _ -> true)
      |> Job.awaitTask
      |> run |> Expect.equal "Awaited task completion" true

    testCase "Job.awaitUnitTask awaits its task to completion" <| fun _ ->
      let mutable ok = false
      Task.Delay(100).ContinueWith(fun _ -> ok <- true)
      |> Job.awaitUnitTask |> run
      Expect.equal "Awaited task completion" true ok

    testCase "Job.bindTask awaits task then does bound job" <| fun _ ->
      Task.Delay(100).ContinueWith(fun _ -> true)
      |> Job.bindTask Job.result
      |> run |> Expect.equal "Awaited task completion into bind" true

    testCase "Job.bindUnitTask awaits task then does bound job" <| fun _ ->
      let mutable ok = false
      Task.Delay(100).ContinueWith(fun _ -> ok <- true)
      |> Job.bindUnitTask (fun _ ->
        ok <- ok && true; Job.unit ()) |> run
      Expect.equal "Awaited task completion into bind" true ok

    testCase "Task.startJob returns its result in a task" <| fun _ ->
      timeOutMillis 100 ^->. true
      |> Task.startJob
      |> run |> fun t -> t.Result
      |> Expect.equal "Job result obtained from task" true

    testCase "Hopac.queueAsTask queues and completes job as task" <| fun _ ->
      let mutable ok = false
      timeOutMillis 100 ^=>. Job.thunk (fun _ -> ok <- true)
      |> queueAsTask |> fun t -> t.Wait()
      Expect.equal "Task completed successfully" true ok

    testCase "Hopac.startAsTask starts and completes job as task" <| fun _ ->
      let mutable ok = false
      timeOutMillis 100 ^=>. Job.thunk (fun _ -> ok <- true)
      |> startAsTask |> fun t -> t.Wait()
      Expect.equal "Task completed successfully" true ok
  ]
]