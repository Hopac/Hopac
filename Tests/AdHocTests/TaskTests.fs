// Copyright (C) by Vesa Karvonen

module TaskTests

open Hopac
open Hopac.Infixes
open Hopac.Extensions
open System
open System.Threading
open System.Threading.Tasks
open RunTask

exception Ex

let inline (^) x = x

let verify n c = printfn "%s %s" (if c then "Ok" else "FAILURE") n

let delayAndSet (ms: int) r = Alt.fromTask <| fun ct -> runTask {
  do! Task.Delay (ms, ct)
  return match Interlocked.CompareExchange (r, ms, 0) with
          | 0 -> ms
          | ms' ->
            // This is an unavoidable race condition as cancellation is not
            // transactional.  Increase timeout if you get this.
            printfn "Unexpected %d (in delayAndSet %d)" ms' ms
            exitCode <- 1
            ms
}

let delayAndRaise (ms: int) ex = Alt.fromTask <| fun ct -> runTask {
  do! Task.Delay (ms, ct)
  return raise ex
}

let run () =
  do let r = ref 0
     (delayAndSet 401 r <|> delayAndSet 50 r |> run, !r)
     |> testEq (50, 50)

  do let r = ref 0
     (delayAndSet 50 r <|> delayAndSet 202 r |> run, !r)
     |> testEq (50, 50)

  do delayAndSet 150 ^ ref 1
     <|> Alt.fromTask ^ fun _ -> raise Ex ; Task.FromResult 1
     |> Job.catch
     |> run
     |> testExpected [Ex]

  do delayAndRaise 50 Ex
     <|> delayAndSet 203 ^ ref 1
     |> Job.catch
     |> run
     |> testExpected [Ex]
