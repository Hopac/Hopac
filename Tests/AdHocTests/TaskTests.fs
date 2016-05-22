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

let delayAndSet (ms: int) r = Task.toAlt <| fun ct -> runTask {
  do! Task.Delay (ms, ct)
  return match Interlocked.CompareExchange (r, ms, 0) with
          | 0 -> ms
          | ms ->
            printfn "Unexpected %d" ms
            failwithf "Unexpected %d" ms
}

let delayAndRaise (ms: int) ex = Task.toAlt <| fun ct -> runTask {
  do! Task.Delay (ms, ct)
  return raise ex
}

let run () =
  do let r = ref 0
     (delayAndSet 100 r <|> delayAndSet 50 r |> run = 50 && !r = 50)
     |> verify "Delays"

  do let r = ref 0
     (delayAndSet 50 r <|> delayAndSet 100 r |> run = 50 && !r = 50)
     |> verify "Delays"

  do Job.tryIn (delayAndSet 50 ^ ref 1
            <|> Task.toAlt ^ fun _ -> raise Ex ; Task.FromResult 1)
         <| fun x -> printfn "Unexpected %A" x ; Job.unit ()
         <| fun e -> printfn "OK %A" e ; Job.unit ()
     |> run

  do Job.tryIn (delayAndRaise 50 Ex
            <|> delayAndSet 100 ^ ref 1)
         <| fun x -> printfn "Unexpected %A" x ; Job.unit ()
         <| fun e -> printfn "OK %A" <| e.GetBaseException () ; Job.unit ()
     |> run
