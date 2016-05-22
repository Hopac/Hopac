// Copyright (C) by Vesa Karvonen

module TaskTests

open Hopac
open Hopac.Infixes
open Hopac.Extensions
open System
open System.Threading
open System.Threading.Tasks

exception Ex

let verify n c = printfn "%s %s" (if c then "Ok" else "FAILURE") n

let delayAndThen (ms: int) fn =
  Task.toAlt <| fun ct ->
  Task.Delay(ms, ct).ContinueWith(Func<_,_>(fn), ct)

let delayAndSet r (ms: int) =
  delayAndThen ms <| fun t ->
  match Interlocked.CompareExchange(r, ms, 0) with
   | 0 -> ms
   | ms ->
     printfn "Unexpected %d %A" ms t.IsCanceled
     failwithf "Unexpected %d" ms

let run () =
  do let r = ref 0
     (delayAndSet r 100 <|> delayAndSet r 50 |> run = 50 && !r = 50)
     |> verify "Delays"

  do let r = ref 0
     (delayAndSet r 50 <|> delayAndSet r 100 |> run = 50 && !r = 50)
     |> verify "Delays"

  do Job.tryIn (delayAndSet (ref 1) 50
                <|> Task.toAlt (fun _ -> raise Ex ; Task.FromResult 1))
         <| fun x -> printfn "Unexpected %A" x ; Job.unit ()
         <| fun e -> printfn "OK %A" e ; Job.unit ()
     |> run

  do Job.tryIn (delayAndThen 50 (fun _ -> raise Ex)
                <|> delayAndSet (ref 1) 100)
         <| fun x -> printfn "Unexpected %A" x ; Job.unit ()
         <| fun e -> printfn "OK %A" <| e.GetBaseException () ; Job.unit ()
     |> run
