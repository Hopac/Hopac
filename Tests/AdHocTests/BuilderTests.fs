// Copyright (C) by Vesa Karvonen

module BuilderTests

open Hopac
open System
open System.Threading.Tasks

// This is a compile only test.
let testSyntax (xT: Task<IDisposable>)
               (xE: IEvent<int>)
               (xO: IObservable<float>)
               (t: Task)
               (xA: Async<int>)
               (xs: seq<float>)
               (uJ: Job<unit>) = job {
  let! _ = xE
  let! _ = xO
  let! _ = xT
  let! _ = xA
  do! t
  do! uJ
  if true then
    return ()
  use! x = xT
  for _ in xs do
    ()
  try return! uJ
  with _ -> ()
  try return! t
  finally ()
  let n = ref 1
  while !n > 0 do
    n := !n-1
    return! t
  return x
}
