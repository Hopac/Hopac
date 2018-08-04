// Copyright (C) by Vesa Karvonen

module BuilderTests

open Hopac
open System
open System.Threading.Tasks

// This is a compile only test.
let testSyntax (xT: Task<'disposable>)
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
  do! Job.awaitUnitTask t
  do! uJ
  if true then
    return ()
  for _ in xs do
    ()
  try return! uJ
  with _ -> ()
  try return! Job.awaitUnitTask t
  finally ()
  let n = ref 1
  while !n > 0 do
    n := !n-1
    return! Job.awaitUnitTask t
#if !CORECLR
  use xT' = xT
  use! x = xT'
  return x
#else
  return ()
#endif
}
