// Copyright (C) by Vesa Karvonen

module RunTask

open System
open System.Threading.Tasks

type RunTaskBuilder =
  new: unit -> RunTaskBuilder
  member Bind: Task * (unit -> Task<'x>) -> Task<'x>
  member Bind: Task<'x> * ('x -> Task<'y>) -> Task<'y>
  member Combine: Task * (unit -> Task<'x>) -> Task<'x>
  member Delay: (unit -> Task<'x>) -> (unit -> Task<'x>)
  member For: seq<'x> * ('x -> #Task) -> Task<unit>
  member Return: 'x -> Task<'x>
  member ReturnFrom: Task<'x> -> Task<'x>
  member ReturnFrom: Task -> Task<unit>
  member Run: (unit -> Task<'x>) -> Task<'x>
  member TryFinally: (unit -> Task<'x>) * (unit -> unit) -> Task<'x>
  member TryWith: (unit -> Task<'x>) * (exn -> Task<'x>) -> Task<'x>
  member Using: 'x * ('x -> Task<'y>) -> Task<'y> when 'x :> IDisposable
  member While: (unit -> bool) * (unit -> #Task) -> Task<unit>
  member Zero: unit -> Task<unit>

val runTask: RunTaskBuilder
