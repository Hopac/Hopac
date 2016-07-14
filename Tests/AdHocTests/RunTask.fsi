// Copyright (C) by Vesa Karvonen

module RunTask

open System
open System.Runtime.CompilerServices
open System.Threading.Tasks

type RunTaskBuilder =
  new: unit -> RunTaskBuilder
  member Bind:                    Task     * (unit -> Task<'x>) -> Task<'x>
  member Bind:                    Task<'w> * ('w   -> Task<'x>) -> Task<'x>
  member Bind: ConfiguredTaskAwaitable     * (unit -> Task<'x>) -> Task<'x>
  member Bind: ConfiguredTaskAwaitable<'w> * ('w   -> Task<'x>) -> Task<'x>
  member Combine: Task * (unit -> Task<'x>) -> Task<'x>
  member Delay: (unit -> Task<'x>) -> (unit -> Task<'x>)
  member For: seq<'x> * ('x -> #Task) -> Task<unit>
  member Return: 'x -> Task<'x>
  member ReturnFrom:                    Task     -> Task<unit>
  member ReturnFrom:                    Task<'x> -> Task<'x>
  member ReturnFrom: ConfiguredTaskAwaitable     -> Task<unit>
  member ReturnFrom: ConfiguredTaskAwaitable<'x> -> Task<'x>
  member Run: (unit -> Task<'x>) -> Task<'x>
  member TryFinally: (unit -> Task<'x>) * (unit -> unit) -> Task<'x>
  member TryWith: (unit -> Task<'x>) * (exn -> Task<'x>) -> Task<'x>
  member Using: 'w * ('w -> Task<'x>) -> Task<'x> when 'w :> IDisposable
  member While: (unit -> bool) * (unit -> #Task) -> Task<unit>
  member Zero: unit -> Task<unit>

val runTask: RunTaskBuilder
