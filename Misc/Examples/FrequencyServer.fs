// Copyright (C) by Housemarque, Inc.

module FrequencyServer

// Example inspired by an example in the book Erlang Programming by Cesarini and Thompson.

open System.Collections.Generic
open Hopac
open Hopac.Infixes
open Hopac.Job.Infixes
open Hopac.Alt.Infixes

type Frequency = int
type FrequencyServer = {
    allocCh: Ch<Proc * Promise<unit> * Ch<Frequency>>
    deallocCh: Ch<Proc * Frequency>
  }

let allocate s = Alt.withNack <| fun nack ->
  Proc.self () >>= fun self ->
  let replyCh = ch ()
  s.allocCh <-+ (self, nack, replyCh) >>%
  replyCh

let deallocate s freq =
  Proc.self () >>= fun self ->
  s.deallocCh <-- (self, freq)

let create (frequencies: seq<Frequency>) = Job.delay <| fun () ->
  let self = {allocCh = ch (); deallocCh = ch ()}

  let free = HashSet<_>(frequencies)
  let allocated = HashSet<_>()

  let alloc =
    self.allocCh >>=? fun (proc, nack, replyCh) ->
    let mutable e = free.GetEnumerator ()
    if e.MoveNext () then
      let freq = e.Current
      e.Dispose ()
      (replyCh <-- freq |>>? fun () ->
       free.Remove freq |> ignore
       allocated.Add (proc, freq) |> ignore) <|>?
      nack :> Job<_>
    else
      e.Dispose ()
      Job.unit ()

  let deallocate proc freq =
    if allocated.Remove (proc, freq) then
      free.Add freq |> ignore
    // We just ignore spurious deallocations.

  let dealloc =
    self.deallocCh |>>? fun (proc, freq) ->
    deallocate proc freq

  let join =
    allocated
    |> Seq.map (fun (proc, freq) ->
       proc |>>? fun () -> deallocate proc freq)
    |> Alt.choose

  let noneFree = dealloc <|>? join
  let someFree = alloc <|>? noneFree

  Job.iterateServer () (fun () ->
    if 0 < free.Count then someFree else noneFree) >>%
  self
