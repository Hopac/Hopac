// Copyright (C) by Housemarque, Inc.

module FrequencyServer

// Example inspired by an example in the book Erlang Programming by Cesarini and Thompson.

open System.Collections.Generic
open Hopac
open Hopac.Infixes
open Hopac.Job.Infixes
open Hopac.Alt.Infixes

exception NoFrequency

type Frequency = int
type Request =
  | Allocate of Proc * IVar<Frequency>
  | Deallocate of Proc * Frequency
type FrequencyServer = { reqCh: Ch<Request> }

let allocate s =
  Proc.self () >>= fun self ->
  let replyI = ivar ()
  s.reqCh <-+ Allocate (self, replyI) >>.
  replyI

let deallocate s freq =
  Proc.self () >>= fun self ->
  s.reqCh <-- Deallocate (self, freq)

let create () = Job.delay <| fun () ->
  let s = {reqCh = ch ()}
  let free = HashSet<_>([10;11;12;13;14;15])
  let allocated = HashSet<_>()
  let allocate proc replyI =
    let mutable e = free.GetEnumerator ()
    if e.MoveNext () then
      let freq = e.Current
      free.Remove freq |> ignore
      allocated.Add (proc, freq) |> ignore
      replyI <-= freq
    else
      replyI <-=! NoFrequency
  let deallocate proc freq =
    if allocated.Remove (proc, freq) then
      free.Add freq |> ignore
    // We just ignore spurious deallocations.
  let reqAlt =
    s.reqCh >>=? function
     | Allocate (proc, replyI) -> allocate proc replyI
     | Deallocate (proc, freq) -> deallocate proc freq ; Job.unit ()
  let joinAlt =
    allocated
    |> Seq.map (fun (proc, freq) ->
       proc |>>? fun () -> deallocate proc freq)
    |> Alt.choose
  Job.foreverServer (reqAlt <|> joinAlt) >>% s
