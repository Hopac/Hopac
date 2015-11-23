// Copyright (C) by Housemarque, Inc.

module FrequencyServer

// Example inspired by an example in the book Erlang Programming by Cesarini and
// Thompson.

open System.Collections.Generic
open Hopac
open Hopac.Infixes

type Frequency = int
type FrequencyServer =
   {allocCh: Ch<Proc * Ch<Frequency> * Promise<unit>>
    deallocCh: Ch<Proc * Frequency>}

let allocate s =
  s.allocCh *<+->= fun replyCh nack ->
    Proc.map <| fun self -> (self, replyCh, nack)

let deallocate s freq =
  Proc.bind <| fun self ->
  s.deallocCh *<+ (self, freq)

let create (frequencies: seq<Frequency>) = Job.delay <| fun () ->
  let self = {allocCh = Ch (); deallocCh = Ch ()}

  let free = HashSet<_>(frequencies)
  let allocated = HashSet<_>()

  let deallocate proc freq =
    if allocated.Remove (proc, freq) then
      free.Add freq |> ignore
    // We just ignore spurious deallocations.

  let noneFree =
        allocated
        |> Seq.map (fun (proc, freq) ->
           proc ^-> fun () -> deallocate proc freq)
        |> Alt.choose
    <|> self.deallocCh ^-> fun (proc, freq) ->
          deallocate proc freq

  let someFree =
        noneFree
    <|> self.allocCh ^=> fun (proc, replyCh, nack) ->
          let mutable e = free.GetEnumerator ()
          if e.MoveNext () then
            let freq = e.Current
            e.Dispose ()
            replyCh *<- freq ^-> fun () ->
                 free.Remove freq |> ignore
                 allocated.Add (proc, freq) |> ignore
            <|> nack
          else
            e.Dispose ()
            Alt.unit ()

  Job.iterateServer () <| fun () ->
        if 0 < free.Count then someFree else noneFree
  >>-. self
