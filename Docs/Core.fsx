// ## Core (minimal subset) of Hopac

// For practical reasons (performance and convenience), Hopac has a relatively
// large [API](http://hopac.github.io/Hopac/Hopac.html).  This document tries to
// capture and describe a *minimal subset* of Hopac that could be used to implement
// *everything* else.  Note that, for just understanding the main ideas, an even
// smaller and slightly different subset should suffice, but then there would be
// some important semantics that could not be implemented precisely.

#I "../Libs/Hopac.Core/bin/Release"
#I "../Libs/Hopac/bin/Release"
#I "../Libs/Hopac.Platform/bin/Release"
#I "../Libs/Hopac.Bench/bin/Release"
#I "../Libs/Hopac.Experimental/bin/Release"

#r "Hopac.Core.dll"
#r "Hopac.dll"
#r "Hopac.Platform.dll"
#r "Hopac.Bench.dll"
#r "Hopac.Experimental.dll"

// ### Core of Hopac

type Job<'x>                                                                = Hopac.Job<'x>
type Alt<'x>                                                                = Hopac.Alt<'x>
type Ch<'x>                                                                 = Hopac.Ch<'x>
type Promise<'x>                                                            = Hopac.Promise<'x>
type Proc                                                                   = Hopac.Proc

module Job =
  let tryIn: Job<'x> -> ('x -> #Job<'y>) -> (exn -> #Job<'y>) -> Job<'y>    = Hopac.Job.tryIn

module Alt =
  let withNackJob: (Promise<unit> -> #Job<#Alt<'x>>) -> Alt<'x>             = Hopac.Alt.withNackJob
  let tryIn: Alt<'x> -> ('x -> #Job<'y>) -> (exn -> #Job<'y>) -> Alt<'y>    = Hopac.Alt.tryIn

[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Proc =
  let self: unit -> Job<Proc>                                               = Hopac.Proc.self

module Infixes =
  let ( >>= ): Job<'x> -> ('x -> #Job<'y>) -> Job<'y>                       = Hopac.Infixes.( >>= )
  let ( *<- ): Ch<'x> -> 'x -> Alt<unit>                                    = Hopac.Infixes.( *<- )
  let ( <|> ): Alt<'x> -> Alt<'x> -> Alt<'x>                                = Hopac.Infixes.( <|> )
  let ( ^=> ): Alt<'x> -> ('x -> #Job<'y>) -> Alt<'y>                       = Hopac.Infixes.( ^=> )

[<AutoOpen>]
module Hopac =
  let timeOutMillis: int -> Alt<unit>                                       = Hopac.Hopac.timeOutMillis
  let memo: Job<'x> -> Promise<'x>                                          = Hopac.Hopac.memo
  let start: Job<unit> -> unit                                              = Hopac.Hopac.start
  let queue: Job<unit> -> unit                                              = Hopac.Hopac.queue

// ### Derived definitions

open Infixes

[<AutoOpen>]
module ``always and result via channels and memo`` =
  module Alt =
    let always: 'x -> Alt<'x> =
      fun x ->
        let xCh = Ch ()
        xCh *<- x |> start
        memo xCh :> Alt<_>

  module Job =
    let result: 'x -> Job<'x> =
      fun x -> Alt.always x :> Job<_>

[<AutoOpen>]
module ``run via start and blocking`` =
  open System.Threading
  type State<'x> = private Started | Returned of 'x | Raised of exn
  [<AutoOpen>]
  module Hopac =
    let run: Job<'x> -> 'x =
      fun xJ ->
        let state = ref Started
        let signal newState =
          lock state <| fun () ->
            state := newState
            Monitor.Pulse state
          Alt.always ()
        Job.tryIn xJ (Returned >> signal) (Raised >> signal) |> start
        let rec wait () =
          match !state with
           | Started -> Monitor.Wait state |> ignore ; wait ()
           | Returned x -> x
           | Raised e -> raise e
        lock state wait

[<AutoOpen>]
module ``never and abort via nack`` =
  module Alt =
    let never: unit -> Alt<'x> =
      fun () ->
        run << Alt.withNackJob <| fun nack ->
        nack ^=> fun () -> failwith "never"
        |> Alt.always
        |> Job.result

  module Job =
    let abort: unit -> Job<'x> =
      fun () -> Alt.never () :> Job<_>
