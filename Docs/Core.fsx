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

open System

// ### Core of Hopac

type Job<'x>                                                                = Hopac.Job<'x>
type Alt<'x>                                                                = Hopac.Alt<'x>
type Ch<'x>                                                                 = Hopac.Ch<'x>
type Promise<'x>                                                            = Hopac.Promise<'x>
type Proc                                                                   = Hopac.Proc
type Void                                                                   = Hopac.Void

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
module ``always via channels and memo`` =
  module Alt =
    let always: 'x -> Alt<'x> =
      fun x ->
        let xCh = Ch ()
        xCh *<- x |> start
        memo xCh :> Alt<_>

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
module ``never via nack`` =
  open Alt
  module Alt =
    let never: unit -> Alt<'x>                                              = fun () -> run << Alt.withNackJob <| fun n -> Alt.always << Alt.always <| n ^=> fun () -> failwith "never"

[<AutoOpen>]
module ``infixes for jobs`` =
  [<AutoOpen>]
  module Infixes =
    let ( >>=. ): Job<_>  ->    Job<'y> -> Job<'y>                          = fun _J yJ -> _J >>= fun _ -> yJ
    let ( >>-  ): Job<'x> -> ('x -> 'y) -> Job<'y>                          = fun xJ x2y -> xJ >>= (x2y >> Alt.always)
    let ( >>-. ): Job<_>  ->        'y  -> Job<'y>                          = fun _J y -> _J >>- fun _ -> y
    let ( >>-! ): Job<_>  ->       exn  -> Job<_>                           = fun _J e -> _J >>= fun _ -> raise e

[<AutoOpen>]
module ``basic job combinators`` =
  module Job =

    // Basic combinators
    let result: 'x   -> Job<'x>                                             = fun x -> Alt.always x :> Job<_>
    let   unit: unit -> Job<unit>                                           = result

    let      bind: ('x   -> #Job<'y>) -> Job<'x> -> Job<'y>                 = fun x2yJ xJ -> xJ >>= x2yJ
    let delayWith: ('x   -> #Job<'y>) ->     'x  -> Job<'y>                 = fun x2yJ x -> result x >>= x2yJ
    let       map: ('x   ->      'y)  -> Job<'x> -> Job<'y>                 = fun x2y xJ -> xJ >>- x2y
    let      lift: ('x   ->      'y)  ->     'x  -> Job<'y>                 = fun x2y x -> result x >>- x2y
    let     delay: (unit -> #Job<'y>) ->            Job<'y>                 = fun u2yJ -> unit () >>= u2yJ
    let     thunk: (unit ->      'y)  ->            Job<'y>                 = fun u2y -> delay (u2y >> result)

    let apply: Job<'x> -> Job<'x -> 'y> -> Job<'y>                          = fun xJ x2yJ -> x2yJ >>= (>>-) xJ

    let join: Job<#Job<'x>> -> Job<'x>                                      = fun xJJ -> xJJ >>= id

    let abort: unit -> Job<'x>                                              = fun () -> Alt.never () :> Job<_>

    let Ignore: Job<_> -> Job<unit>                                         = fun _J -> map ignore _J

    // Spawning jobs
    let queue:       Job<unit> -> Job<unit>                                 = fun uJ -> thunk <| fun () -> queue uJ
    let queueIgnore: Job<_>    -> Job<unit>                                 = fun _J -> queue <| Ignore _J
    let start:       Job<unit> -> Job<unit>                                 = fun uJ -> thunk <| fun () -> start uJ
    let startIgnore: Job<_>    -> Job<unit>                                 = fun _J -> start <| Ignore _J
    let server:      Job<Void> -> Job<unit>                                 = startIgnore

    // Exception handling
    let raises: exn -> Job<_>                                               = fun e -> thunk <| fun () -> raise e
    let tryInDelay: (unit -> #Job<'x>)
                 -> ('x -> #Job<'y>)
                 -> (exn -> #Job<'y>)
                 -> Job<'y>                                                 = fun u2xJ -> Job.tryIn <| delay u2xJ
    let tryWith:                Job<'x>  -> (exn -> #Job<'x>) -> Job<'x>    = fun xJ -> Job.tryIn xJ result
    let tryWithDelay: (unit -> #Job<'x>) -> (exn -> #Job<'x>) -> Job<'x>    = fun u2xJ -> tryWith <| delay u2xJ
    let tryFinallyJob:                Job<'x>  ->      Job<unit> -> Job<'x> = fun xJ uJ -> Job.tryIn xJ (fun x -> uJ >>-. x) (fun e -> uJ >>-! e)
    let tryFinallyJobDelay: (unit -> #Job<'x>) ->      Job<unit> -> Job<'x> = fun u2xJ -> tryFinallyJob <| delay u2xJ
    let tryFinallyFun:                Job<'x>  -> (unit -> unit) -> Job<'x> = fun xJ u2u -> tryFinallyJob xJ <| thunk u2u
    let tryFinallyFunDelay: (unit -> #Job<'x>) -> (unit -> unit) -> Job<'x> = fun u2xJ -> tryFinallyFun <| delay u2xJ
    let catch: Job<'x> -> Job<Choice<'x, exn>>                              = fun xJ -> Job.tryIn xJ <| lift Choice1Of2 <| lift Choice2Of2

    // Finalization
    let useIn: ('x -> #Job<'y>) -> 'x -> Job<'y> when 'x :> IDisposable     = fun x2yJ x -> tryFinallyFunDelay (fun () -> x2yJ x) x.Dispose
    let using: 'x -> ('x -> #Job<'y>) -> Job<'y> when 'x :> IDisposable     = fun x x2yJ -> useIn x2yJ x

    // Repeating an operation
    let forN:       int -> Job<unit> -> Job<unit>                           = fun n uJ -> let rec lp n = if n <= 0 then unit () else uJ >>= fun () -> lp (n-1) in lp n
    let forNIgnore: int -> Job<_>    -> Job<unit>                           = fun n _J -> forN n <| Ignore _J

    // Iterating over a range
    let forUpTo:         int -> int -> (int -> #Job<unit>) -> Job<unit>     = fun lo hi i2uJ -> let rec lp i = if i <= hi then i2uJ i >>= fun () -> lp (i+1) else unit () in lp lo
    let forUpToIgnore:   int -> int -> (int -> #Job<_>)    -> Job<unit>     = fun lo hi i2_J -> forUpTo lo hi (i2_J >> Ignore)
    let forDownTo:       int -> int -> (int -> #Job<unit>) -> Job<unit>     = fun hi lo i2uJ -> let rec lp i = if lo <= i then i2uJ i >>= fun () -> lp (i-1) else unit () in lp hi
    let forDownToIgnore: int -> int -> (int -> #Job<_>)    -> Job<unit>     = fun hi lo i2_J -> forDownTo hi lo (i2_J >> Ignore)

    // Iterating conditionally
    let whileDo:       (unit -> bool) ->           Job<unit> -> Job<unit>   = fun u2b uJ -> let rec lp () = if u2b () then uJ >>= lp else unit () in lp ()
    let whileDoDelay:  (unit -> bool) -> (unit -> #Job<_>)   -> Job<unit>   = fun u2b u2uJ -> whileDo u2b <| delay u2uJ
    let whileDoIgnore: (unit -> bool) ->           Job<_>    -> Job<unit>   = fun u2b _J -> whileDo u2b <| Ignore _J

    // Conditional
    let whenDo: bool -> Job<unit> -> Job<unit>                              = fun b uJ -> if b then uJ else unit ()

    // Server loops
    let forever:       Job<unit> -> Job<_>                                  = fun uJ -> let rec lp () = uJ >>= lp in lp ()
    let foreverIgnore: Job<_>    -> Job<_>                                  = fun _J -> forever <| Ignore _J
    let iterate: 'x -> ('x -> #Job<'x>) -> Job<_>                           = fun x x2xJ -> let rec lp x = x2xJ x >>= lp in lp x

    // Spawning server loops
    let foreverServer: Job<unit> -> Job<unit>                               = forever >> server
    let iterateServer: 'x -> ('x -> #Job<'x>) -> Job<unit>                  = fun x -> iterate x >> server

    // Sequences of jobs
    let seqCollect: seq<#Job<'x>> -> Job<ResizeArray<'x>>                   = fun xJs -> let xs = ResizeArray () in let xJe = xJs.GetEnumerator () in let rec lp () = if xJe.MoveNext () then xJe.Current >>= (xs.Add >> lp) else xJe.Dispose () ; result xs in lp ()
    let seqIgnore: seq<#Job<_>> -> Job<unit>                                = fun xJs -> let xJe = xJs.GetEnumerator () in let rec lp () = if xJe.MoveNext () then xJe.Current >>= fun _ -> lp () else xJe.Dispose () ; unit () in lp ()
[<AutoOpen>]
module ``basic alt combinators`` =
  module Alt =
    let unit: unit -> Alt<unit>                                             = Alt.always

    let once: 'x -> Alt<'x>                                                 = fun x -> let xCh = Ch () in xCh *<- x |> start ; xCh :> Alt<_>

    let zero: unit -> Alt<unit>                                             = Alt.never

    let prepareJob: (unit -> #Job<#Alt<'x>>) -> Alt<'x>                     = fun u2xAJ -> Alt.withNackJob <| fun _ -> u2xAJ ()
    let prepare:              Job<#Alt<'x>>  -> Alt<'x>                     = fun xAJ -> prepareJob <| fun () -> xAJ
    let prepareFun: (unit ->      #Alt<'x>)  -> Alt<'x>                     = fun u2xA -> prepareJob <| Job.lift u2xA

    let withNackFun: (Promise<unit> -> #Alt<'x>) -> Alt<'x>                 = fun n2xA -> Alt.withNackJob <| Job.lift n2xA

    let wrapAbortJob:      Job<unit> -> Alt<'x> -> Alt<'x>                  = fun uJ xA -> Alt.withNackJob <| fun n -> n >>=. uJ |> Job.start >>-. xA
    let wrapAbortFun: (unit -> unit) -> Alt<'x> -> Alt<'x>                  = fun u2u xA -> wrapAbortJob <| Job.thunk u2u <| xA

    let choose:   seq<#Alt<'x>> -> Alt<'x>                                  = fun xAs -> prepareFun <| fun () -> Seq.foldBack (<|>) xAs <| Alt.never ()
    let choosy: array<#Alt<'x>> -> Alt<'x>                                  = fun xAs -> choose xAs

    let afterJob: ('x -> #Job<'y>) -> Alt<'x> -> Alt<'y>                    = fun x2yJ xA -> xA ^=> x2yJ
    let afterFun: ('x ->      'y)  -> Alt<'x> -> Alt<'y>                    = fun x2y xA -> xA ^=> Job.lift x2y

    let Ignore: Alt<_> -> Alt<unit>                                         = fun _A -> afterFun ignore _A

    let raises: exn -> Alt<_>                                               = fun e -> prepareJob <| fun () -> raise e

    let tryFinallyJob: Alt<'x> ->      Job<unit> -> Alt<'x>                 = fun xA uJ -> Alt.tryIn xA (fun x -> uJ >>-. x) (fun e -> uJ >>-! e)
    let tryFinallyFun: Alt<'x> -> (unit -> unit) -> Alt<'x>                 = fun xA u2u -> tryFinallyJob xA <| Job.thunk u2u
