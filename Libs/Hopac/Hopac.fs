// Copyright (C) by Housemarque, Inc.

namespace Hopac

open System
open System.Collections.Generic
open System.Diagnostics
open System.Runtime.CompilerServices
open System.Threading
open Hopac.Core

/////////////////////////////////////////////////////////////////////////

type Void = Void

/////////////////////////////////////////////////////////////////////////

[<AutoOpen>]
module Util =
  let inline inc (i: byref<int>) : int =
    let j = i+1 in i <- j ; j
  let inline dec (i: byref<int>) : int =
    let j = i-1 in i <- j ; j

  let inline forward (e: exn) : 'x =
    raise (exn ("forwarded", e))

  module Option =
    let orDefaultOf x =
      match x with
       | None -> Unchecked.defaultof<_>
       | Some x -> x

  let inline ctor x2y x =
    {new Job<'y> () with
      override yJ'.DoJob (wr, yK) =
       yK.DoCont (&wr, x2y x)}

  type TryInCont<'x, 'y> (x2yJ: 'x -> Job<'y>,
                          e2yJ: exn -> Job<'y>,
                          yK: Cont<'y>) =
    inherit Cont<'x> ()
    override xK'.DoHandle (wr, e) =
     wr.Handler <- yK
     (e2yJ e).DoJob (&wr, yK)
    override xK'.DoWork (wr) =
     wr.Handler <- yK
     (x2yJ xK'.Value).DoJob (&wr, yK)
    override xK'.DoCont (wr, x) =
     wr.Handler <- yK
     (x2yJ x).DoJob (&wr, yK)

  type BindCont<'x, 'y> (x2yJ: 'x -> Job<'y>, yK: Cont<'y>) =
    inherit Cont<'x> ()
    override xK'.DoHandle (wr, e) = Handler.DoHandle (yK, &wr, e)
    override xK'.DoWork (wr) = (x2yJ xK'.Value).DoJob (&wr, yK)
    override xK'.DoCont (wr, x) = (x2yJ x).DoJob (&wr, yK)

  type MapCont<'x, 'y> (x2y: 'x -> 'y, yK: Cont<'y>) =
    inherit Cont<'x> ()
    override xK'.DoHandle (wr, e) = Handler.DoHandle (yK, &wr, e)
    override xK'.DoWork (wr) = yK.DoCont (&wr, x2y xK'.Value)
    override xK'.DoCont (wr, x) = yK.DoCont (&wr, x2y x)

  type DropCont<'x, 'y> (xK: Cont<'x>) =
    inherit Cont<'y> ()
    override yK'.DoHandle (wr, e) = Handler.DoHandle (xK, &wr, e)
    override yK'.DoWork (wr) = xK.DoWork (&wr)
    override yK'.DoCont (wr, _) = xK.DoWork (&wr)

  type Handler<'x> () =
    inherit Cont<'x> ()
    override xK'.DoHandle (wr, e) = Handler.DoHandle (null, &wr, e)
    override xK'.DoWork (_) = ()
    override xK'.DoCont (_, _) = ()

  let mutable globalScheduler : Scheduler = null

  let reallyGlobalScheduler () =
    lock typeof<Scheduler> <| fun () ->
    let sr = Scheduler (Environment.ProcessorCount,
                        Unchecked.defaultof<_>,
                        null)
    globalScheduler <- sr
    sr

  let inline initGlobalScheduler () =
    match globalScheduler with
     | null -> reallyGlobalScheduler ()
     | sr -> sr

  let mutable globalTimer : Timer = null

  let reallyInitGlobalTimer () =
    let sr = initGlobalScheduler ()
    lock typeof<Timer> <| fun () ->
    let tr = Timer sr
    globalTimer <- tr
    tr

  let inline initGlobalTimer () =
    match globalTimer with
     | null -> reallyInitGlobalTimer ()
     | tr -> tr

/////////////////////////////////////////////////////////////////////////

module IVar =
  module Now =
    let inline create () = IVar<'x> ()
  let create () = ctor Now.create ()
  let inline fill (xI: IVar<'x>) (x: 'x) = IVarFill<'x> (xI, x) :> Job<unit>
  let inline read (xI: IVar<'x>) = xI :> Job<'x>
  module Alt =
    let inline read (xI: IVar<'x>) = xI :> Alt<'x>

/////////////////////////////////////////////////////////////////////////

module Ch =
  module Now =
    let inline create () = Ch<'x> ()
  module Global =
    [<MethodImpl(MethodImplOptions.NoInlining)>]
    let send (xCh: Ch<'x>) (x: 'x) = Ch<'x>.Send (globalScheduler, xCh, x)
  let create () = ctor Now.create ()
  let inline give (xCh: Ch<'x>) (x: 'x) = ChGive<'x> (xCh, x) :> Job<unit>
  let inline send (xCh: Ch<'x>) (x: 'x) = ChSend<'x> (xCh, x) :> Job<unit>
  let inline take (xCh: Ch<'x>) = xCh :> Job<'x>
  module Alt =
    let inline give (xCh: Ch<'x>) (x: 'x) = ChGive<'x> (xCh, x) :> Alt<unit>
    let inline take (xCh: Ch<'x>) = xCh :> Alt<'x>

/////////////////////////////////////////////////////////////////////////

module Mailbox =
  module Now =
    let inline create () = Mailbox<'x> ()
  module Global =
    [<MethodImpl(MethodImplOptions.NoInlining)>]
    let send (xMb: Mailbox<'x>) (x: 'x) =
      Mailbox<'x>.Send (globalScheduler, xMb, x)
  let create () = ctor Now.create ()
  let inline send (xMb: Mailbox<'x>) (x: 'x) =
    MailboxSend<'x> (xMb, x) :> Job<unit>
  let inline take (xMb: Mailbox<'x>) = xMb :> Job<'x>
  module Alt =
    let inline take (xMb: Mailbox<'x>) = xMb :> Alt<'x>

/////////////////////////////////////////////////////////////////////////

module Promise =
  let start (xJ: Job<'x>) =
    {new Job<Promise<'x>> () with
      override self.DoJob (wr, xPrK) =
       Cont.Do (xPrK, &wr, Promise<'x> (&wr, xJ))}
  module Now =
    let inline delay (xJ: Job<'x>) = Promise<'x> (xJ)
    let inline withValue (x: 'x) = Promise<'x> (x)
    let inline withFailure (e: exn) = Promise<'x> (e)
  let delay (xJ: Job<'x>) = ctor Now.delay xJ
  let inline read (xPr: Promise<'x>) = xPr :> Job<'x>
  module Alt =
    let inline read (xPr: Promise<'x>) = xPr :> Alt<'x>
    
/////////////////////////////////////////////////////////////////////////

module Alt =
  let inline always (x: 'x) =
    Always<'x> (x) :> Alt<'x>

  let never () =
    {new Alt<'x> () with
      override xA'.DoJob (wr, _) = ()
      override xA'.TryAlt (wr, i, pk, xK, xE) = xE.TryElse (&wr, i, pk, xK)}

  type GuardJobCont<'x> (xK: Cont<'x>) =
    inherit Cont<Alt<'x>> ()
    override xAK'.DoHandle (wr, e) = Handler.DoHandle (xK, &wr, e)
    override xAK'.DoWork (wr) = xAK'.Value.DoJob (&wr, xK)
    override xAK'.DoCont (wr, xA) = xA.DoJob (&wr, xK)

  let guard (xAJ: Job<Alt<'x>>) =
    {new Alt<'x> () with
      override xA'.DoJob (wr, xK) =
       xAJ.DoJob (&wr, GuardJobCont xK)
      override xA'.TryAlt (wr, i, pk, xK, xE) =
       let xAK' = {new Cont<Alt<'x>> () with
        override xAK'.DoHandle (wr, e) =
         Pick.PickClaimed pk
         Handler.DoHandle (xK, &wr, e)
        override xAK'.DoWork (wr) =
         Pick.Unclaim pk
         xAK'.Value.TryAlt (&wr, i, pk, xK, xE)
        override xAK'.DoCont (wr, xA) =
         Pick.Unclaim pk
         xA.TryAlt (&wr, i, pk, xK, xE)}
       if Pick.Claim pk then
         xAJ.DoJob (&wr, xAK')}

  let delay (u2xA: unit -> Alt<'x>) =
    {new Alt<'x> () with
      override xA'.DoJob (wr, xK) = (u2xA ()).DoJob (&wr, xK)
      override xA'.TryAlt (wr, i, pk, xK, xE) =
       if Pick.Claim pk then
         let mutable e = null
         let mutable xA = null
         try
           xA <- u2xA ()
         with exn ->
           e <- exn
         match xA with
          | null ->
            Pick.PickClaimed pk
            Handler.DoHandle (xK, &wr, e)
          | xA ->
            Pick.Unclaim pk
            xA.TryAlt (&wr, i, pk, xK, xE)}

  let inline pick (xA: Alt<'x>) =
    xA :> Job<'x>
     
  type WithNackElse<'x> (nk: Nack, xE: Else<'x>) =
    inherit Else<'x> ()
    override xE'.TryElse (wr, i, pk, xK) =
      nk.I1 <- i
      xE.TryElse (&wr, i, pk, xK)

  let inline WithNackCont (pk: Pick, xK: Cont<'x>, xE: Else<'x>) =
    {new Cont<Alt<'x>> () with
      override xAK'.DoHandle (wr, e) =
       Pick.PickClaimed pk
       Handler.DoHandle (xK, &wr, e)
      override xAK'.DoWork (wr) =
       Pick.Unclaim pk
       let nk = pk.Nacks
       xAK'.Value.TryAlt (&wr, nk.I0, pk, xK, WithNackElse (nk, xE))
      override xAK'.DoCont (wr, xA) =
       Pick.Unclaim pk
       let nk = pk.Nacks
       xA.TryAlt (&wr, nk.I0, pk, xK, WithNackElse (nk, xE))}

  let withNack (nack2xAJ: Alt<unit> -> Job<Alt<'x>>) =
    {new Alt<'x> () with
      override xA'.DoJob (wr, xK) =
       (nack2xAJ (never ())).DoJob (&wr, GuardJobCont xK)
      override xA'.TryAlt (wr, i, pk, xK, xE) =
       match Pick.AddNack (pk, i) with
        | null -> ()
        | nk -> (nack2xAJ nk).DoJob (&wr, WithNackCont (pk, xK, xE))}

  module Infixes =
    let (<|>) (xA1: Alt<'x>) (xA2: Alt<'x>) =
      {new Alt<'x> () with
        override xA'.DoJob (wr, xK) =
         xA1.TryAlt (&wr, 0, Pick (), xK, {new Else_State<'x, _> (xA2) with
          override xE'.TryElse (wr, i, pk, xK) =
           match xE'.State with
            | null -> ()
            | xA2 ->
              xE'.State <- null
              xA2.TryAlt (&wr, i, pk, xK, xE')})
        override xA'.TryAlt (wr, i, pk, xK, xE) =
         xA1.TryAlt (&wr, i, pk, xK, {new Else<'x> () with
          override xE'.TryElse (wr, i, pk, xK) =
           xA2.TryAlt (&wr, i, pk, xK, xE)})}

    type MapElse<'x, 'y> (yK: Cont<'y>, yE: Else<'y>) =
      inherit Else<'x> ()
      override xE'.TryElse (wr, i, pk, _) = yE.TryElse (&wr, i, pk, yK)
 
    let (>->) (xA: Alt<'x>) (x2y: 'x -> 'y) =
      {new Alt<'y> () with
        override yA'.DoJob (wr, yK) =
         xA.DoJob (&wr, MapCont (x2y, yK))
        override yA'.TryAlt (wr, i, pk, yK, yE) =
         xA.TryAlt (&wr, i, pk, MapCont (x2y, yK), MapElse (yK, yE))}

    let (>=>) (xA: Alt<'x>) (x2yJ: 'x -> Job<'y>) =
      {new Alt<'y> () with
        override yA'.DoJob (wr, yK) =
         xA.DoJob (&wr, BindCont (x2yJ, yK))
        override yA'.TryAlt (wr, i, pk, yK, yE) =
         xA.TryAlt (&wr, i, pk, BindCont (x2yJ, yK), MapElse (yK, yE))}

  let choose (xAs: seq<Alt<'x>>) =
    {new Alt<'x> () with
      override xA'.DoJob (wr, xK) =
       let xAs = xAs.GetEnumerator ()
       if xAs.MoveNext () then
         xAs.Current.TryAlt (&wr, 0, Pick (), xK, {new Else<'x> () with
          override xE'.TryElse (wr, i, pk, xK) =
           if xAs.MoveNext () then
             xAs.Current.TryAlt (&wr, i, pk, xK, xE')})
      override xA'.TryAlt (wr, i, pk, xK, xE) =
       let xAs = xAs.GetEnumerator ()
       if xAs.MoveNext () then
         xAs.Current.TryAlt (&wr, i, pk, xK, {new Else<'x> () with
          override xE'.TryElse (wr, i, pk, xK) =
           if xAs.MoveNext () then
             xAs.Current.TryAlt (&wr, i, pk, xK, xE')
           else
             xE.TryElse (&wr, i, pk, xK)})
       else
         xE.TryElse (&wr, i, pk, xK)}

  let inline select xAs = choose xAs :> Job<'x>

  let tryIn (xA: Alt<'x>) (x2yJ: 'x -> Job<'y>) (e2yJ: exn -> Job<'y>) =
    {new Alt<'y> () with
      override yJ'.DoJob (wr, yK) =
       let xK = TryInCont (x2yJ, e2yJ, yK)
       wr.Handler <- xK
       xA.DoJob (&wr, xK)
      override yJ'.TryAlt (wr, i, pk, yK, yE) =
       let xK = TryInCont (x2yJ, e2yJ, yK)
       wr.Handler <- xK
       xA.TryAlt (&wr, i, pk, xK, {new Else<'x> () with
        override xE'.TryElse (wr, i, pk, _) =
         wr.Handler <- yK
         yE.TryElse (&wr, i, pk, yK)})}

/////////////////////////////////////////////////////////////////////////

module Scheduler =
  module Global =
    let setTopLevelHandler e2uJO =
      let sr = initGlobalScheduler ()
      sr.TopLevelHandler <- Option.orDefaultOf e2uJO

  type Create =
    {NumWorkers: option<int>
     TopLevelHandler: option<exn -> Job<unit>>
     IdleHandler: option<Job<int>>}
    static member Def: Create =
      {NumWorkers = None
       TopLevelHandler = None
       IdleHandler = None}

  let create (c: Create) =
    Scheduler ((match c.NumWorkers with
                 | None -> Environment.ProcessorCount
                 | Some n ->
                   if n < 1 then
                     failwith "Invalid number of workers specified: %d" n
                   n),
               Option.orDefaultOf c.TopLevelHandler,
               Option.orDefaultOf c.IdleHandler)

  let startWithActions (sr: Scheduler)
                       (eF: exn -> unit)
                       (xF: 'x -> unit)
                       (xJ: Job<'x>) =
    Worker.RunOnThisThread (sr, xJ, {new Cont<'x> () with
     override xK'.DoHandle (_, e) = eF e
     override xK'.DoWork (_) = xF xK'.Value
     override xK'.DoCont (_, x) = xF x})

  let start (sr: Scheduler) (xJ: Job<'x>) =
    Worker.RunOnThisThread (sr, xJ, Handler<'x> ())

  let server (sr: Scheduler) (vJ: Job<Void>) =
    Worker.RunOnThisThread (sr, vJ, null)

  let run (sr: Scheduler) (xJ: Job<'x>) =
    let xK' = {new Cont_State<_, _, _> () with
     override xK'.DoHandle (wr, e) =
      xK'.State1 <- e
      Condition.Pulse (xK', &xK'.State2)
     override xK'.DoWork (wr) =
      Condition.Pulse (xK', &xK'.State2)
     override xK'.DoCont (wr, x) =
      xK'.Value <- x
      Condition.Pulse (xK', &xK'.State2)}
    Worker.RunOnThisThread (sr, xJ, xK')
    Condition.Wait (xK', &xK'.State2)
    match xK'.State1 with
     | null -> xK'.Value
     | e -> Util.forward e

  let signal (sr: Scheduler) =
    Scheduler.Signal sr

  let isIdle (sr: Scheduler) =
    Scheduler.IsIdle sr

/////////////////////////////////////////////////////////////////////////

module Job =
  module Global =
    let startWithActions eF xF xJ =
      Scheduler.startWithActions (initGlobalScheduler ()) eF xF xJ
    let start xJ = Scheduler.start (initGlobalScheduler ()) xJ
    let server vJ = Scheduler.server (initGlobalScheduler ()) vJ
    let run xJ = Scheduler.run (initGlobalScheduler ()) xJ

  ///////////////////////////////////////////////////////////////////////

  let delay (u2xJ: unit -> Job<'x>) =
    {new Job<'x> () with
      override xJ'.DoJob (wr, xK) =
       (u2xJ ()).DoJob (&wr, xK)}

  let delayWith (x2yJ: 'x -> Job<'y>) (x: 'x) =
    {new Job<'y> () with
      override yJ'.DoJob (wr, yK) =
       (x2yJ x).DoJob (&wr, yK)}

  let lift (x2y: 'x -> 'y) (x: 'x) =
    {new Job<'y> () with
      override yJ'.DoJob (wr, yK) = Cont.Do (yK, &wr, x2y x)}

  let thunk (u2x: unit -> 'x) =
    {new Job<'x> () with
      override xJ'.DoJob (wr, xK) = Cont.Do (xK, &wr, u2x ())}

  let forN (n: int) (xJ: Job<'x>) =
    {new Job<unit> () with
      override uJ'.DoJob (wr, uK) =
       {new Cont_State<_, _> (n) with
         override xK'.DoHandle (wr, e) = Handler.DoHandle (uK, &wr, e)
         override xK'.DoCont (wr, _) =
          let n = xK'.State
          if 0 < n then
            xK'.State <- n - 1
            xJ.DoJob (&wr, xK')
          else
            uK.DoWork (&wr)
         override xK'.DoWork (wr) =
          let n = xK'.State
          if 0 < n then
            xK'.State <- n - 1
            xJ.DoJob (&wr, xK')
          else
            uK.DoWork (&wr)}.DoWork (&wr)}

  module Internal =
    let inline mkFor more next i0 i1 (i2xJ: _ -> Job<'x>) =
      {new Job<unit> () with
        override uJ'.DoJob (wr, uK) =
         {new Cont_State<_, _> (i0) with
           override xK'.DoHandle (wr, e) = Handler.DoHandle (uK, &wr, e)
           override xK'.DoCont (wr, _) =
            let i = xK'.State
            if more i i1 then
              xK'.State <- next i
              (i2xJ i).DoJob (&wr, xK')
            else
              uK.DoWork (&wr)
           override xK'.DoWork (wr) =
            let i = xK'.State
            if more i i1 then
              xK'.State <- next i
              (i2xJ i).DoJob (&wr, xK')
            else
              uK.DoWork (&wr)}.DoWork (&wr)}

  let forUpTo (i0: int) (i1: int) (i2xJ: int -> Job<'x>) =
    Internal.mkFor (fun i i1 -> i <= i1) (fun i -> i + 1) i0 i1 i2xJ

  let forDownTo (i0: int) (i1: int) (i2xJ: int -> Job<'x>) =
    Internal.mkFor (fun i i1 -> i1 <= i) (fun i -> i - 1) i0 i1 i2xJ

  let forever (xJ: Job<'x>) =
    {new Job<'y> () with
      override yJ'.DoJob (wr, yK) = {new Cont<'x> () with
       override xK'.DoHandle (wr, e) = Handler.DoHandle (yK, &wr, e)
       override xK'.DoWork (wr) = xJ.DoJob (&wr, xK')
       override xK'.DoCont (wr, _) = xJ.DoJob (&wr, xK')}.DoWork (&wr)}

  let iterate (x: 'x) (x2xJ: 'x -> Job<'x>) =
    {new Job<'y> () with
      override yJ'.DoJob (wr, yK) = {new Cont<_> () with
       override xK'.DoHandle (wr, e) = Handler.DoHandle (yK, &wr, e)
       override xK'.DoWork (wr) = (x2xJ xK'.Value).DoJob (&wr, xK')
       override xK'.DoCont (wr, x) = (x2xJ x).DoJob (&wr, xK')}.DoCont (&wr, x)}

  let whileDo (cond: unit -> bool) (xJ: Job<'x>) =
    {new Job<unit> () with
      override uJ'.DoJob (wr, uK) = {new Cont<'x> () with
       override xK'.DoHandle (wr, e) = Handler.DoHandle (uK, &wr, e)
       override xK'.DoCont (wr, _) =
        if cond () then xJ.DoJob (&wr, xK') else uK.DoWork (&wr)
       override xK'.DoWork (wr) =
        if cond () then xJ.DoJob (&wr, xK') else uK.DoWork (&wr)}.DoWork (&wr)}

  let result (x: 'x) =
    if sizeof<IntPtr> = 8 then {new Job<'x> () with
      override self.DoJob (wr, xK) =
       xK.DoCont (&wr, x)}
    else {new Job<'x> () with
      override self.DoJob (wr, xK) =
       Cont.Do (xK, &wr, x)}

  let raises (e: exn) =
    {new Job<_> () with
      override xJ.DoJob (wr, xK) = Handler.DoHandle (xK, &wr, e)}

  let unit = result ()

  let inline whenDo (b: bool) (uJ: Job<unit>) =
    if b then uJ else unit

  ///////////////////////////////////////////////////////////////////////

  module Infixes =
    let (>>=) (xJ: Job<'x>) (x2yJ: 'x -> Job<'y>) =
      {new Job<'y> () with
        override yJ'.DoJob (wr, yK) =
         xJ.DoJob (&wr, BindCont (x2yJ, yK))}

    let (>>.) (xJ: Job<'x>) (yJ: Job<'y>) =
      {new Job<'y> () with
        override yJ'.DoJob (wr, yK) =
         xJ.DoJob (&wr, {new Cont<'x> () with
          override xK'.DoHandle (wr, e) = Handler.DoHandle (yK, &wr, e)
          override xK'.DoWork (wr) = yJ.DoJob (&wr, yK)
          override xK'.DoCont (wr, _) = yJ.DoJob (&wr, yK)})}

    let (.>>) (xJ: Job<'x>) (yJ: Job<'y>) =
      {new Job<'x> () with
        override xJ'.DoJob (wr, xK) =
         xJ.DoJob (&wr, {new Cont<'x> () with
          override xK'.DoHandle (wr, e) = Handler.DoHandle (xK, &wr, e)
          override xK'.DoWork (wr) =
           xK.Value <- xK'.Value
           yJ.DoJob (&wr, DropCont xK)
          override xK'.DoCont (wr, x) =
           xK.Value <- x
           yJ.DoJob (&wr, DropCont xK)})}

    let (|>>) (xJ: Job<'x>) (x2y: 'x -> 'y) =
      {new Job<'y> () with
        override yJ'.DoJob (wr, yK) =
         xJ.DoJob (&wr, MapCont (x2y, yK))}

    let (>>%) (xJ: Job<'x>) (y: 'y) =
      {new Job<'y> () with
        override yJ'.DoJob (wr, yK) =
         yK.Value <- y
         xJ.DoJob (&wr, DropCont yK)}

    let (>>!) (xJ: Job<'x>) (e: exn) =
      {new Job<'y> () with
        override yJ'.DoJob (wr, yK) =
         xJ.DoJob (&wr, {new Cont<'x> () with
          override xK'.DoHandle (wr, e) = Handler.DoHandle (yK, &wr, e)
          override xK'.DoWork (wr) = Handler.DoHandle (yK, &wr, e)
          override xK'.DoCont (wr, _) = Handler.DoHandle (yK, &wr, e)})}

    type PairCont2<'x, 'y> (x: 'x, xyK: Cont<'x * 'y>) =
      inherit Cont<'y> ()
      override yK'.DoHandle (wr, e) = Handler.DoHandle (xyK, &wr, e)
      override yK'.DoWork (wr) = xyK.DoCont (&wr, (x, yK'.Value))
      override yK'.DoCont (wr, y) = xyK.DoCont (&wr, (x, y))

    type PairCont<'x, 'y> (yJ: Job<'y>, xyK: Cont<'x * 'y>) =
      inherit Cont<'x> ()
      override xK'.DoHandle (wr, e) = Handler.DoHandle (xyK, &wr, e)
      override xK'.DoWork (wr) = yJ.DoJob (&wr, PairCont2<'x, 'y> (xK'.Value, xyK))
      override xK'.DoCont (wr, x) = yJ.DoJob (&wr, PairCont2<'x, 'y> (x, xyK))

    let (<&>) (xJ: Job<'x>) (yJ: Job<'y>) =
      {new Job<'x * 'y> () with
        override xyJ'.DoJob (wr, xyK) =
         xJ.DoJob (&wr, PairCont (yJ, xyK))}

    let (<*>) (xJ: Job<'x>) (yJ: Job<'y>) =
      {new Job<'x * 'y> () with
        override xyJ'.DoJob (wr, xyK) =
          match wr.Scheduler.WorkStack with
           | null ->
             let yK' = ParTuple<'x, 'y> (xyK)
             Worker.PushNew (&wr, {new Cont_State<_, _> (xJ) with
              override xK'.DoHandle (wr, e) = Handler.DoHandle (yK', &wr, e)
              override xK'.DoCont (wr, a) = yK'.DoOtherCont (&wr, a)
              override xK'.DoWork (wr) =
               match xK'.State with
                | null -> yK'.DoOtherCont (&wr, xK'.Value)
                | xJ ->
                  xK'.State <- null
                  xJ.DoJob (&wr, xK')})
             yJ.DoJob (&wr, yK')
           | _ ->
            xJ.DoJob (&wr, PairCont (yJ, xyK))}

  ///////////////////////////////////////////////////////////////////////

  type DelayWithWork<'x, 'y> (x2yJ: 'x -> Job<'y>, x: 'x, yK: Cont<'y>) =
    inherit Work ()
    override work.DoHandle (wr, e) = Handler.DoHandle (yK, &wr, e)
    override work.DoWork (wr) = (x2yJ x).DoJob (&wr, yK)

  let tryIn (xJ: Job<'x>) (x2yJ: 'x -> Job<'y>) (e2yJ: exn -> Job<'y>) =
    {new Job<'y> () with
      override yJ'.DoJob (wr, yK) =
       let xK' = TryInCont (x2yJ, e2yJ, yK)
       wr.Handler <- xK'
       xJ.DoJob (&wr, xK')}

  let tryWith (xJ: Job<'x>) (e2xJ: exn -> Job<'x>) =
    {new Job<'x> () with
      override xJ'.DoJob (wr, xK) =
       let xK' = {new Cont<'x> () with
        override xK'.DoHandle (wr, e) =
         wr.Handler <- xK
         (e2xJ e).DoJob (&wr, xK)
        override xK'.DoWork (wr) =
         wr.Handler <- xK
         xK.DoCont (&wr, xK'.Value)
        override xK'.DoCont (wr, x) =
         wr.Handler <- xK
         xK.DoCont (&wr, x)}
       wr.Handler <- xK'
       xJ.DoJob (&wr, xK')}

  let tryFinallyFun (xJ: Job<'x>) (u2u: unit -> unit) =
    {new Job<'x> () with
      override xJ'.DoJob (wr, xK) =
       let xK' = {new Cont<'x> () with
        override xK'.DoHandle (wr, e) =
         wr.Handler <- xK
         u2u ()
         Handler.DoHandle (xK, &wr, e)
        override xK'.DoWork (wr) =
         wr.Handler <- xK
         u2u ()
         xK.DoCont (&wr, xK'.Value)
        override xK'.DoCont (wr, x) =
         wr.Handler <- xK
         u2u ()
         xK.DoCont (&wr, x)}
       wr.Handler <- xK'
       xJ.DoJob (&wr, xK')}

  let tryFinallyJob (xJ: Job<'x>) (uJ: Job<unit>) =
    {new Job<'x> () with
      override xJ'.DoJob (wr, xK) =
       let xK' = {new Cont<'x> () with
        override xK'.DoHandle (wr, e) =
         wr.Handler <- xK
         uJ.DoJob (&wr, FailCont<unit> (xK, e))
        override xK'.DoWork (wr) =
         wr.Handler <- xK
         xK.Value <- xK'.Value
         uJ.DoJob (&wr, DropCont<'x, unit> (xK))
        override xK'.DoCont (wr, x) =
         wr.Handler <- xK
         xK.Value <- x
         uJ.DoJob (&wr, DropCont<'x, unit> (xK))}
       wr.Handler <- xK'
       xJ.DoJob (&wr, xK')}

  let using (x: 'x when 'x :> IDisposable) (x2yJ: 'x -> Job<'y>) =
    {new Job<'y> () with
      override yJ'.DoJob (wr, yK) =
       let yK' = {new Cont<'y> () with
        override yK'.DoHandle (wr, e) =
         wr.Handler <- yK
         x.Dispose ()
         Handler.DoHandle (yK, &wr, e)
        override yK'.DoWork (wr) =
         wr.Handler <- yK
         x.Dispose ()
         yK.DoCont (&wr, yK'.Value)
        override yK'.DoCont (wr, y) =
         wr.Handler <- yK
         x.Dispose ()
         yK.DoCont (&wr, y)}
       wr.Handler <- yK'
       (x2yJ x).DoJob (&wr, yK')}

  let catch (xJ: Job<'x>) =
    {new Job<Choice<'x, exn>> () with
      override cJ'.DoJob (wr, cK) =
       let xK' = {new Cont<'x> () with
        override xK'.DoHandle (wr, e) =
         wr.Handler <- cK
         cK.DoCont (&wr, Choice2Of2 e)
        override xK'.DoWork (wr) =
         wr.Handler <- cK
         cK.DoCont (&wr, Choice1Of2 xK'.Value)
        override xK'.DoCont (wr, x) =
         wr.Handler <- cK
         cK.DoCont (&wr, Choice1Of2 x)}
       wr.Handler <- xK'
       xJ.DoJob (&wr, xK')}

  ///////////////////////////////////////////////////////////////////////

  let start (xJ: Job<'x>) =
    {new Job<unit> () with
      override uJ'.DoJob (wr, uK) =
       Worker.PushNew (&wr, {new Work () with
        override w'.DoHandle (wr, e) = Handler.DoHandle (null, &wr, e)
        override w'.DoWork (wr) = xJ.DoJob (&wr, Handler<'x> ())})
       Work.Do (uK, &wr)}

  let server (vJ: Job<Void>) =
    {new Job<unit> () with
      override uJ'.DoJob (wr, uK) =
       Worker.PushNew (&wr, {new Work () with
        override w'.DoHandle (wr, e) = Handler.DoHandle (null, &wr, e)
        override w'.DoWork (wr) = vJ.DoJob (&wr, null)})
       Work.Do (uK, &wr)}

  ///////////////////////////////////////////////////////////////////////

  let seqCollect (xJs: seq<Job<'x>>) =
    {new Job<ResizeArray<'x>> () with
      override self.DoJob (wr, xsK) =
       let xJs = xJs.GetEnumerator ()
       xsK.Value <- ResizeArray<_> ()
       if xJs.MoveNext () then
         xJs.Current.DoJob (&wr, {new Cont<'x> () with
          override xK'.DoHandle (wr, e) = Handler.DoHandle (xsK, &wr, e)
          override xK'.DoWork (wr) =
           xsK.Value.Add xK'.Value
           if xJs.MoveNext () then
             xJs.Current.DoJob (&wr, xK')
           else
             xsK.DoWork (&wr)
          override xK'.DoCont (wr, x) =
           xsK.Value.Add x
           if xJs.MoveNext () then
             xJs.Current.DoJob (&wr, xK')
           else
             xsK.DoWork (&wr)})
       else
         Work.Do (xsK, &wr)}

  let seqIgnore (xJs: seq<Job<'x>>) =
    {new Job<unit> () with
      override self.DoJob (wr, uK) =
       let xJs = xJs.GetEnumerator ()
       if xJs.MoveNext () then
         xJs.Current.DoJob (&wr, {new Cont<'x> () with
          override xK'.DoHandle (wr, e) = Handler.DoHandle (uK, &wr, e)
          override xK'.DoWork (wr) =
           if xJs.MoveNext () then
             xJs.Current.DoJob (&wr, xK')
           else
             uK.DoWork (&wr)
          override xK'.DoCont (wr, _) =
           if xJs.MoveNext () then
             xJs.Current.DoJob (&wr, xK')
           else
             uK.DoWork (&wr)})
       else
         Work.Do (uK, &wr)}

  type [<AbstractClass>] ConCollect<'y> =
    inherit Work
    [<DefaultValue>] val mutable Lock: WorkQueueLock
    [<DefaultValue>] val mutable N: int
    [<DefaultValue>] val mutable Exns: ResizeArray<exn>
    val ysK: Cont<ResizeArray<'y>>
    member cc'.Inc (wr: byref<Worker>) =
     if cc'.ysK.Value.Count < Util.inc &cc'.N then
       let ysK = cc'.ysK
       wr.Handler <- ysK
       match cc'.Exns with
        | null -> ysK.DoWork (&wr)
        | exns -> Handler.DoHandle (ysK, &wr, AggregateException exns)
    override cc'.DoHandle (wr, e) =
     let exns =
       match cc'.Exns with
        | null ->
          let exns = ResizeArray<_> ()
          cc'.Exns <- exns
          exns
        | exns -> exns
     exns.Add e
     cc'.Inc (&wr)
    member cc'.OutsideDoHandle (wr: byref<Worker>, e) =
     cc'.Lock.Enter (&wr, {new Work () with
      override wk.DoHandle (_, _) = ()
      override wk.DoWork (wr) = cc'.DoHandle (&wr, e)})
    new (ysK) = {
      inherit Work ()
      ysK = ysK
    }

  let conCollect (xJs: seq<Job<'x>>) =
    {new Job<ResizeArray<'x>> () with
      override xsJ'.DoJob (wr, xsK_) =
       xsK_.Value <- ResizeArray<_> ()
       let xJs = xJs.GetEnumerator ()
       let cc' = {new ConCollect<'x> (xsK_) with
         override cc'.DoWork (wr) =
          let mutable nth = 0
          while xJs.MoveNext () do
            cc'.Lock.ExitAndEnter (&wr, cc')
            let xJ = xJs.Current
            cc'.ysK.Value.Add Unchecked.defaultof<_>
            Worker.PushNew (&wr, {new Cont_State<_, _, _> (xJ, Util.dec &nth) with
             override xK'.DoHandle (wr, e) = cc'.OutsideDoHandle (&wr, e)
             override xK'.DoCont (wr, x) =
              xK'.Value <- x
              xK'.State2 <- ~~~ xK'.State2
              cc'.Lock.Enter (&wr, xK')
             override xK'.DoWork (wr) =
              match xK'.State1 with
               | null ->
                 let i = xK'.State2
                 if i < 0 then
                   xK'.State2 <- ~~~ i
                   cc'.Lock.Enter (&wr, xK')
                 else
                   cc'.ysK.Value.[i] <- xK'.Value
                   cc'.Inc (&wr)
               | xJ ->
                 xK'.State1 <- null
                 xJ.DoJob (&wr, xK')})
          cc'.Inc (&wr)}
       wr.Handler <- cc'
       cc'.Lock.Enter (&wr, cc')}

  type ConIgnore =
    inherit Handler
    [<DefaultValue>] val mutable Finished: int
    [<DefaultValue>] val mutable Started: int
    [<DefaultValue>] val mutable Exns: ResizeArray<exn> 
    val uK: Cont<unit>
    new (uK) = {uK = uK}
    static member inline Inc (self: ConIgnore) =
     self.Started <- self.Started - 1
    static member Continue (self: ConIgnore, wr: byref<Worker>) =
     let uK = self.uK
     wr.Handler <- uK
     match self.Exns with
      | null -> uK.DoWork (&wr)
      | exns -> Handler.DoHandle (uK, &wr, AggregateException exns)
    static member inline Dec (self: ConIgnore, wr: byref<Worker>) =
     if Interlocked.Increment &self.Finished = self.Started then
       ConIgnore.Continue (self, &wr)
    static member inline Done (self: ConIgnore, wr: byref<Worker>) =
     self.Started <- 1 - self.Started
     ConIgnore.Dec (self, &wr)
    static member AddExn (self: ConIgnore, e: exn) =
     lock self <| fun () ->
     let exns =
       match self.Exns with
        | null ->
          let exns = ResizeArray<_> ()
          self.Exns <- exns
          exns
        | exns -> exns
     exns.Add e
    static member OutsideDoHandle (self: ConIgnore, wr: byref<Worker>, e: exn) =
     ConIgnore.AddExn (self, e)
     ConIgnore.Dec (self, &wr)
    override self.DoHandle (wr: byref<Worker>, e: exn) =
     ConIgnore.AddExn (self, e)
     ConIgnore.Done (self, &wr)

  type ConIgnore_State<'s> =
    inherit ConIgnore
    val mutable State: 's
    new (uK, s) = {inherit ConIgnore (uK); State=s}

  let conIgnore (xJs: seq<Job<'x>>) =
    {new Job<unit> () with
      override uJ.DoJob (wr, uK) =
       let join = ConIgnore (uK)
       wr.Handler <- join
       let xJs = xJs.GetEnumerator ()
       while xJs.MoveNext () do
         ConIgnore.Inc join
         Worker.PushNew (&wr, {new Cont_State<_, _> (xJs.Current) with
          override xK'.DoHandle (wr, e) = ConIgnore.OutsideDoHandle (join, &wr, e)
          override xK'.DoCont (wr, _) = ConIgnore.Dec (join, &wr)
          override xK'.DoWork (wr) =
           match xK'.State with
            | null ->
              ConIgnore.Dec (join, &wr)
            | xJ ->
              xK'.State <- null
              xJ.DoJob (&wr, xK')})
       ConIgnore.Done (join, &wr)}

  ///////////////////////////////////////////////////////////////////////

  let doAsyncCallback = AsyncCallback (fun ar ->
    match ar.AsyncState with
      | :? WorkWithReady<IAsyncResult> as ta ->
        ta.Ready(ar);
      | _ ->
        failwith "Bug")

  type AsyncBeginEnd<'x> (sr, doEnd, xK: Cont<'x>) =
    inherit WorkWithReady<IAsyncResult> (sr)
    override self.DoHandle (wr, e) = Handler.DoHandle (xK, &wr, e)
    override self.DoWork (wr) = xK.DoCont (&wr, doEnd self.Value)

  let fromBeginEnd (doBegin: AsyncCallback * obj -> IAsyncResult)
                   (doEnd: IAsyncResult -> 'x) =
    {new Job<'x> () with
      override self.DoJob (wr, xK) =
       let rv = AsyncBeginEnd<'x> (wr.Scheduler, doEnd, xK)
       doBegin (doAsyncCallback, rv) |> ignore}

/////////////////////////////////////////////////////////////////////////

module Timer =
  module Global =
    type [<AllowNullLiteral>] WorkTimedUnitCont =
      inherit WorkTimed
      val uK: Cont<unit>
      override wt.DoHandle (wr, e) = Handler.DoHandle (wt.uK, &wr, e)
      override wt.DoWork (wr) = wt.uK.DoWork (&wr)
      new (t, me, pk, uK) = {inherit WorkTimed (t, me, pk); uK=uK}

    let timeOut (span: System.TimeSpan) =
      let ms = span.Ticks / 10000L
      if ms < 0L || 2147483647L < ms then
        failwith "TimeSpan out of allowed range"
      let ms = int ms
      {new Alt<unit> () with
        override uA'.DoJob (wr, uK) =
         (initGlobalTimer ()).SynchronizedPushTimed
          (WorkTimedUnitCont (Environment.TickCount + ms, 0, null, uK))
        override uA'.TryAlt (wr, i, pk, uK, uE) =
         (initGlobalTimer ()).SynchronizedPushTimed
          (WorkTimedUnitCont (Environment.TickCount + ms, i, pk, uK))
         uE.TryElse (&wr, i+1, pk, uK)}

    let sleep (span: TimeSpan) =
      timeOut span :> Job<unit>

/////////////////////////////////////////////////////////////////////////

module Lock =
  module Now =
    let inline create () = Lock ()
  let create () = ctor Now.create ()
  let inline duringFun (l: Lock) (xF: unit -> 'x) = LockDuringFun<'x> (l, xF) :> Job<'x>
  let inline duringJob (l: Lock) (xJ: Job<'x>) = LockDuringJob<'x> (l, xJ) :> Job<'x>

/////////////////////////////////////////////////////////////////////////

module MVar =
  open Job.Infixes
  module Now =
    let inline create () = MVar<'x> ()
    let inline createFull (x: 'x) = MVar<'x> (x)
  let create () = ctor Now.create ()
  let createFull x = ctor Now.createFull x
  let inline fill (xM: MVar<'x>) (x: 'x) = MVarFill<'x> (xM, x) :> Job<unit>
  let inline take (xM: MVar<'x>) = xM :> Job<'x>
  let inline modifyFun (x2xy: 'x -> 'x * 'y) (xM: MVar<'x>) =
    take xM >>= (x2xy >> fun (x, y) -> fill xM x >>% y)
  let inline modifyJob (x2xyJ: 'x -> Job<'x * 'y>) (xM: MVar<'x>) =
    take xM >>= x2xyJ >>= fun (x, y) -> fill xM x >>% y
  module Alt =
    let inline take (xM: MVar<'x>) = xM :> Alt<'x>

/////////////////////////////////////////////////////////////////////////

module Extensions =
  open Job

  module Array =
    let mapJob (x2yJ: 'x -> Job<'y>) (xs: array<'x>) =
      {new Job<array<'y>> () with
        override ysJ'.DoJob (wr, ysK) =
         if 0 < xs.Length then
           ysK.Value <- Array.zeroCreate xs.Length
           (x2yJ xs.[0]).DoJob (&wr, {new Cont_State<_, _> (0) with
            override yK'.DoHandle (wr, e) = Handler.DoHandle (ysK, &wr, e)
            override yK'.DoWork (wr) =
             let j = yK'.State
             ysK.Value.[j] <- yK'.Value
             let j = j + 1
             if j < xs.Length then
               yK'.State <- j
               (x2yJ xs.[j]).DoJob (&wr, yK')
             else
               ysK.DoWork (&wr)
            override yK'.DoCont (wr, y) =
             let j = yK'.State
             ysK.Value.[j] <- y
             let j = j + 1
             if j < xs.Length then
               yK'.State <- j
               (x2yJ xs.[j]).DoJob (&wr, yK')
             else
               ysK.DoWork (&wr)})
         else
           Cont.Do (ysK, &wr, [||])}

    let iterJob (x2yJ: 'x -> Job<'y>) (xs: array<'x>) =
      {new Job<unit> () with
        override uJ'.DoJob (wr, uK) =
         Work.Do ({new Cont_State<_,_> (0) with
          override yK'.DoHandle (wr, e) = Handler.DoHandle (uK, &wr, e)
          override yK'.DoCont (wr, _) =
           let i = yK'.State
           if i < xs.Length then
             let x = xs.[i]
             yK'.State <- i + 1
             (x2yJ x).DoJob (&wr, yK')
           else
             uK.DoWork (&wr)
          override yK'.DoWork (wr) =
           let i = yK'.State
           if i < xs.Length then
             let x = xs.[i]
             yK'.State <- i + 1
             (x2yJ x).DoJob (&wr, yK')
           else
             uK.DoWork (&wr)}, &wr)}

  module Seq =
    let iterJob (x2yJ: 'x -> Job<'y>) (xs: seq<'x>) =
      {new Job<unit> () with
        override uJ'.DoJob (wr, uK) =
         let xs = xs.GetEnumerator ()
         Work.Do ({new Cont<'y> () with
          override yK'.DoHandle (wr, e) = Handler.DoHandle (uK, &wr, e)
          override yK'.DoCont (wr, _) =
           if xs.MoveNext () then
             (x2yJ xs.Current).DoJob (&wr, yK')
           else
             uK.DoWork (&wr)
          override yK'.DoWork (wr) =
           if xs.MoveNext () then
             (x2yJ xs.Current).DoJob (&wr, yK')
           else
             uK.DoWork (&wr)}, &wr)}

    let mapJob (x2yJ: 'x -> Job<'y>) (xs: seq<'x>) =
      {new Job<ResizeArray<'y>> () with
        override ysJ'.DoJob (wr, ysK) =
         ysK.Value <- ResizeArray<_> ()
         let xs = xs.GetEnumerator ()
         if xs.MoveNext () then
           (x2yJ xs.Current).DoJob (&wr, {new Cont<_> () with
            override yK'.DoHandle (wr, e) = Handler.DoHandle (ysK, &wr, e)
            override yK'.DoWork (wr) =
             ysK.Value.Add yK'.Value
             if xs.MoveNext () then
               (x2yJ xs.Current).DoJob (&wr, yK')
             else
               ysK.DoWork (&wr)
            override yK'.DoCont (wr, y) =
             ysK.Value.Add y
             if xs.MoveNext () then
               (x2yJ xs.Current).DoJob (&wr, yK')
             else
               ysK.DoWork (&wr)})
         else
           Work.Do (ysK, &wr)}

    let foldJob (xy2xJ: 'x -> 'y -> Job<'x>) (x: 'x) (ys: seq<'y>) =
      let xy2xJ = OptimizedClosures.FSharpFunc<_, _, _>.Adapt xy2xJ
      {new Job<'x> () with
        override xJ'.DoJob (wr, xK) =
         let ys = ys.GetEnumerator ()
         if ys.MoveNext () then
           (xy2xJ.Invoke (x, ys.Current)).DoJob (&wr, {new Cont<'x> () with
            override xK'.DoHandle (wr, e) = Handler.DoHandle (xK, &wr, e)
            override xK'.DoWork (wr) =
             if ys.MoveNext () then
               (xy2xJ.Invoke (xK'.Value, ys.Current)).DoJob (&wr, xK')
             else
               xK.DoCont (&wr, xK'.Value)
            override xK'.DoCont (wr, x) =
             if ys.MoveNext () then
               (xy2xJ.Invoke (x, ys.Current)).DoJob (&wr, xK')
             else
               xK.DoCont (&wr, x)})
         else
           Cont.Do (xK, &wr, x)}

    module Con =
      let iterJob (x2yJ: 'x -> Job<'y>) (xs: seq<'x>) =
        {new Job<unit> () with
          override uJ'.DoJob (wr, uK) =
           let join = ConIgnore_State (uK, x2yJ)
           wr.Handler <- join
           let xs = xs.GetEnumerator ()
           while xs.MoveNext () do
             let x = xs.Current
             ConIgnore.Inc join
             Worker.PushNew (&wr, {new Cont_State<_, _> () with
              override yK'.DoHandle (wr, e) = ConIgnore.OutsideDoHandle (join, &wr, e)
              override yK'.DoCont (wr, _) = ConIgnore.Dec (join, &wr)
              override yK'.DoWork (wr) =
               if yK'.State then
                 ConIgnore.Dec (join, &wr)
               else
                 yK'.State <- true
                 (join.State x).DoJob (&wr, yK')})
           ConIgnore.Done (join, &wr)}

      let mapJob (x2yJ: 'x -> Job<'y>) (xs: seq<'x>) =
        {new Job<ResizeArray<'y>> () with
          override ysJ'.DoJob (wr, ysK_) =
           ysK_.Value <- ResizeArray<_> ()
           let xs = xs.GetEnumerator ()
           let cc' = {new ConCollect<'y> (ysK_) with
             override cc'.DoWork (wr) =
              let mutable nth = 0
              while xs.MoveNext () do
                cc'.Lock.ExitAndEnter (&wr, cc')
                cc'.ysK.Value.Add Unchecked.defaultof<_>
                Worker.PushNew (&wr, {new Cont_State<_, _, _, _> (xs.Current, Util.dec &nth, x2yJ) with
                 override yK'.DoHandle (wr, e) = cc'.OutsideDoHandle (&wr, e)
                 override yK'.DoCont (wr, y) =
                  yK'.Value <- y
                  yK'.State2 <- ~~~ yK'.State2
                  cc'.Lock.Enter (&wr, yK')
                 override yK'.DoWork (wr) =
                  let x2yJ = yK'.State3
                  if LanguagePrimitives.PhysicalEquality x2yJ Unchecked.defaultof<_> then
                    let i = yK'.State2
                    if i < 0 then
                      yK'.State2 <- ~~~ i
                      cc'.Lock.Enter (&wr, yK')
                    else
                      cc'.ysK.Value.[i] <- yK'.Value
                      cc'.Inc (&wr)
                  else
                    let x = yK'.State1
                    yK'.State3 <- Unchecked.defaultof<_>
                    yK'.State1 <- Unchecked.defaultof<_>
                    (x2yJ x).DoJob (&wr, yK')})
              cc'.Inc (&wr)}
           wr.Handler <- cc'
           cc'.Lock.Enter (&wr, cc')}

  ///////////////////////////////////////////////////////////////////////
  
  type [<Sealed>] Task =
    static member inline awaitJob (xTask: System.Threading.Tasks.Task<'x>) =
      AwaitTaskWithResult<'x> (xTask) :> Job<'x>

    static member inline awaitJob (task: System.Threading.Tasks.Task) =
      AwaitTask (task) :> Job<unit>

/////////////////////////////////////////////////////////////////////////

open Extensions
open Job.Infixes

type JobBuilder () =
  member inline job.Bind (xJ: Job<'x>, x2yJ: 'x -> Job<'y>) : Job<'y> =
    xJ >>= x2yJ
  member inline job.Combine (uJ: Job<unit>, xJ: Job<'x>) : Job<'x> = uJ >>. xJ
  member inline job.Delay (u2xJ: unit -> Job<'x>) : Job<'x> = Job.delay u2xJ
  member inline job.For (xs: seq<'x>, x2uJ: 'x -> Job<unit>) : Job<unit> =
    Seq.iterJob x2uJ xs
  member inline job.For (xs: array<'x>, x2uJ: 'x -> Job<unit>) : Job<unit> =
    Array.iterJob x2uJ xs
  member inline job.Return (x: 'x) : Job<'x> = Job.result x
  member inline job.ReturnFrom (xJ: Job<'x>) : Job<'x> = xJ
  member inline job.TryFinally (xJ: Job<'x>, u2u: unit -> unit) : Job<'x> =
    Job.tryFinallyFun xJ u2u
  member inline job.TryWith (xJ: Job<'x>, e2xJ: exn -> Job<'x>) : Job<'x> =
    Job.tryWith xJ e2xJ
  member inline job.Using (x: 'x, x2yJ: 'x -> Job<'y>) : Job<'y>
      when 'x :> IDisposable =
    Job.using x x2yJ
  member inline job.While (u2b: unit -> bool, uJ: Job<unit>) : Job<unit> =
    Job.whileDo u2b uJ
  member inline job.Zero () : Job<unit> = Job.unit

[<AutoOpen>]
module TopLevel =
  let job = JobBuilder ()
  let inline run x = Job.Global.run x
