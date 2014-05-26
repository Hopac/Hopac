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

  let forward (e: exn) : 'x =
    raise (exn ("forwarded", e))

  let aggrExn (exns: ResizeArray<exn>) =
    if exns.Count = 1 then exns.[0] else upcast AggregateException exns

  module Option =
    let orDefaultOf x =
      match x with
       | None -> Unchecked.defaultof<_>
       | Some x -> x

  let inline ctor x2y x =
    {new Job<'y> () with
      override yJ'.DoJob (wr, yK) =
       yK.DoCont (&wr, x2y x)}

  type TryInCont<'x, 'y> =
    inherit Cont<'x>
    val x2yJ: 'x -> Job<'y>
    val e2yJ: exn -> Job<'y>
    val mutable yK: Cont<'y>
    new (x2yJ, e2yJ, yK) = {inherit Cont<'x> (); x2yJ=x2yJ; e2yJ=e2yJ; yK=yK}
    override xK'.GetProc (wr) = Handler.GetProc (&wr, &xK'.yK)
    override xK'.DoHandle (wr, e) =
     let yK = xK'.yK
     wr.Handler <- yK
     (xK'.e2yJ e).DoJob (&wr, yK)
    override xK'.DoWork (wr) =
     let yK = xK'.yK
     wr.Handler <- yK
     (xK'.x2yJ xK'.Value).DoJob (&wr, yK)
    override xK'.DoCont (wr, x) =
     let yK = xK'.yK
     wr.Handler <- yK
     (xK'.x2yJ x).DoJob (&wr, yK)

  type BindCont<'x, 'y> =
    inherit Cont<'x>
    val x2yJ: 'x -> Job<'y>
    val mutable yK: Cont<'y>
    new (x2yJ, yK) = {inherit Cont<'x> (); x2yJ=x2yJ; yK=yK}
    override xK'.GetProc (wr) = Handler.GetProc (&wr, &xK'.yK)
    override xK'.DoHandle (wr, e) = Handler.DoHandle (xK'.yK, &wr, e)
    override xK'.DoWork (wr) = (xK'.x2yJ xK'.Value).DoJob (&wr, xK'.yK)
    override xK'.DoCont (wr, x) = (xK'.x2yJ x).DoJob (&wr, xK'.yK)

  type MapCont<'x, 'y> =
    inherit Cont<'x>
    val x2y: 'x -> 'y
    val mutable yK: Cont<'y>
    new (x2y, yK) = {inherit Cont<'x> (); x2y=x2y; yK=yK}
    override xK'.GetProc (wr) = Handler.GetProc (&wr, &xK'.yK)
    override xK'.DoHandle (wr, e) = Handler.DoHandle (xK'.yK, &wr, e)
    override xK'.DoWork (wr) = xK'.yK.DoCont (&wr, xK'.x2y xK'.Value)
    override xK'.DoCont (wr, x) = xK'.yK.DoCont (&wr, xK'.x2y x)

  type DropCont<'x, 'y> =
    inherit Cont<'y>
    val mutable xK: Cont<'x>
    new (xK) = {inherit Cont<'y> (); xK=xK}
    override yK'.GetProc (wr) = Handler.GetProc (&wr, &yK'.xK)
    override yK'.DoHandle (wr, e) = Handler.DoHandle (yK'.xK, &wr, e)
    override yK'.DoWork (wr) = yK'.xK.DoWork (&wr)
    override yK'.DoCont (wr, _) = yK'.xK.DoWork (&wr)

  type ValueCont<'x, 'y> (y: 'y, yK: Cont<'y>) =
    inherit Cont<'x> ()
    override xK'.GetProc (wr) = yK.GetProc (&wr)
    override xK'.DoHandle (wr, e) = yK.DoHandle (&wr, e)
    override xK'.DoWork (wr) = yK.DoCont (&wr, y)
    override xK'.DoCont (wr, _) = yK.DoCont (&wr, y)

  type SeqCont<'x, 'y> =
    inherit Cont<'x>
    val yJ: Job<'y>
    val mutable yK: Cont<'y>
    new (yJ, yK) = {inherit Cont<'x> (); yJ=yJ; yK=yK}
    override xK'.GetProc (wr) = Handler.GetProc (&wr, &xK'.yK)
    override xK'.DoHandle (wr, e) = Handler.DoHandle (xK'.yK, &wr, e)
    override xK'.DoWork (wr) = xK'.yJ.DoJob (&wr, xK'.yK)
    override xK'.DoCont (wr, _) = xK'.yJ.DoJob (&wr, xK'.yK)

  type SkipCont<'x, 'y> =
    inherit Cont<'x>
    val mutable xK: Cont<'x>
    val yJ: Job<'y>
    new (xK, yJ) = {inherit Cont<'x> (); xK=xK; yJ=yJ}
    override xK'.GetProc (wr) = Handler.GetProc (&wr, &xK'.xK)
    override xK'.DoHandle (wr, e) = Handler.DoHandle (xK'.xK, &wr, e)
    override xK'.DoWork (wr) =
     let xK = xK'.xK
     xK.Value <- xK'.Value
     xK'.yJ.DoJob (&wr, DropCont xK)
    override xK'.DoCont (wr, x) =
     let xK = xK'.xK
     xK.Value <- x
     xK'.yJ.DoJob (&wr, DropCont xK)

  /// Only valid to be used when spawning a new thread.
  type [<AbstractClass>] WorkHandler () =
    inherit Work ()
    override w'.GetProc (wr) = raise <| NotImplementedException ()
    override w'.DoHandle (wr, e) = Handler.DoHandle (null, &wr, e)

  type Handler<'x, 'y> =
    inherit Cont<'x>
    val mutable yK: Cont<'y>
    new () = {inherit Cont<'x> (); yK=null}
    new (yK) = {inherit Cont<'x> (); yK=yK}
    override xK'.GetProc (wr) = Handler.GetProc (&wr, &xK'.yK)
    override xK'.DoHandle (wr, e) = Handler.DoHandle (xK'.yK, &wr, e)
    override xK'.DoWork (wr) = Handler.Terminate (&wr, xK'.yK)
    override xK'.DoCont (wr, _) = Handler.Terminate (&wr, xK'.yK)

/////////////////////////////////////////////////////////////////////////

module IVar =
  module Now =
    let inline create () = IVar<'x> ()
    let inline createFull (x: 'x) = IVar<'x> (x)
    let inline createFailure (e: exn) = IVar<'x> (e)
    let inline isFull (xI: IVar<'x>) = xI.Full
    [<MethodImpl(MethodImplOptions.NoInlining)>]
    let get (xI: IVar<'x>) : 'x = xI.Get ()
      
  let create () = ctor Now.create ()
  let inline fill (xI: IVar<'x>) (x: 'x) = IVarFill<'x> (xI, x) :> Job<unit>
  let inline tryFill (xI: IVar<'x>) (x: 'x) = IVarTryFill<'x> (xI, x) :> Job<unit>
  let inline fillFailure (xI: IVar<'x>) (e: exn) =
    IVarFillFailure<'x> (xI, e) :> Job<unit>
  let inline read (xI: IVar<'x>) = xI :> Job<'x>
  module Alt =
    let inline read (xI: IVar<'x>) = xI :> Alt<'x>

/////////////////////////////////////////////////////////////////////////

module Promise =
  let start (xJ: Job<'x>) =
    {new Job<Promise<'x>> () with
      override self.DoJob (wr, xPrK) =
       let pr = Promise<'x> ()
       xPrK.Value <- pr
       Worker.Push (&wr, xPrK)
       Job.Do (xJ, &wr, Promise<'x>.PrCont (pr))}
  let queue (xJ: Job<'x>) =
    {new Job<Promise<'x>> () with
      override self.DoJob (wr, xPrK) =
       let pr = Promise<'x> ()
       Worker.PushNew (&wr, {new WorkHandler () with
        override w'.DoWork (wr) =
         let prc = Promise<'x>.PrCont (pr)
         wr.Handler <- prc
         xJ.DoJob (&wr, prc)})
       Cont.Do (xPrK, &wr, pr)}
  let startAsAlt (xJ: Job<'x>) =
    {new Job<Alt<'x>> () with
      override self.DoJob (wr, xPrK) =
       let pr = Promise<'x> ()
       xPrK.Value <- pr
       Worker.Push (&wr, xPrK)
       Job.Do (xJ, &wr, Promise<'x>.PrCont (pr))}
  let queueAsAlt (xJ: Job<'x>) =
    {new Job<Alt<'x>> () with
      override self.DoJob (wr, xPrK) =
       let pr = Promise<'x> ()
       Worker.PushNew (&wr, {new WorkHandler () with
        override w'.DoWork (wr) =
         let prc = Promise<'x>.PrCont (pr)
         wr.Handler <- prc
         xJ.DoJob (&wr, prc)})
       Cont.Do (xPrK, &wr, upcast pr)}
  module Now =
    let inline withValue (x: 'x) = Promise<'x> (x)
    let inline withFailure (e: exn) = Promise<'x> (e)
  let inline read (xPr: Promise<'x>) = xPr :> Job<'x>
  module Alt =
    let inline read (xPr: Promise<'x>) = xPr :> Alt<'x>
    
/////////////////////////////////////////////////////////////////////////

module Alt =
  let inline always (x: 'x) = Always<'x> (x) :> Alt<'x>

  let inline unit () = StaticData.unit

  let never () =
    {new Alt<'x> () with
      override xA'.DoJob (wr, _) = ()
      override xA'.TryAlt (wr, i, xK, xE) = xE.TryElse (&wr, i)}

  let inline zero () = StaticData.zero

  type GuardJobCont<'x> =
    inherit Cont<Alt<'x>>
    val mutable xK: Cont<'x>
    new (xK) = {inherit Cont<Alt<'x>> (); xK=xK}
    override xAK'.GetProc (wr) = Handler.GetProc (&wr, &xAK'.xK)
    override xAK'.DoHandle (wr, e) = Handler.DoHandle (xAK'.xK, &wr, e)
    override xAK'.DoWork (wr) = xAK'.Value.DoJob (&wr, xAK'.xK)
    override xAK'.DoCont (wr, xA) = xA.DoJob (&wr, xAK'.xK)

  type GuardCont<'x> =
   inherit Cont<Alt<'x>>
   val i: int
   val mutable xK: Cont<'x>
   val xE: Else
   new (i, xK, xE) = {inherit Cont<Alt<'x>> (); i=i; xK=xK; xE=xE}
   override xAK'.GetProc (wr) =
    Handler.GetProc (&wr, &xAK'.xK)
   override xAK'.DoHandle (wr, e) =
    Pick.PickClaimed xAK'.xE.pk
    Handler.DoHandle (xAK'.xK, &wr, e)
   override xAK'.DoWork (wr) =
    let xE = xAK'.xE
    Pick.Unclaim xE.pk
    xAK'.Value.TryAlt (&wr, xAK'.i, xAK'.xK, xE)
   override xAK'.DoCont (wr, xA) =
    let xE = xAK'.xE
    Pick.Unclaim xE.pk
    xA.TryAlt (&wr, xAK'.i, xAK'.xK, xE)

  let guard (xAJ: Job<Alt<'x>>) =
    {new Alt<'x> () with
      override xA'.DoJob (wr, xK) =
       xAJ.DoJob (&wr, GuardJobCont xK)
      override xA'.TryAlt (wr, i, xK, xE) =
       Pick.ClaimAndDoJob (xE.pk, &wr, xAJ, GuardCont (i, xK, xE))}

  let inline delay (u2xA: unit -> Alt<'x>) =
    {new AltDelay<'x> () with
      override xA'.Do () = u2xA ()} :> Alt<_>

  let inline pick (xA: Alt<'x>) = xA :> Job<'x>
     
  type WithNackElse (nk: Nack, xE: Else) =
    inherit Else (xE.pk)
    override xE'.TryElse (wr, i) =
      nk.I1 <- i
      xE.TryElse (&wr, i)

  type WithNackCont<'x> =
    inherit Cont<Alt<'x>>
    val mutable xK: Cont<'x>
    val xE: Else
    new (xK, xE) = {inherit Cont<Alt<'x>> (); xK=xK; xE=xE}
    override xAK'.GetProc (wr) =
     Handler.GetProc (&wr, &xAK'.xK)
    override xAK'.DoHandle (wr, e) =
     Pick.PickClaimed xAK'.xE.pk
     Handler.DoHandle (xAK'.xK, &wr, e)
    override xAK'.DoWork (wr) =
     let xE = xAK'.xE
     let pk = xE.pk
     let nk = pk.Nacks
     Pick.Unclaim pk
     xAK'.Value.TryAlt (&wr, nk.I0, xAK'.xK, WithNackElse (nk, xE))
    override xAK'.DoCont (wr, xA) =
     let xE = xAK'.xE
     let pk = xE.pk
     let nk = pk.Nacks
     Pick.Unclaim pk
     xA.TryAlt (&wr, nk.I0, xAK'.xK, WithNackElse (nk, xE))

  let withNack (nack2xAJ: Alt<unit> -> Job<Alt<'x>>) =
    {new Alt<'x> () with
      override xA'.DoJob (wr, xK) =
       (nack2xAJ StaticData.zero).DoJob (&wr, GuardJobCont xK)
      override xA'.TryAlt (wr, i, xK, xE) =
       match Pick.AddNack (xE.pk, i) with
        | null -> ()
        | nk -> (nack2xAJ nk).DoJob (&wr, WithNackCont (xK, xE))}

  module Infixes =
    let (<|>) (xA1: Alt<'x>) (xA2: Alt<'x>) =
      {new Alt<'x> () with
        override xA'.DoJob (wr, xK) =
         let pk = Pick ()
         xA1.TryAlt (&wr, 0, xK, {new Else_State<_> (pk, xA2) with
          override xE'.TryElse (wr, i) =
           match xE'.State with
            | null -> ()
            | xA2 ->
              xE'.State <- null
              xA2.TryAlt (&wr, i, xK, xE')})
        override xA'.TryAlt (wr, i, xK, xE) =
         xA1.TryAlt (&wr, i, xK, {new Else (xE.pk) with
          override xE'.TryElse (wr, i) =
           xA2.TryAlt (&wr, i, xK, xE)})}
 
    let inline (|>>?) (xA: Alt<'x>) (x2y: 'x -> 'y) =
      {new AltMap<'x, 'y> (xA) with
        override yA'.Do (x) = x2y x} :> Alt<_>

    let inline (>>=?) (xA: Alt<'x>) (x2yJ: 'x -> Job<'y>) =
      {new AltBind<'x, 'y> (xA) with
        override yA'.Do (x) = x2yJ x} :> Alt<_>

    let (>>%?) (xA: Alt<'x>) (y: 'y) =
      {new Alt<'y> () with
        override yA'.DoJob (wr, yK) =
         yK.Value <- y
         xA.DoJob (&wr, DropCont yK)
        override yA'.TryAlt (wr, i, yK, yE) =
         xA.TryAlt (&wr, i, ValueCont (y, yK), yE)}

    let (>>!?) (xA: Alt<'x>) (e: exn) =
      {new Alt<'y> () with
        override yA'.DoJob (wr, yK) =
         xA.DoJob (&wr, FailCont (yK, e))
        override yA'.TryAlt (wr, i, yK, yE) =
         xA.TryAlt (&wr, i, FailCont (yK, e), yE)}

    let (>>.?) (xA: Alt<'x>) (yJ: Job<'y>) =
      {new Alt<'y> () with
        override yA'.DoJob (wr, yK) =
         xA.DoJob (&wr, SeqCont (yJ, yK))
        override yA'.TryAlt (wr, i, yK, yE) =
         xA.TryAlt (&wr, i, SeqCont (yJ, yK), yE)}

    let (.>>?) (xA: Alt<'x>) (yJ: Job<'y>) =
      {new Alt<'x> () with
        override xA'.DoJob (wr, xK) =
         xA.DoJob (&wr, SkipCont (xK, yJ))
        override xA'.TryAlt (wr, i, xK, xE) =
         xA.TryAlt (&wr, i, SkipCont (xK, yJ), xE)}

  let choose (xAs: seq<Alt<'x>>) =
    {new Alt<'x> () with
      override xA'.DoJob (wr, xK) =
       let xAs = xAs.GetEnumerator ()
       if xAs.MoveNext () then
         let pk = Pick ()
         xAs.Current.TryAlt (&wr, 0, xK, {new Else (pk) with
          override xE'.TryElse (wr, i) =
           if xAs.MoveNext () then
             xAs.Current.TryAlt (&wr, i, xK, xE')})
      override xA'.TryAlt (wr, i, xK, xE) =
       let xAs = xAs.GetEnumerator ()
       if xAs.MoveNext () then
         xAs.Current.TryAlt (&wr, i, xK, {new Else (xE.pk) with
          override xE'.TryElse (wr, i) =
           if xAs.MoveNext () then
             xAs.Current.TryAlt (&wr, i, xK, xE')
           else
             xE.TryElse (&wr, i)})
       else
         xE.TryElse (&wr, i)}

  let inline select xAs = choose xAs :> Job<'x>

  let tryIn (xA: Alt<'x>) (x2yJ: 'x -> Job<'y>) (e2yJ: exn -> Job<'y>) =
    {new Alt<'y> () with
      override yJ'.DoJob (wr, yK) =
       let xK = TryInCont (x2yJ, e2yJ, yK)
       wr.Handler <- xK
       xA.DoJob (&wr, xK)
      override yJ'.TryAlt (wr, i, yK, yE) =
       let xK = TryInCont (x2yJ, e2yJ, yK)
       wr.Handler <- xK
       xA.TryAlt (&wr, i, xK, {new Else (yE.pk) with
        override xE'.TryElse (wr, i) =
         wr.Handler <- yK
         yE.TryElse (&wr, i)})}

/////////////////////////////////////////////////////////////////////////

module Scheduler =
  type Create =
    {Foreground: option<bool>
     IdleHandler: option<Job<int>>
     MaxStackSize: option<int>
     NumWorkers: option<int>
     Priority: option<ThreadPriority>
     TopLevelHandler: option<exn -> Job<unit>>}
    static member Def: Create =
      StaticData.Init ()
      {Foreground = None
       IdleHandler = None
       MaxStackSize = None
       NumWorkers = None
       Priority = None
       TopLevelHandler = None}

  module Global =
    let mutable create = Create.Def

    let setCreate (c: Create) =
      create <- c

  let create (c: Create) =
    Scheduler (Option.orDefaultOf c.Foreground,
               Option.orDefaultOf c.IdleHandler,
               Option.orDefaultOf c.MaxStackSize,
               (match c.NumWorkers with
                 | None -> Environment.ProcessorCount
                 | Some n ->
                   if n < 1 then
                     failwith "Invalid number of workers specified: %d" n
                   n),
               (match c.Priority with
                 | None -> ThreadPriority.Normal
                 | Some p -> p),
               Option.orDefaultOf c.TopLevelHandler)

  let startWithActions (sr: Scheduler)
                       (eF: exn -> unit)
                       (xF: 'x -> unit)
                       (xJ: Job<'x>) =
    Worker.RunOnThisThread (sr, xJ, {new Cont_State<'x, Cont<unit>> () with
     override xK'.GetProc (wr) = Handler.GetProc (&wr, &xK'.State)
     override xK'.DoHandle (wr, e) =
      Handler.Terminate (&wr, xK'.State)
      eF e
     override xK'.DoWork (wr) =
      Handler.Terminate (&wr, xK'.State)
      xF xK'.Value
     override xK'.DoCont (wr, x) =
      Handler.Terminate (&wr, xK'.State)
      xF x})

  let start (sr: Scheduler) (xJ: Job<'x>) =
    Worker.RunOnThisThread (sr, xJ, Handler<'x, unit> ())

  let server (sr: Scheduler) (vJ: Job<Void>) =
    Worker.RunOnThisThread (sr, vJ, null)

  let run (sr: Scheduler) (xJ: Job<'x>) =
    let xK' = {new Cont_State<_, _, _, Cont<unit>> () with
     override xK'.GetProc (wr) =
      Handler.GetProc (&wr, &xK'.State3)
     override xK'.DoHandle (wr, e) =
      Handler.Terminate (&wr, xK'.State3)
      xK'.State1 <- e
      Condition.Pulse (xK', &xK'.State2)
     override xK'.DoWork (wr) =
      Handler.Terminate (&wr, xK'.State3)
      Condition.Pulse (xK', &xK'.State2)
     override xK'.DoCont (wr, x) =
      Handler.Terminate (&wr, xK'.State3)
      xK'.Value <- x
      Condition.Pulse (xK', &xK'.State2)}
    Worker.RunOnThisThread (sr, xJ, xK')
    Condition.Wait (xK', &xK'.State2)
    match xK'.State1 with
     | null -> xK'.Value
     | e -> Util.forward e

  let wait (sr: Scheduler) =
    if sr.NumActive > 0 then
      Interlocked.Increment &sr.NumPulseWaiters |> ignore
      Monitor.Enter sr
      while sr.NumActive > 0 do
        Monitor.Wait sr |> ignore
      Monitor.Exit sr
      Interlocked.Decrement &sr.NumPulseWaiters |> ignore

  let kill (sr: Scheduler) =
    Scheduler.Kill sr

/////////////////////////////////////////////////////////////////////////

[<AutoOpen>]
module Global =

  let mutable globalScheduler : Scheduler = null

  let reallyInitGlobalScheduler () =
    let t = typeof<Scheduler>
    Monitor.Enter t
    match globalScheduler with
     | null ->
       if not System.Runtime.GCSettings.IsServerGC then
         printf "WARNING: You are using single-threaded workstation garbage \
          collection, which means that parallel programs cannot scale.  Please \
          configure your program to use server garbage collection.  See \
          http://msdn.microsoft.com/en-us/library/ms229357%%28v=vs.110%%29.aspx \
          for details.\n"
       let sr = Scheduler.create Scheduler.Global.create
       globalScheduler <- sr
       Monitor.Exit t
       sr
     | sr ->
       Monitor.Exit t
       sr

  let inline initGlobalScheduler () =
    match globalScheduler with
     | null -> reallyInitGlobalScheduler ()
     | sr -> sr

  let mutable globalTimer : Timer = null

  let reallyInitGlobalTimer () =
    let sr = initGlobalScheduler ()
    let t = typeof<Timer>
    Monitor.Enter t
    match globalTimer with
     | null ->
       let tr = Timer sr
       globalTimer <- tr
       Monitor.Exit t
       tr
     | tr ->
       Monitor.Exit t
       tr

  let inline initGlobalTimer () =
    match globalTimer with
     | null -> reallyInitGlobalTimer ()
     | tr -> tr

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

module Job =
  module Global =
    let startWithActions eF xF xJ =
      Scheduler.startWithActions (initGlobalScheduler ()) eF xF xJ
    let start xJ = Scheduler.start (initGlobalScheduler ()) xJ
    let server vJ = Scheduler.server (initGlobalScheduler ()) vJ
    let run xJ = Scheduler.run (initGlobalScheduler ()) xJ

  ///////////////////////////////////////////////////////////////////////

  let inline delay (u2xJ: unit -> Job<'x>) =
    {new JobDelay<'x> () with
      override xJ'.Do () =
       u2xJ ()} :> Job<_>

  let inline delayWith (x2yJ: 'x -> Job<'y>) (x: 'x) =
    {new JobDelay<'y> () with
      override yJ'.Do () =
       x2yJ x} :> Job<_>

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
         override xK'.GetProc (wr) = uK.GetProc (&wr)
         override xK'.DoHandle (wr, e) = uK.DoHandle (&wr, e)
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
           override xK'.GetProc (wr) = uK.GetProc (&wr)
           override xK'.DoHandle (wr, e) = uK.DoHandle (&wr, e)
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

  type ForeverCont<'x, 'y> =
    inherit Handler<'x, 'y>
    val xJ: Job<'x>
    new (xJ, yK) = {inherit Handler<'x, 'y> (yK); xJ=xJ}
    override xK'.DoWork (wr) = xK'.xJ.DoJob (&wr, xK')
    override xK'.DoCont (wr, _) = xK'.xJ.DoJob (&wr, xK')

  let forever (xJ: Job<'x>) =
    {new Job<'y> () with
      override yJ'.DoJob (wr, yK_) = {new ForeverCont<'x, 'y> (xJ, yK_) with
       override xK'.DoHandle (wr, e) =
        Handler.DoHandle (xK'.yK, &wr, e)}.DoWork (&wr)}

  let inline iterate (x: 'x) (x2xJ: 'x -> Job<'x>) =
    {new JobRun<'y> () with
      override yJ'.Do (yK) =
        {new ContIterate<'x, 'y> (x, yK) with
          override xK'.Do (x) = x2xJ x} :> Work} :> Job<_>

  let whileDo (cond: unit -> bool) (xJ: Job<'x>) =
    {new Job<unit> () with
      override uJ'.DoJob (wr, uK) = {new Cont<'x> () with
       override xK'.GetProc (wr) = uK.GetProc (&wr)
       override xK'.DoHandle (wr, e) = uK.DoHandle (&wr, e)
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

  let inline unit () = StaticData.unit :> Job<_>

  let abort () =
    {new Job<_> () with
      override xJ'.DoJob (_, _) = ()}

  let raises (e: exn) =
    {new Job<_> () with
      override xJ.DoJob (wr, xK) = Handler.DoHandle (xK, &wr, e)}

  let inline whenDo (b: bool) (uJ: Job<unit>) =
    if b then uJ else StaticData.unit :> Job<_>

  ///////////////////////////////////////////////////////////////////////

  module Infixes =
    let inline (>>=) (xJ: Job<'x>) (x2yJ: 'x -> Job<'y>) =
      {new JobBind<'x, 'y> (xJ) with
        override yJ'.Do (x) = x2yJ x} :> Job<_>

    let (>>.) (xJ: Job<'x>) (yJ: Job<'y>) =
      {new Job<'y> () with
        override yJ'.DoJob (wr, yK) =
         xJ.DoJob (&wr, SeqCont (yJ, yK))}

    let (.>>) (xJ: Job<'x>) (yJ: Job<'y>) =
      {new Job<'x> () with
        override xJ'.DoJob (wr, xK) =
         xJ.DoJob (&wr, SkipCont (xK, yJ))}

    let inline (|>>) (xJ: Job<'x>) (x2y: 'x -> 'y) =
      {new JobMap<'x, 'y> (xJ) with
        override yJ'.Do (x) = x2y x} :> Job<_>

    let (>>%) (xJ: Job<'x>) (y: 'y) =
      {new Job<'y> () with
        override yJ'.DoJob (wr, yK) =
         yK.Value <- y
         xJ.DoJob (&wr, DropCont yK)}

    let (>>!) (xJ: Job<'x>) (e: exn) =
      {new Job<'y> () with
        override yJ'.DoJob (wr, yK_) =
         xJ.DoJob (&wr, {new Cont_State<'x, _> (yK_) with
          override xK'.GetProc (wr) = Handler.GetProc (&wr, &xK'.State)
          override xK'.DoHandle (wr, e) = Handler.DoHandle (xK'.State, &wr, e)
          override xK'.DoWork (wr) = Handler.DoHandle (xK'.State, &wr, e)
          override xK'.DoCont (wr, _) = Handler.DoHandle (xK'.State, &wr, e)})}

    type PairCont2<'x, 'y> (x: 'x, xyK: Cont<'x * 'y>) =
      inherit Cont<'y> ()
      override yK'.GetProc (wr) = xyK.GetProc (&wr)
      override yK'.DoHandle (wr, e) = xyK.DoHandle (&wr, e)
      override yK'.DoWork (wr) = xyK.DoCont (&wr, (x, yK'.Value))
      override yK'.DoCont (wr, y) = xyK.DoCont (&wr, (x, y))

    type PairCont<'x, 'y> (yJ: Job<'y>, xyK: Cont<'x * 'y>) =
      inherit Cont<'x> ()
      override xK'.GetProc (wr) = xyK.GetProc (&wr)
      override xK'.DoHandle (wr, e) = xyK.DoHandle (&wr, e)
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
              override xK'.GetProc (wr) = yK'.GetProc (&wr)
              override xK'.DoHandle (wr, e) = yK'.DoHandle (&wr, e)
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

  let inline tryIn (xJ: Job<'x>) (x2yJ: 'x -> Job<'y>) (e2yJ: exn -> Job<'y>) =
    {new JobTryIn<'x, 'y> (xJ) with
      override yJ'.DoIn (x) = x2yJ x
      override yJ'.DoExn (e) = e2yJ e} :> Job<_>

  let inline tryWith (xJ: Job<'x>) (e2xJ: exn -> Job<'x>) =
    {new JobTryWith<'x> (xJ) with
      override xJ'.DoExn (e) = e2xJ e} :> Job<_>

  let tryFinallyFun (xJ: Job<'x>) (u2u: unit -> unit) =
    {new Job<'x> () with
      override xJ'.DoJob (wr, xK_) =
       let xK' = {new Cont_State<'x, _> (xK_) with
        override xK'.GetProc (wr) = Handler.GetProc (&wr, &xK'.State)
        override xK'.DoHandle (wr, e) =
         let xK = xK'.State
         wr.Handler <- xK
         u2u ()
         Handler.DoHandle (xK, &wr, e)
        override xK'.DoWork (wr) =
         let xK = xK'.State
         wr.Handler <- xK
         u2u ()
         xK.DoCont (&wr, xK'.Value)
        override xK'.DoCont (wr, x) =
         let xK = xK'.State
         wr.Handler <- xK
         u2u ()
         xK.DoCont (&wr, x)}
       wr.Handler <- xK'
       xJ.DoJob (&wr, xK')}

  let tryFinallyJob (xJ: Job<'x>) (uJ: Job<unit>) =
    {new Job<'x> () with
      override xJ'.DoJob (wr, xK_) =
       let xK' = {new Cont_State<'x, _> (xK_) with
        override xK'.GetProc (wr) = Handler.GetProc (&wr, &xK'.State)
        override xK'.DoHandle (wr, e) =
         let xK = xK'.State
         wr.Handler <- xK
         uJ.DoJob (&wr, FailCont<unit> (xK, e))
        override xK'.DoWork (wr) =
         let xK = xK'.State
         wr.Handler <- xK
         xK.Value <- xK'.Value
         uJ.DoJob (&wr, DropCont<'x, unit> (xK))
        override xK'.DoCont (wr, x) =
         let xK = xK'.State
         wr.Handler <- xK
         xK.Value <- x
         uJ.DoJob (&wr, DropCont<'x, unit> (xK))}
       wr.Handler <- xK'
       xJ.DoJob (&wr, xK')}

  let using (x: 'x when 'x :> IDisposable) (x2yJ: 'x -> Job<'y>) =
    {new Job<'y> () with
      override yJ'.DoJob (wr, yK_) =
       let yK' = {new Cont_State<'y, _> (yK_) with
        override yK'.GetProc (wr) = Handler.GetProc (&wr, &yK'.State)
        override yK'.DoHandle (wr, e) =
         let yK = yK'.State
         wr.Handler <- yK
         x.Dispose ()
         Handler.DoHandle (yK, &wr, e)
        override yK'.DoWork (wr) =
         let yK = yK'.State
         wr.Handler <- yK
         x.Dispose ()
         yK.DoCont (&wr, yK'.Value)
        override yK'.DoCont (wr, y) =
         let yK = yK'.State
         wr.Handler <- yK
         x.Dispose ()
         yK.DoCont (&wr, y)}
       wr.Handler <- yK'
       (x2yJ x).DoJob (&wr, yK')}

  let catch (xJ: Job<'x>) =
    {new Job<Choice<'x, exn>> () with
      override cJ'.DoJob (wr, cK) =
       let xK' = {new Cont<'x> () with
        override xK'.GetProc (wr) = cK.GetProc (&wr)
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
       Worker.Push (&wr, uK)
       Job.Do (xJ, &wr, Handler<'x, unit> ())}

  let queue (xJ: Job<'x>) =
    {new Job<unit> () with
      override uJ'.DoJob (wr, uK) =
       Worker.PushNew (&wr, {new WorkHandler () with
        override w'.DoWork (wr) =
         xJ.DoJob (&wr, Handler<'x, unit> ())})
       Work.Do (uK, &wr)}

  let server (vJ: Job<Void>) =
    {new Job<unit> () with
      override uJ'.DoJob (wr, uK) =
       Worker.Push (&wr, uK)
       Job.Do (vJ, &wr, null)}

  type Finalizer<'x> (sr: Scheduler, uJ: Job<unit>) =
    inherit Cont<'x> ()
    [<DefaultValue>] val mutable Proc: Proc
    override xK'.Finalize () =
     match xK'.Proc with
      | null -> ()
      | proc -> Worker.RunOnThisThread (sr, xK')
     Scheduler.start sr uJ
    override xK'.GetProc (wr) =
     match xK'.Proc with
      | null ->
        Interlocked.CompareExchange (&xK'.Proc, Proc (), null) |> ignore
        xK'.Proc
      | proc ->
        proc
    member xK'.Term (wr: byref<Worker>) =
     GC.SuppressFinalize xK'
     match xK'.Proc with
      | null -> ()
      | proc -> proc.Terminate (&wr)
    override xK'.DoHandle (wr, e) =
     xK'.Term (&wr)
     Handler.DoHandle (null, &wr, e)
    override xK'.DoWork (wr) = xK'.Term (&wr)
    override xK'.DoCont (wr, _) = xK'.Term (&wr)

  let startWithFinalizer (uJ: Job<unit>) (xJ: Job<'x>) =
    {new Job<unit> () with
      override uJ'.DoJob (wr, uK) =
       Worker.Push (&wr, uK)
       Job.Do (xJ, &wr, Finalizer<'x> (wr.Scheduler, uJ))}

  ///////////////////////////////////////////////////////////////////////

  let foreverServer (xJ: Job<'x>) =
    {new Job<unit> () with
      override uJ'.DoJob (wr, uK) =
       Worker.PushNew (&wr, ForeverCont<'x, unit> (xJ, null))
       Work.Do (uK, &wr)}

  let inline iterateServer (x: 'x) (x2xJ: 'x -> Job<'x>) =
    {new JobStart () with
      override uJ'.Do () =
       {new ContIterate<'x, unit> (x, null) with
         override xK'.Do (x) = x2xJ x} :> Work} :> Job<_>

  ///////////////////////////////////////////////////////////////////////

  let seqCollect (xJs: seq<Job<'x>>) =
    {new Job<ResizeArray<'x>> () with
      override self.DoJob (wr, xsK) =
       let xJs = xJs.GetEnumerator ()
       xsK.Value <- ResizeArray<_> ()
       if xJs.MoveNext () then
         let xK' = {new Cont<'x> () with
          override xK'.GetProc (wr) = xsK.GetProc (&wr)
          override xK'.DoHandle (wr, e) =
           wr.Handler <- xsK ; xJs.Dispose () ; xsK.DoHandle (&wr, e)
          override xK'.DoWork (wr) =
           xsK.Value.Add xK'.Value
           if xJs.MoveNext () then
             xJs.Current.DoJob (&wr, xK')
           else
             wr.Handler <- xsK ; xJs.Dispose () ; xsK.DoWork (&wr)
          override xK'.DoCont (wr, x) =
           xsK.Value.Add x
           if xJs.MoveNext () then
             xJs.Current.DoJob (&wr, xK')
           else
             wr.Handler <- xsK ; xJs.Dispose () ; xsK.DoWork (&wr)}
         wr.Handler <- xK'
         xJs.Current.DoJob (&wr, xK')
       else
         xJs.Dispose () ; Work.Do (xsK, &wr)}

  let seqIgnore (xJs: seq<Job<'x>>) =
    {new Job<unit> () with
      override self.DoJob (wr, uK) =
       let xJs = xJs.GetEnumerator ()
       if xJs.MoveNext () then
         let xK' = {new Cont<'x> () with
          override xK'.GetProc (wr) = uK.GetProc (&wr)
          override xK'.DoHandle (wr, e) =
           wr.Handler <- uK ; xJs.Dispose () ; uK.DoHandle (&wr, e)
          override xK'.DoWork (wr) =
           if xJs.MoveNext () then
             xJs.Current.DoJob (&wr, xK')
           else
             wr.Handler <- uK ; xJs.Dispose () ; uK.DoWork (&wr)
          override xK'.DoCont (wr, _) =
           if xJs.MoveNext () then
             xJs.Current.DoJob (&wr, xK')
           else
             wr.Handler <- uK ; xJs.Dispose () ;  uK.DoWork (&wr)}
         wr.Handler <- xK'
         xJs.Current.DoJob (&wr, xK')
       else
         xJs.Dispose () ; Work.Do (uK, &wr)}

  type [<AbstractClass>] ConCollect<'x, 'y> =
    inherit Work
    [<DefaultValue>] val mutable Lock: WorkQueueLock
    [<DefaultValue>] val mutable N: int
    [<DefaultValue>] val mutable Exns: ResizeArray<exn>
    val mutable xs: IEnumerator<'x>
    val ysK: Cont<ResizeArray<'y>>
    static member Continue (cc': ConCollect<'x, 'y>, wr: byref<Worker>) =
       let ysK = cc'.ysK
       wr.Handler <- ysK
       match cc'.xs with
        | null -> ()
        | xs -> xs.Dispose ()
       match cc'.Exns with
        | null -> ysK.DoWork (&wr)
        | exns -> ysK.DoHandle (&wr, aggrExn exns)
    static member inline Done (cc': ConCollect<'x, 'y>, wr: byref<Worker>) =
     if cc'.ysK.Value.Count < Util.inc &cc'.N then
       ConCollect<'x, 'y>.Continue (cc', &wr)
    override cc'.GetProc (wr) = cc'.ysK.GetProc (&wr)
    override cc'.DoHandle (wr, e) =
     let exns =
       match cc'.Exns with
        | null ->
          let exns = ResizeArray<_> ()
          cc'.Exns <- exns
          exns
        | exns -> exns
     exns.Add e
     ConCollect<'x, 'y>.Done (cc', &wr)
    static member OutsideDoHandle (cc': ConCollect<'x, 'y>, wr: byref<Worker>, e) =
     cc'.Lock.Enter (&wr, {new Work () with
      override wk.GetProc (_) = raise <| NotImplementedException ()
      override wk.DoHandle (_, _) = raise <| NotImplementedException ()
      override wk.DoWork (wr) = cc'.DoHandle (&wr, e)})
    new (xs, ysK) = {inherit Work (); xs=xs; ysK=ysK}

  let conCollect (xJs: seq<Job<'x>>) =
    {new Job<ResizeArray<'x>> () with
      override xsJ'.DoJob (wr, xsK_) =
       xsK_.Value <- ResizeArray<_> ()
       let cc' = {new ConCollect<Job<'x>, 'x> (xJs.GetEnumerator (), xsK_) with
         override cc'.DoWork (wr) =
          let mutable nth = 0
          let xs = cc'.xs
          while xs.MoveNext () do
            let xJ = xs.Current
            cc'.Lock.ExitAndEnter (&wr, cc')
            cc'.ysK.Value.Add Unchecked.defaultof<_>
            let i = Util.dec &nth
            Worker.PushNew (&wr, {new Cont_State<_, _, _> (xJ, i) with
             override xK'.GetProc (wr) = cc'.ysK.GetProc (&wr)
             override xK'.DoHandle (wr, e) =
              ConCollect<Job<'x>, 'x>.OutsideDoHandle (cc', &wr, e)
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
                   ConCollect<Job<'x>, 'x>.Done (cc', &wr)
               | xJ ->
                 xK'.State1 <- null
                 xJ.DoJob (&wr, xK')})
          xs.Dispose ()
          cc'.xs <- null
          ConCollect<Job<'x>, 'x>.Done (cc', &wr)}
       wr.Handler <- cc'
       cc'.Lock.Enter (&wr, cc')}

  type ConIgnore<'x> =
    inherit Handler
    [<DefaultValue>] val mutable Finished: int
    [<DefaultValue>] val mutable Started: int
    [<DefaultValue>] val mutable Exns: ResizeArray<exn> 
    val mutable xs: IEnumerator<'x>
    val uK: Cont<unit>
    new (xs, uK) = {xs=xs; uK=uK}
    static member inline Inc (self: ConIgnore<'x>) =
     self.Started <- self.Started - 1
    static member Continue (self: ConIgnore<'x>, wr: byref<Worker>) =
     let uK = self.uK
     wr.Handler <- uK
     match self.xs with
      | null -> ()
      | xs -> xs.Dispose ()
     match self.Exns with
      | null -> uK.DoWork (&wr)
      | exns -> uK.DoHandle (&wr, aggrExn exns)
    static member inline Dec (self: ConIgnore<'x>, wr: byref<Worker>) =
     if Interlocked.Increment &self.Finished = self.Started then
       ConIgnore.Continue (self, &wr)
    static member inline Done (self: ConIgnore<'x>, wr: byref<Worker>) =
     self.Started <- 1 - self.Started
     ConIgnore<'x>.Dec (self, &wr)
    static member AddExn (self: ConIgnore<'x>, e: exn) =
     Monitor.Enter self
     let exns =
       match self.Exns with
        | null ->
          let exns = ResizeArray<_> ()
          self.Exns <- exns
          exns
        | exns -> exns
     exns.Add e
     Monitor.Exit self
    static member OutsideDoHandle (self: ConIgnore<'x>, wr: byref<Worker>, e: exn) =
     ConIgnore<'x>.AddExn (self, e)
     ConIgnore<'x>.Dec (self, &wr)
    override self.GetProc (wr) = self.uK.GetProc (&wr)
    override self.DoHandle (wr: byref<Worker>, e: exn) =
     ConIgnore<'x>.AddExn (self, e)
     ConIgnore<'x>.Done (self, &wr)

  let conIgnore (xJs: seq<Job<'x>>) =
    {new Job<unit> () with
      override uJ.DoJob (wr, uK) =
       let xJs = xJs.GetEnumerator ()
       let join = ConIgnore (xJs, uK)
       wr.Handler <- join
       while xJs.MoveNext () do
         let xJ = xJs.Current
         ConIgnore.Inc join
         Worker.PushNew (&wr, {new Cont_State<_, _> (xJ) with
          override xK'.GetProc (wr) = join.uK.GetProc (&wr)
          override xK'.DoHandle (wr, e) = ConIgnore.OutsideDoHandle (join, &wr, e)
          override xK'.DoCont (wr, _) = ConIgnore.Dec (join, &wr)
          override xK'.DoWork (wr) =
           match xK'.State with
            | null ->
              ConIgnore.Dec (join, &wr)
            | xJ ->
              xK'.State <- null
              xJ.DoJob (&wr, xK')})
       xJs.Dispose ()
       join.xs <- null
       ConIgnore.Done (join, &wr)}

  ///////////////////////////////////////////////////////////////////////

  let inline fromBeginEnd (doBegin: AsyncCallback * obj -> IAsyncResult)
                          (doEnd: IAsyncResult -> 'x) =
    {new JobFromBeginEnd<'x> () with
      override xJ'.DoBegin (acb, s) = doBegin (acb, s)
      override xJ'.DoEnd (iar) = doEnd iar} :> Job<_>

  let inline fromEndBegin (doEnd: IAsyncResult -> 'x)
                          (doBegin: AsyncCallback * obj -> IAsyncResult) =
    {new JobFromBeginEnd<'x> () with
      override xJ'.DoBegin (acb, s) = doBegin (acb, s)
      override xJ'.DoEnd (iar) = doEnd iar} :> Job<_>

  ///////////////////////////////////////////////////////////////////////

  let inline scheduler () = StaticData.scheduler

/////////////////////////////////////////////////////////////////////////

module Proc =
  let start (xJ: Job<_>) =
    {new Job<Proc> () with
      override pJ'.DoJob (wr, pK) =
       let proc = Proc ()
       pK.Value <- proc
       Worker.Push (&wr, pK)
       let pf = ProcFinalizer<_> (wr.Scheduler, proc)
       wr.Handler <- pf
       Job.Do (xJ, &wr, pf)}

  let queue (xJ: Job<_>) =
    {new Job<Proc> () with
      override pJ'.DoJob (wr, pK) =
       let proc = Proc ()
       Worker.Push (&wr, {new WorkHandler () with
        override w'.DoWork (wr) =
         let pf = ProcFinalizer<_> (wr.Scheduler, proc)
         wr.Handler <- pf
         xJ.DoJob (&wr, pf)})
       Cont.Do (pK, &wr, proc)}

  let inline self () = StaticData.proc

  module Alt =
    let inline join (p: Proc) = p :> Alt<unit>

/////////////////////////////////////////////////////////////////////////

module Timer =
  module Global =
    type [<AllowNullLiteral>] WorkTimedUnitCont =
      inherit WorkTimed
      val uK: Cont<unit>
      override wt.GetProc (wr) = wt.uK.GetProc (&wr)
      override wt.DoHandle (wr, e) = wt.uK.DoHandle (&wr, e)
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
        override uA'.TryAlt (wr, i, uK, uE) =
         (initGlobalTimer ()).SynchronizedPushTimed
          (WorkTimedUnitCont (Environment.TickCount + ms, i, uE.pk, uK))
         uE.TryElse (&wr, i+1)}

    let sleep (span: TimeSpan) = timeOut span :> Job<unit>

/////////////////////////////////////////////////////////////////////////

module Lock =
  module Now =
    let inline create () = Lock ()
  let create () = ctor Now.create ()
  let inline duringFun (l: Lock) (xF: unit -> 'x) = LockDuringFun<'x> (l, xF) :> Job<'x>
  let inline duringJob (l: Lock) (xJ: Job<'x>) = LockDuringJob<'x> (l, xJ) :> Job<'x>

#if NOT_YET_IMPLEMENTED
/// Operations on condition variables.
module Cond =
  val create: Lock -> (unit -> bool) -> Job<Cond>
  val wait: Cond -> Job<unit>
  val signal: Cond -> Job<unit>
  module Now =
    val create: unit -> Cond
#endif

/////////////////////////////////////////////////////////////////////////

module MVar =
  open Job.Infixes
  open Alt.Infixes
  module Now =
    let inline create () = MVar<'x> ()
    let inline createFull (x: 'x) = MVar<'x> (x)
  let create () = ctor Now.create ()
  let createFull x = ctor Now.createFull x
  let inline fill (xM: MVar<'x>) (x: 'x) = MVarFill<'x> (xM, x) :> Job<unit>
  let inline read (xM: MVar<'x>) = xM >>= fun x -> fill xM x >>% x
  let inline take (xM: MVar<'x>) = xM :> Job<'x>
  let inline modifyFun (x2xy: 'x -> 'x * 'y) (xM: MVar<'x>) =
    xM >>= (x2xy >> fun (x, y) -> fill xM x >>% y)
  let inline modifyJob (x2xyJ: 'x -> Job<'x * 'y>) (xM: MVar<'x>) =
    xM >>= x2xyJ >>= fun (x, y) -> fill xM x >>% y
  module Alt =
    let inline read (xM: MVar<'x>) = xM >>=? fun x -> fill xM x >>% x
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
            override yK'.GetProc (wr) = ysK.GetProc (&wr)
            override yK'.DoHandle (wr, e) = ysK.DoHandle (&wr, e)
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
          override yK'.GetProc (wr) = uK.GetProc (&wr)
          override yK'.DoHandle (wr, e) = uK.DoHandle (&wr, e)
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
         let yK' = {new Cont<'y> () with
          override yK'.GetProc (wr) = uK.GetProc (&wr)
          override yK'.DoHandle (wr, e) =
           wr.Handler <- uK ; xs.Dispose () ; uK.DoHandle (&wr, e)
          override yK'.DoCont (wr, _) =
           if xs.MoveNext () then
             (x2yJ xs.Current).DoJob (&wr, yK')
           else
             wr.Handler <- uK ; xs.Dispose () ; uK.DoWork (&wr)
          override yK'.DoWork (wr) =
           if xs.MoveNext () then
             (x2yJ xs.Current).DoJob (&wr, yK')
           else
             wr.Handler <- uK ; xs.Dispose () ; uK.DoWork (&wr)}
         wr.Handler <- yK'
         Work.Do (yK', &wr)}

    let mapJob (x2yJ: 'x -> Job<'y>) (xs: seq<'x>) =
      {new Job<ResizeArray<'y>> () with
        override ysJ'.DoJob (wr, ysK) =
         ysK.Value <- ResizeArray<_> ()
         let xs = xs.GetEnumerator ()
         if xs.MoveNext () then
           let yK' = {new Cont<'y> () with
            override yK'.GetProc (wr) = ysK.GetProc (&wr)
            override yK'.DoHandle (wr, e) =
             wr.Handler <- ysK ; xs.Dispose () ; ysK.DoHandle (&wr, e)
            override yK'.DoWork (wr) =
             ysK.Value.Add yK'.Value
             if xs.MoveNext () then
               (x2yJ xs.Current).DoJob (&wr, yK')
             else
               wr.Handler <- ysK ; xs.Dispose () ; ysK.DoWork (&wr)
            override yK'.DoCont (wr, y) =
             ysK.Value.Add y
             if xs.MoveNext () then
               (x2yJ xs.Current).DoJob (&wr, yK')
             else
               wr.Handler <- ysK ; xs.Dispose () ; ysK.DoWork (&wr)}
           wr.Handler <- yK'
           (x2yJ xs.Current).DoJob (&wr, yK')
         else
           xs.Dispose ()
           Work.Do (ysK, &wr)}

    let foldJob (xy2xJ: 'x -> 'y -> Job<'x>) (x: 'x) (ys: seq<'y>) =
      let xy2xJ = OptimizedClosures.FSharpFunc<_, _, _>.Adapt xy2xJ
      {new Job<'x> () with
        override xJ'.DoJob (wr, xK) =
         let ys = ys.GetEnumerator ()
         if ys.MoveNext () then
           let xK' = {new Cont<'x> () with
            override xK'.GetProc (wr) = xK.GetProc (&wr)
            override xK'.DoHandle (wr, e) =
             wr.Handler <- xK ; ys.Dispose () ; xK.DoHandle (&wr, e)
            override xK'.DoWork (wr) =
             if ys.MoveNext () then
               (xy2xJ.Invoke (xK'.Value, ys.Current)).DoJob (&wr, xK')
             else
               wr.Handler <- xK ; ys.Dispose () ; xK.DoCont (&wr, xK'.Value)
            override xK'.DoCont (wr, x) =
             if ys.MoveNext () then
               (xy2xJ.Invoke (x, ys.Current)).DoJob (&wr, xK')
             else
               wr.Handler <- xK ; ys.Dispose () ; xK.DoCont (&wr, x)}
           wr.Handler <- xK'
           (xy2xJ.Invoke (x, ys.Current)).DoJob (&wr, xK')
         else
           ys.Dispose ()
           Cont.Do (xK, &wr, x)}

    module Con =
      let iterJob (x2yJ: 'x -> Job<'y>) (xs: seq<'x>) =
        {new Job<unit> () with
          override uJ'.DoJob (wr, uK) =
           let xs = xs.GetEnumerator ()
           let join = ConIgnore (xs, uK)
           wr.Handler <- join
           while xs.MoveNext () do
             let x = xs.Current
             ConIgnore.Inc join
             Worker.PushNew (&wr, {new Cont_State<_, _, _> (x, x2yJ) with
              override yK'.GetProc (wr) = join.uK.GetProc (&wr)
              override yK'.DoHandle (wr, e) = ConIgnore.OutsideDoHandle (join, &wr, e)
              override yK'.DoCont (wr, _) = ConIgnore.Dec (join, &wr)
              override yK'.DoWork (wr) =
               let x2yJ = yK'.State2
               match x2yJ :> obj with
                | null ->
                  ConIgnore.Dec (join, &wr)
                | _ ->
                  let x = yK'.State1
                  yK'.State1 <- Unchecked.defaultof<_>
                  yK'.State2 <- Unchecked.defaultof<_>
                  (x2yJ x).DoJob (&wr, yK')})
           xs.Dispose ()
           join.xs <- null
           ConIgnore.Done (join, &wr)}

      let mapJob (x2yJ: 'x -> Job<'y>) (xs: seq<'x>) =
        {new Job<ResizeArray<'y>> () with
          override ysJ'.DoJob (wr, ysK_) =
           ysK_.Value <- ResizeArray<_> ()
           let cc' = {new ConCollect<'x, 'y> (xs.GetEnumerator (), ysK_) with
             override cc'.DoWork (wr) =
              let mutable nth = 0
              let xs = cc'.xs
              while xs.MoveNext () do
                let x = xs.Current
                cc'.Lock.ExitAndEnter (&wr, cc')
                cc'.ysK.Value.Add Unchecked.defaultof<_>
                let i = Util.dec &nth
                Worker.PushNew (&wr, {new Cont_State<_, _, _, _> (x, i, x2yJ) with
                 override yK'.GetProc (wr) = cc'.ysK.GetProc (&wr)
                 override yK'.DoHandle (wr, e) =
                  ConCollect<'x, 'y>.OutsideDoHandle (cc', &wr, e)
                 override yK'.DoCont (wr, y) =
                  yK'.Value <- y
                  yK'.State2 <- ~~~ yK'.State2
                  cc'.Lock.Enter (&wr, yK')
                 override yK'.DoWork (wr) =
                  let x2yJ = yK'.State3
                  match x2yJ :> obj with
                   | null ->
                     let i = yK'.State2
                     if i < 0 then
                       yK'.State2 <- ~~~ i
                       cc'.Lock.Enter (&wr, yK')
                     else
                       cc'.ysK.Value.[i] <- yK'.Value
                       ConCollect<'x, 'y>.Done (cc', &wr)
                   | _ ->
                     let x = yK'.State1
                     yK'.State3 <- Unchecked.defaultof<_>
                     yK'.State1 <- Unchecked.defaultof<_>
                     (x2yJ x).DoJob (&wr, yK')})
              xs.Dispose ()
              cc'.xs <- null
              ConCollect<'x, 'y>.Done (cc', &wr)}
           wr.Handler <- cc'
           cc'.Lock.Enter (&wr, cc')}

  ///////////////////////////////////////////////////////////////////////

  open Job.Infixes
  
  type [<Sealed>] Task =
    static member inline awaitJob (xTask: Tasks.Task<'x>) =
      AwaitTaskWithResult<'x> (xTask) :> Job<'x>

    static member inline awaitJob (task: Tasks.Task) =
      AwaitTask (task) :> Job<unit>

    static member inline bindJob (xT: Tasks.Task<'x>, x2yJ: 'x -> Job<'y>) =
      {new BindTaskWithResult<'x, 'y> (xT) with
        override yJ'.Do (x) = x2yJ x} :> Job<_>

    static member inline bindJob (uT: Tasks.Task, u2xJ: unit -> Job<'x>) =
      {new BindTask<'x> (uT) with
        override xJ'.Do () = u2xJ ()} :> Job<_>

    static member startJob (xJ: Job<'x>) =
      {new Job<Tasks.Task<'x>> () with
        override xTJ'.DoJob (wr, xTK) =
         let xTCS = Tasks.TaskCompletionSource<'x> ()
         xTK.Value <- xTCS.Task
         Worker.Push (&wr, xTK)
         Job.Do (xJ, &wr, {new Cont_State<'x, Cont<unit>> () with
          override xK'.GetProc (wr) = Handler.GetProc (&wr, &xK'.State)
          override xK'.DoHandle (wr, e) =
           Handler.Terminate (&wr, xK'.State)
           xTCS.TrySetException e |> ignore
          override xK'.DoWork (wr) =
           Handler.Terminate (&wr, xK'.State)
           xTCS.TrySetResult xK'.Value |> ignore
          override xK'.DoCont (wr, x) =
           Handler.Terminate (&wr, xK'.State)
           xTCS.TrySetResult x |> ignore})}

/////////////////////////////////////////////////////////////////////////

open Extensions
open Job.Infixes

type JobBuilder () =
  member inline job.Bind (xT: Tasks.Task<'x>, x2yJ: 'x -> Job<'y>) : Job<'y> =
    Task.bindJob (xT, x2yJ)
  member inline job.Bind (uT: Tasks.Task, u2xJ: unit -> Job<'x>) : Job<'x> =
    Task.bindJob (uT, u2xJ)
  member inline job.Bind (xJ: Job<'x>, x2yJ: 'x -> Job<'y>) : Job<'y> =
    xJ >>= x2yJ
  member inline job.Combine (uT: Tasks.Task, xJ: Job<'x>) : Job<'x> =
    Task.awaitJob uT >>. xJ
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
  member inline job.Zero () : Job<unit> = StaticData.unit :> Job<unit>

[<AutoOpen>]
module TopLevel =
  let job = JobBuilder ()

  let inline run x = Job.Global.run x

  let inline start x = Job.Global.start x

  let inline asAlt (xA: Alt<'x>) = xA
  let inline asJob (xJ: Job<'x>) = xJ

  let inline ch () = Ch<'x> ()
  let inline mb () = Mailbox<'x> ()
  let inline ivar () = IVar<'x> ()
  let inline mvar () = MVar<'x> ()

/////////////////////////////////////////////////////////////////////////

module Infixes =
  let inline (<-?) (xCh: Ch<'x>) (x: 'x) = ChGive<'x> (xCh, x) :> Alt<unit>
  let inline (<--) (xCh: Ch<'x>) (x: 'x) = ChGive<'x> (xCh, x) :> Job<unit>
  let inline (<-+) (xCh: Ch<'x>) (x: 'x) = ChSend<'x> (xCh, x) :> Job<unit>
  let inline (<-=) (xI: IVar<'x>) (x: 'x) = IVarFill<'x> (xI, x) :> Job<unit>
  let inline (<-=!) (xI: IVar<'x>) (e: exn) = IVarFillFailure<'x> (xI, e) :> Job<unit>
  let inline (<<-=) (xM: MVar<'x>) (x: 'x) = MVarFill<'x> (xM, x) :> Job<unit>
  let inline (<<-+) (xMb: Mailbox<'x>) (x: 'x) = MailboxSend<'x> (xMb, x) :> Job<unit>
