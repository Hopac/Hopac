// Copyright (C) by Housemarque, Inc.

namespace Hopac

open System
open System.Collections.Generic
open System.Diagnostics
open System.Runtime.CompilerServices
open System.Threading
open System.Threading.Tasks
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

  type TryInCont<'x, 'x2yJ, 'y, 'e2yJ> when 'x2yJ :> Job<'y> and 'e2yJ :> Job<'y> =
    inherit Cont<'x>
    val x2yJ: 'x -> 'x2yJ
    val e2yJ: exn -> 'e2yJ
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

  type TryFinallyFunCont<'x> =
    inherit Cont<'x>
    val u2u: unit -> unit
    val mutable xK: Cont<'x>
    new (u2u, xK) = {inherit Cont<'x> (); u2u=u2u; xK=xK}
    override xK'.GetProc (wr) = Handler.GetProc (&wr, &xK'.xK)
    override xK'.DoHandle (wr, e) =
      let xK = xK'.xK
      wr.Handler <- xK
      xK'.u2u ()
      Handler.DoHandle (xK, &wr, e)
    override xK'.DoWork (wr) =
      let xK = xK'.xK
      wr.Handler <- xK
      xK'.u2u ()
      xK.DoCont (&wr, xK'.Value)
    override xK'.DoCont (wr, x) =
      let xK = xK'.xK
      wr.Handler <- xK
      xK'.u2u ()
      xK.DoCont (&wr, x)

  type DropCont<'x, 'y> =
    inherit Cont<'y>
    val mutable xK: Cont<'x>
    new (xK) = {inherit Cont<'y> (); xK=xK}
    override yK'.GetProc (wr) = Handler.GetProc (&wr, &yK'.xK)
    override yK'.DoHandle (wr, e) = Handler.DoHandle (yK'.xK, &wr, e)
    override yK'.DoWork (wr) = yK'.xK.DoWork (&wr)
    override yK'.DoCont (wr, _) = yK'.xK.DoWork (&wr)

  type TryFinallyJobCont<'x> =
    inherit Cont<'x>
    val uJ: Job<unit>
    val mutable xK: Cont<'x>
    new (uJ, xK) = {inherit Cont<'x> (); uJ=uJ; xK=xK}
    override xK'.GetProc (wr) = Handler.GetProc (&wr, &xK'.xK)
    override xK'.DoHandle (wr, e) =
      let xK = xK'.xK
      wr.Handler <- xK
      xK'.uJ.DoJob (&wr, FailCont<unit> (xK, e))
    override xK'.DoWork (wr) =
      let xK = xK'.xK
      wr.Handler <- xK
      xK.Value <- xK'.Value
      xK'.uJ.DoJob (&wr, DropCont<'x, unit> (xK))
    override xK'.DoCont (wr, x) =
      let xK = xK'.xK
      wr.Handler <- xK
      xK.Value <- x
      xK'.uJ.DoJob (&wr, DropCont<'x, unit> (xK))

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
  let inline fill (xI: IVar<'x>) (x: 'x) = IVar<'x>.Fill (xI, x) :> Job<unit>
  let inline tryFill (xI: IVar<'x>) (x: 'x) = IVar<'x>.TryFill (xI, x) :> Job<unit>
  let inline fillFailure (xI: IVar<'x>) (e: exn) =
    IVar<'x>.FillFailure (xI, e) :> Job<unit>
  let inline read (xI: IVar<'x>) = xI :> Alt<'x>

/////////////////////////////////////////////////////////////////////////

module Promise =
  let start (xJ: Job<'x>) =
    {new Job<Promise<'x>> () with
      override self.DoJob (wr, xPrK) =
       let pr = Promise<'x> ()
       xPrK.Value <- pr
       Worker.Push (&wr, xPrK)
       Job.Do (xJ, &wr, Promise<'x>.Fulfill (pr))}
  let queue (xJ: Job<'x>) =
    {new Job<Promise<'x>> () with
      override self.DoJob (wr, xPrK) =
       let pr = Promise<'x> ()
       Worker.PushNew (&wr, {new WorkHandler () with
        override w'.DoWork (wr) =
         let prc = Promise<'x>.Fulfill (pr)
         wr.Handler <- prc
         xJ.DoJob (&wr, prc)})
       Cont.Do (xPrK, &wr, pr)}
  module Now =
    let inline delay (xJ: Job<'x>) = Promise<'x> (xJ)
    let inline withValue (x: 'x) = Promise<'x> (x)
    let inline withFailure (e: exn) = Promise<'x> (e)
    let inline isFulfilled (xP: Promise<'x>) = xP.Full
    let get (xP: Promise<'x>) = xP.Get ()
  let inline read (xPr: Promise<'x>) = xPr :> Alt<'x>

/////////////////////////////////////////////////////////////////////////

module Alt =
  let inline always (x: 'x) = Always<'x> (x) :> Alt<'x>

  let inline unit () = StaticData.unit

  let inline never () = Never<_>() :> Alt<_>

  let inline zero () = StaticData.zero :> Alt<_>

  type GuardJobCont<'x, 'xA> when 'xA :> Alt<'x> =
    inherit Cont<'xA>
    val mutable xK: Cont<'x>
    new (xK) = {inherit Cont<'xA> (); xK=xK}
    override xAK'.GetProc (wr) = Handler.GetProc (&wr, &xAK'.xK)
    override xAK'.DoHandle (wr, e) = Handler.DoHandle (xAK'.xK, &wr, e)
    override xAK'.DoWork (wr) = xAK'.Value.DoJob (&wr, xAK'.xK)
    override xAK'.DoCont (wr, xA) = xA.DoJob (&wr, xAK'.xK)

  type GuardCont<'x, 'xA> when 'xA :> Alt<'x> =
   inherit Cont<'xA>
   val i: int
   val mutable xK: Cont<'x>
   val xE: Else
   new (i, xK, xE) = {inherit Cont<'xA> (); i=i; xK=xK; xE=xE}
   override xAK'.GetProc (wr) =
    Handler.GetProc (&wr, &xAK'.xK)
   override xAK'.DoHandle (wr, e) =
    Pick.PickClaimedAndSetNacks (&wr, xAK'.i, xAK'.xE.pk)
    Handler.DoHandle (xAK'.xK, &wr, e)
   override xAK'.DoWork (wr) =
    let xE = xAK'.xE
    Pick.Unclaim xE.pk
    xAK'.Value.TryAlt (&wr, xAK'.i, xAK'.xK, xE)
   override xAK'.DoCont (wr, xA) =
    let xE = xAK'.xE
    Pick.Unclaim xE.pk
    xA.TryAlt (&wr, xAK'.i, xAK'.xK, xE)

  let guard (xAJ: Job<#Alt<'x>>) =
    {new Alt<'x> () with
      override xA'.DoJob (wr, xK) =
       xAJ.DoJob (&wr, GuardJobCont xK)
      override xA'.TryAlt (wr, i, xK, xE) =
       Pick.ClaimAndDoJob (xE.pk, &wr, xAJ, GuardCont (i, xK, xE))}

  let inline delay (u2xA: unit -> #Alt<'x>) =
    {new AltDelay<'x> () with
      override xA'.Do () = upcast u2xA ()} :> Alt<_>

  type WithNackElse (nk: Nack, xE: Else) =
    inherit Else (xE.pk)
    override xE'.TryElse (wr, i) =
      nk.I1 <- i
      xE.TryElse (&wr, i)

  type WithNackCont<'x, 'xA> when 'xA :> Alt<'x> =
    inherit Cont<'xA>
    val mutable xK: Cont<'x>
    val xE: Else
    new (xK, xE) = {inherit Cont<'xA> (); xK=xK; xE=xE}
    override xAK'.GetProc (wr) =
     Handler.GetProc (&wr, &xAK'.xK)
    override xAK'.DoHandle (wr, e) =
     Pick.PickClaimedAndSetNacks (&wr, xAK'.xE.pk.Nacks.I0, xAK'.xE.pk)
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

  let withNack (nack2xAJ: Promise<unit> -> #Job<#Alt<'x>>) =
    {new Alt<'x> () with
      override xA'.DoJob (wr, xK) =
       (nack2xAJ StaticData.zero).DoJob (&wr, GuardJobCont xK)
      override xA'.TryAlt (wr, i, xK, xE) =
       match Pick.AddNack (xE.pk, i) with
        | null -> ()
        | nk -> (nack2xAJ nk).DoJob (&wr, WithNackCont (xK, xE))}

  let inline map (x2y: 'x -> 'y) (xA: Alt<'x>) =
    {new AltMap<'x, 'y> (xA) with
      override yA'.Do (x) = x2y x} :> Alt<_>

  let inline wrap (x2yJ: 'x -> #Job<'y>) (xA: Alt<'x>) =
    {new AltBind<'x, 'y> (xA) with
      override yA'.Do (x) = upcast x2yJ x} :> Alt<_>

  module Infixes =
    let (<|>?) (xA1: Alt<'x>) (xA2: Alt<'x>) =
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

    let inline (>>=?) (xA: Alt<'x>) (x2yJ: 'x -> #Job<'y>) =
      {new AltBind<'x, 'y> (xA) with
        override yA'.Do (x) = upcast x2yJ x} :> Alt<_>

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

  let choose (xAs: seq<#Alt<'x>>) =
    {new Alt<'x> () with
      override xA'.DoJob (wr, xK) =
       let xAs = xAs.GetEnumerator ()
       if xAs.MoveNext () then
         let pk = Pick ()
         xAs.Current.TryAlt (&wr, 0, xK, {new Else (pk) with
          override xE'.TryElse (wr, i) =
           if xAs.MoveNext () then
             xAs.Current.TryAlt (&wr, i, xK, xE')
           else
             xAs.Dispose ()})
       else
         xAs.Dispose ()
      override xA'.TryAlt (wr, i, xK, xE) =
       let xAs = xAs.GetEnumerator ()
       if xAs.MoveNext () then
         xAs.Current.TryAlt (&wr, i, xK, {new Else (xE.pk) with
          override xE'.TryElse (wr, i) =
           if xAs.MoveNext () then
             xAs.Current.TryAlt (&wr, i, xK, xE')
           else
             xAs.Dispose ()
             xE.TryElse (&wr, i)})
       else
         xAs.Dispose ()
         xE.TryElse (&wr, i)}

  let tryIn (xA: Alt<'x>) (x2yJ: 'x -> #Job<'y>) (e2yJ: exn -> #Job<'y>) =
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

  let tryFinallyFun (xA: Alt<'x>) (u2u: unit -> unit) =
    {new Alt<'x> () with
      override xA'.DoJob (wr, xK) =
       let xK' = TryFinallyFunCont (u2u, xK)
       wr.Handler <- xK'
       xA.DoJob (&wr, xK')
      override xA'.TryAlt (wr, i, xK, xE) =
       let xK' = TryFinallyFunCont (u2u, xK)
       wr.Handler <- xK'
       xA.TryAlt (&wr, i, xK', {new Else (xE.pk) with
        override xE'.TryElse (wr, i) =
         wr.Handler <- xK
         xE.TryElse (&wr, i)})}

  let tryFinallyJob (xA: Alt<'x>) (uJ: Job<unit>) =
    {new Alt<'x> () with
      override xA'.DoJob (wr, xK) =
       let xK' = TryFinallyJobCont (uJ, xK)
       wr.Handler <- xK'
       xA.DoJob (&wr, xK')
      override xA'.TryAlt (wr, i, xK, xE) =
       let xK' = TryFinallyJobCont (uJ, xK)
       wr.Handler <- xK'
       xA.TryAlt (&wr, i, xK', {new Else (xE.pk) with
        override xE'.TryElse (wr, i) =
         wr.Handler <- xK
         xE.TryElse (&wr, i)})}

  let paranoid (xA: Alt<'x>) =
    {new Alt<'x> () with
      override xA'.DoJob (wr, xK) = xA.DoJob (&wr, xK)
      override xA'.TryAlt (wr, i, xK, xE) = xA.TryAlt (&wr, i, xK, xE)}

/////////////////////////////////////////////////////////////////////////

module Scheduler =
  open System.Reflection

  type Create =
    {Foreground: option<bool>
     IdleHandler: option<Job<int>>
     MaxStackSize: option<int>
     NumWorkers: option<int>
//     Priority: option<ThreadPriority>
     TopLevelHandler: option<exn -> Job<unit>>}
    static member Def: Create =
      StaticData.Init ()
      {Foreground = None
       IdleHandler = None
       MaxStackSize = None
       NumWorkers = None
//       Priority = None
       TopLevelHandler = None}

  module Global =
    let mutable create = Create.Def

    let setCreate (c: Create) =
      create <- c

  let create (c: Create) =
    StaticData.createScheduler.Invoke
     (Option.orDefaultOf c.Foreground,
      Option.orDefaultOf c.IdleHandler,
      Option.orDefaultOf c.MaxStackSize,
      (match c.NumWorkers with
        | None -> Environment.ProcessorCount
        | Some n ->
          if n < 1 then
            failwithf "Invalid number of workers specified: %d" n
          n),
//      (match c.Priority with
//        | None -> ThreadPriority.Normal
//        | Some p -> p)
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

  let reallyInitGlobalScheduler () =
    let t = typeof<Scheduler>
    Monitor.Enter t
    match StaticData.globalScheduler with
     | null ->
       if StaticData.isMono then
         if System.GC.MaxGeneration = 0 then
           StaticData.writeLine.Invoke "WARNING: You are using the Boehm GC, \
            which means that parallel programs cannot scale.  Please configure \
            your program to use the SGen GC."
       elif not System.Runtime.GCSettings.IsServerGC then
         StaticData.writeLine.Invoke "WARNING: You are using single-threaded \
          workstation garbage collection, which means that parallel programs \
          cannot scale.  Please configure your program to use server garbage \
          collection.  See \
          http://msdn.microsoft.com/en-us/library/ms229357%%28v=vs.110%%29.aspx \
          for details.\n"
       let sr = Scheduler.create Scheduler.Global.create
       StaticData.globalScheduler <- sr
       Monitor.Exit t
       sr
     | sr ->
       Monitor.Exit t
       sr

  let inline initGlobalScheduler () =
    match StaticData.globalScheduler with
     | null -> reallyInitGlobalScheduler ()
     | sr -> sr

  let reallyInitGlobalTimer () =
    let sr = initGlobalScheduler ()
    let t = typeof<Timer>
    Monitor.Enter t
    match StaticData.globalTimer with
     | null ->
       let tr = Timer sr
       StaticData.globalTimer <- tr
       Monitor.Exit t
       tr
     | tr ->
       Monitor.Exit t
       tr

  let inline initGlobalTimer () =
    match StaticData.globalTimer with
     | null -> reallyInitGlobalTimer ()
     | tr -> tr

/////////////////////////////////////////////////////////////////////////

module Ch =
  module Now =
    let inline create () = Ch<'x> ()
  module Global =
    [<MethodImpl(MethodImplOptions.NoInlining)>]
    let send (xCh: Ch<'x>) (x: 'x) =
      Ch<'x>.Send (StaticData.globalScheduler, xCh, x)
  let create () = ctor Now.create ()
  let inline send (xCh: Ch<'x>) (x: 'x) = ChSend<'x> (xCh, x) :> Job<unit>
  module Try =
    let inline give (xCh: Ch<'x>) (x: 'x) = ChTryGive<'x> (xCh, x) :> Job<bool>
    let inline take (xCh: Ch<'x>) = ChTryTake<'x> (xCh) :> Job<option<'x>>
  let inline give (xCh: Ch<'x>) (x: 'x) = ChGive<'x> (xCh, x) :> Alt<unit>
  let inline take (xCh: Ch<'x>) = xCh :> Alt<'x>

/////////////////////////////////////////////////////////////////////////

module Mailbox =
  module Now =
    let inline create () = Mailbox<'x> ()
  module Global =
    [<MethodImpl(MethodImplOptions.NoInlining)>]
    let send (xMb: Mailbox<'x>) (x: 'x) =
      Mailbox<'x>.Send (StaticData.globalScheduler, xMb, x)
  let create () = ctor Now.create ()
  let inline send (xMb: Mailbox<'x>) (x: 'x) =
    MailboxSend<'x> (xMb, x) :> Job<unit>
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

  let inline delay (u2xJ: unit -> #Job<'x>) =
    {new JobDelay<'x> () with
      override xJ'.Do () =
       upcast u2xJ ()} :> Job<_>

  let inline delayWith (x2yJ: 'x -> #Job<'y>) (x: 'x) =
    {new JobDelay<'y> () with
      override yJ'.Do () =
       upcast x2yJ x} :> Job<_>

  let lift (x2y: 'x -> 'y) (x: 'x) =
    {new Job<'y> () with
      override yJ'.DoJob (wr, yK) = Cont.Do (yK, &wr, x2y x)}

  let inline thunk (u2x: unit -> 'x) =
    {new JobThunk<'x> () with
      override xJ'.Do () = u2x ()} :> Job<_>

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
    let inline mkFor more next i0 i1 (i2xJ: _ -> #Job<'x>) =
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

  let forUpTo (i0: int) (i1: int) (i2xJ: int -> #Job<'x>) =
    Internal.mkFor (fun i i1 -> i <= i1) (fun i -> i + 1) i0 i1 i2xJ

  let forDownTo (i0: int) (i1: int) (i2xJ: int -> #Job<'x>) =
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

  let inline iterate (x: 'x) (x2xJ: 'x -> #Job<'x>) =
    {new JobRun<'y> () with
      override yJ'.Do (yK) =
        {new ContIterate<'x, 'y> (x, yK) with
          override xK'.Do (x) = upcast x2xJ x} :> Work} :> Job<_>

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
    // XXX Does this speed things up?
    if sizeof<IntPtr> <> 8 || StaticData.isMono then
      {new Job<'x> () with
        override self.DoJob (wr, xK) =
         Cont.Do (xK, &wr, x)}
    else
      {new Job<'x> () with
        override self.DoJob (wr, xK) =
         xK.DoCont (&wr, x)}

  let inline bind (x2yJ: 'x -> #Job<'y>) (xJ: Job<'x>) =
    {new JobBind<'x, 'y> (xJ) with
      override yJ'.Do (x) = upcast x2yJ x} :> Job<_>

  let inline join (xJJ: Job<#Job<'x>>) = JobJoin<_, _>(xJJ) :> Job<_>

  let inline map (x2y: 'x -> 'y) (xJ: Job<'x>) =
    {new JobMap<'x, 'y> (xJ) with
      override yJ'.Do (x) = x2y x} :> Job<_>

  let inline unit () = StaticData.unit :> Job<_>

  let abort () = Never<_>() :> Job<_>

  let raises (e: exn) =
    {new Job<_> () with
      override xJ.DoJob (wr, xK) = Handler.DoHandle (xK, &wr, e)}

  let inline whenDo (b: bool) (uJ: Job<unit>) =
    if b then uJ else StaticData.unit :> Job<_>

  ///////////////////////////////////////////////////////////////////////

  module Infixes =
    let inline (>>=) (xJ: Job<'x>) (x2yJ: 'x -> #Job<'y>) =
      {new JobBind<'x, 'y> (xJ) with
        override yJ'.Do (x) = upcast x2yJ x} :> Job<_>

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
         yJ.DoJob (&wr, yK')}

  ///////////////////////////////////////////////////////////////////////

  let inline tryIn (xJ: Job<'x>) (x2yJ: 'x -> #Job<'y>) (e2yJ: exn -> #Job<'y>) =
    {new JobTryIn<'x, 'y> (xJ) with
      override yJ'.DoIn (x) = upcast x2yJ x
      override yJ'.DoExn (e) = upcast e2yJ e} :> Job<_>

  let inline tryWith (xJ: Job<'x>) (e2xJ: exn -> #Job<'x>) =
    {new JobTryWith<'x> (xJ) with
      override xJ'.DoExn (e) = upcast e2xJ e} :> Job<_>

  let tryFinallyFun (xJ: Job<'x>) (u2u: unit -> unit) =
    {new Job<'x> () with
      override xJ'.DoJob (wr, xK_) =
       let xK' = TryFinallyFunCont (u2u, xK_)
       wr.Handler <- xK'
       xJ.DoJob (&wr, xK')}

  let tryFinallyJob (xJ: Job<'x>) (uJ: Job<unit>) =
    {new Job<'x> () with
      override xJ'.DoJob (wr, xK_) =
       let xK' = TryFinallyJobCont (uJ, xK_)
       wr.Handler <- xK'
       xJ.DoJob (&wr, xK')}

  let using (x: 'x when 'x :> IDisposable) (x2yJ: 'x -> #Job<'y>) =
    // REMINDER: Dispose() of managed resources is an optimization.  Do not
    // implement Finalize() for managed resources.  See:
    //   http://joeduffyblog.com/2005/04/08/dg-update-dispose-finalization-and-resource-management/
    {new Job<'y> () with
      override yJ'.DoJob (wr, yK_) =
       let yK' = {new Cont_State<'y, _> (yK_) with
        override yK'.GetProc (wr) = Handler.GetProc (&wr, &yK'.State)
        override yK'.DoHandle (wr, e) =
         x.Dispose ()
         let yK = yK'.State in wr.Handler <- yK ; Handler.DoHandle (yK, &wr, e)
        override yK'.DoWork (wr) =
         x.Dispose ()
         let yK = yK'.State in wr.Handler <- yK ; yK.DoCont (&wr, yK'.Value)
        override yK'.DoCont (wr, y) =
         x.Dispose ()
         let yK = yK'.State in wr.Handler <- yK ; yK.DoCont (&wr, y)}
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

  let inline iterateServer (x: 'x) (x2xJ: 'x -> #Job<'x>) =
    {new JobStart () with
      override uJ'.Do () =
       {new ContIterate<'x, unit> (x, null) with
         override xK'.Do (x) = upcast x2xJ x} :> Work} :> Job<_>

  ///////////////////////////////////////////////////////////////////////

  let seqCollect (xJs: seq<#Job<'x>>) =
    {new Job<ResizeArray<'x>> () with
      override self.DoJob (wr, xsK) =
       let xJs = xJs.GetEnumerator ()
       xsK.Value <- ResizeArray<_> ()
       if xJs.MoveNext () then
         let xK' = {new Cont<'x> () with
          override xK'.GetProc (wr) = xsK.GetProc (&wr)
          override xK'.DoHandle (wr, e) =
           xJs.Dispose ()
           wr.Handler <- xsK ; xsK.DoHandle (&wr, e)
          override xK'.DoWork (wr) =
           xsK.Value.Add xK'.Value
           if xJs.MoveNext () then
             xJs.Current.DoJob (&wr, xK')
           else
             xJs.Dispose ()
             wr.Handler <- xsK ; xsK.DoWork (&wr)
          override xK'.DoCont (wr, x) =
           xsK.Value.Add x
           if xJs.MoveNext () then
             xJs.Current.DoJob (&wr, xK')
           else
             xJs.Dispose ()
             wr.Handler <- xsK ; xsK.DoWork (&wr)}
         wr.Handler <- xK'
         xJs.Current.DoJob (&wr, xK')
       else
         xJs.Dispose ()
         Work.Do (xsK, &wr)}

  let seqIgnore (xJs: seq<#Job<'x>>) =
    {new Job<unit> () with
      override self.DoJob (wr, uK) =
       let xJs = xJs.GetEnumerator ()
       if xJs.MoveNext () then
         let xK' = {new Cont<'x> () with
          override xK'.GetProc (wr) = uK.GetProc (&wr)
          override xK'.DoHandle (wr, e) =
           xJs.Dispose ()
           wr.Handler <- uK ; uK.DoHandle (&wr, e)
          override xK'.DoWork (wr) =
           if xJs.MoveNext () then
             xJs.Current.DoJob (&wr, xK')
           else
             xJs.Dispose ()
             wr.Handler <- uK ; uK.DoWork (&wr)
          override xK'.DoCont (wr, _) =
           if xJs.MoveNext () then
             xJs.Current.DoJob (&wr, xK')
           else
             xJs.Dispose ()
             wr.Handler <- uK ; uK.DoWork (&wr)}
         wr.Handler <- xK'
         xJs.Current.DoJob (&wr, xK')
       else
         xJs.Dispose ()
         Work.Do (uK, &wr)}

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

  let conCollect (xJs: seq<#Job<'x>>) =
    {new Job<ResizeArray<'x>> () with
      override xsJ'.DoJob (wr, xsK_) =
       xsK_.Value <- ResizeArray<_> ()
       let cc' = {new ConCollect<_, 'x> (xJs.GetEnumerator (), xsK_) with
         override cc'.DoWork (wr) =
          let mutable nth = 0
          let xs = cc'.xs
          while xs.MoveNext () do
            let xJ = xs.Current :> Job<_>
            cc'.Lock.ExitAndEnter (&wr, cc')
            cc'.ysK.Value.Add Unchecked.defaultof<_>
            let i = Util.dec &nth
            Worker.PushNew (&wr, {new Cont_State<_, _, _> (xJ, i) with
             override xK'.GetProc (wr) = cc'.ysK.GetProc (&wr)
             override xK'.DoHandle (wr, e) =
              ConCollect<_, 'x>.OutsideDoHandle (cc', &wr, e)
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
                   ConCollect<_, 'x>.Done (cc', &wr)
               | xJ ->
                 xK'.State1 <- null
                 xJ.DoJob (&wr, xK')})
          xs.Dispose ()
          cc'.xs <- null
          ConCollect<_, 'x>.Done (cc', &wr)}
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

  let conIgnore (xJs: seq<#Job<'x>>) =
    {new Job<unit> () with
      override uJ.DoJob (wr, uK) =
       let xJs = xJs.GetEnumerator ()
       let join = ConIgnore (xJs, uK)
       wr.Handler <- join
       while xJs.MoveNext () do
         let xJ = xJs.Current :> Job<_>
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

  ///////////////////////////////////////////////////////////////////////

  let inline switchToWorker () = StaticData.switchToWorker

  ///////////////////////////////////////////////////////////////////////

  let paranoid (xJ: Job<'x>) =
    {new Job<'x> () with
      override xJ'.DoJob (wr, xK) = xJ.DoJob (&wr, xK)}

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

    let outOfRange ticks =
      failwithf "Timeout out of range (ticks = %d)" ticks

    let timeOutTicks ticks =
      let ms = (ticks + 9999L) / 10000L // Rounds up.
      if ticks <= 0L then
        if -10000L = ticks then
          StaticData.zero :> Alt<_>
        elif 0L = ticks then
          StaticData.unit
        else
          outOfRange ticks
      elif 21474836470000L < ticks then
        outOfRange ticks
      else
        let ms = int ms
        {new Alt<unit> () with
          override uA'.DoJob (wr, uK) =
           (initGlobalTimer ()).SynchronizedPushTimed
            (WorkTimedUnitCont (Environment.TickCount + ms, 0, null, uK))
          override uA'.TryAlt (wr, i, uK, uE) =
           (initGlobalTimer ()).SynchronizedPushTimed
            (WorkTimedUnitCont (Environment.TickCount + ms, i, uE.pk, uK))
           uE.TryElse (&wr, i+1)}
    
    let timeOut (span: System.TimeSpan) = timeOutTicks span.Ticks
    let timeOutMillis (ms: int) = timeOutTicks (int64 ms * 10000L)

/////////////////////////////////////////////////////////////////////////

module Lock =
  module Now =
    let inline create () = Lock ()
  let create () = ctor Now.create ()
  let inline duringFun (l: Lock) (xF: unit -> 'x) =
    LockDuringFun<'x> (l, xF) :> Job<'x>
  let inline duringJob (l: Lock) (xJ: Job<'x>) =
    LockDuringJob<'x> (l, xJ) :> Job<'x>

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
  let inline modifyFun (x2xy: 'x -> 'x * 'y) (xM: MVar<'x>) =
    xM >>= (x2xy >> fun (x, y) -> fill xM x >>% y)
  let inline modifyJob (x2xyJ: 'x -> #Job<'x * 'y>) (xM: MVar<'x>) =
    xM >>= x2xyJ >>= fun (x, y) -> fill xM x >>% y
  let inline read (xM: MVar<'x>) = xM >>=? fun x -> fill xM x >>% x
  let inline take (xM: MVar<'x>) = xM :> Alt<'x>

/////////////////////////////////////////////////////////////////////////

module Extensions =
  open Job

  module Array =
    let mapJob (x2yJ: 'x -> #Job<'y>) (xs: array<'x>) =
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

    let iterJob (x2yJ: 'x -> #Job<'y>) (xs: array<'x>) =
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
    let iterJob (x2yJ: 'x -> #Job<'y>) (xs: seq<'x>) =
      {new Job<unit> () with
        override uJ'.DoJob (wr, uK) =
         let xs = xs.GetEnumerator ()
         let yK' = {new Cont<'y> () with
          override yK'.GetProc (wr) = uK.GetProc (&wr)
          override yK'.DoHandle (wr, e) =
           xs.Dispose ()
           wr.Handler <- uK ; uK.DoHandle (&wr, e)
          override yK'.DoCont (wr, _) =
           if xs.MoveNext () then
             (x2yJ xs.Current).DoJob (&wr, yK')
           else
             xs.Dispose ()
             wr.Handler <- uK ; uK.DoWork (&wr)
          override yK'.DoWork (wr) =
           if xs.MoveNext () then
             (x2yJ xs.Current).DoJob (&wr, yK')
           else
             xs.Dispose ()
             wr.Handler <- uK ; uK.DoWork (&wr)}
         wr.Handler <- yK'
         Work.Do (yK', &wr)}

    let mapJob (x2yJ: 'x -> #Job<'y>) (xs: seq<'x>) =
      {new Job<ResizeArray<'y>> () with
        override ysJ'.DoJob (wr, ysK) =
         ysK.Value <- ResizeArray<_> ()
         let xs = xs.GetEnumerator ()
         if xs.MoveNext () then
           let yK' = {new Cont<'y> () with
            override yK'.GetProc (wr) = ysK.GetProc (&wr)
            override yK'.DoHandle (wr, e) =
             xs.Dispose ()
             wr.Handler <- ysK ; ysK.DoHandle (&wr, e)
            override yK'.DoWork (wr) =
             ysK.Value.Add yK'.Value
             if xs.MoveNext () then
               (x2yJ xs.Current).DoJob (&wr, yK')
             else
               xs.Dispose ()
               wr.Handler <- ysK ; ysK.DoWork (&wr)
            override yK'.DoCont (wr, y) =
             ysK.Value.Add y
             if xs.MoveNext () then
               (x2yJ xs.Current).DoJob (&wr, yK')
             else
               xs.Dispose ()
               wr.Handler <- ysK ; ysK.DoWork (&wr)}
           wr.Handler <- yK'
           (x2yJ xs.Current).DoJob (&wr, yK')
         else
           xs.Dispose ()
           Work.Do (ysK, &wr)}

    let foldJob (xy2xJ: 'x -> 'y -> #Job<'x>) (x: 'x) (ys: seq<'y>) =
      let xy2xJ = OptimizedClosures.FSharpFunc<_, _, _>.Adapt xy2xJ
      {new Job<'x> () with
        override xJ'.DoJob (wr, xK) =
         let ys = ys.GetEnumerator ()
         if ys.MoveNext () then
           let xK' = {new Cont<'x> () with
            override xK'.GetProc (wr) = xK.GetProc (&wr)
            override xK'.DoHandle (wr, e) =
             ys.Dispose ()
             wr.Handler <- xK ; xK.DoHandle (&wr, e)
            override xK'.DoWork (wr) =
             if ys.MoveNext () then
               (xy2xJ.Invoke (xK'.Value, ys.Current)).DoJob (&wr, xK')
             else
               ys.Dispose ()
               wr.Handler <- xK ; xK.DoCont (&wr, xK'.Value)
            override xK'.DoCont (wr, x) =
             if ys.MoveNext () then
               (xy2xJ.Invoke (x, ys.Current)).DoJob (&wr, xK')
             else
               ys.Dispose ()
               wr.Handler <- xK ; xK.DoCont (&wr, x)}
           wr.Handler <- xK'
           (xy2xJ.Invoke (x, ys.Current)).DoJob (&wr, xK')
         else
           ys.Dispose ()
           Cont.Do (xK, &wr, x)}

    module Con =
      let iterJob (x2yJ: 'x -> #Job<'y>) (xs: seq<'x>) =
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

      let mapJob (x2yJ: 'x -> #Job<'y>) (xs: seq<'x>) =
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

  type Task with
    static member inline awaitJob (xTask: Task<'x>) =
      AwaitTaskWithResult<'x> (xTask) :> Job<'x>

    static member inline awaitJob (task: Task) =
      AwaitTask (task) :> Job<unit>

    static member inline bindJob (xT: Task<'x>, x2yJ: 'x -> #Job<'y>) =
      {new BindTaskWithResult<'x, 'y> (xT) with
        override yJ'.Do (x) = upcast x2yJ x} :> Job<_>

    static member inline bindJob (uT: Task, u2xJ: unit -> #Job<'x>) =
      {new BindTask<'x> (uT) with
        override xJ'.Do () = upcast u2xJ ()} :> Job<_>

    static member startJob (xJ: Job<'x>) =
      {new Job<Task<'x>> () with
        override xTJ'.DoJob (wr, xTK) =
         let xTCS = TaskCompletionSource<'x> ()
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

//  type ThreadPool with
//    static member queueAsJob (thunk: unit -> 'x) : Job<'x> =
//      Job.scheduler () >>= fun sr ->
//      let rV = IVar.Now.create ()
//      ThreadPool.QueueUserWorkItem (fun _ ->
//        Scheduler.start sr
//         (try IVar.fill rV (thunk ()) with e -> IVar.fillFailure rV e))
//      |> ignore
//      upcast rV

//  type WaitHandle with
//    member wh.awaitAsJob (timeout: TimeSpan) : Job<bool> =
//      Job.scheduler () >>= fun sr ->
//      let rV = IVar.Now.create ()
//      ThreadPool.RegisterWaitForSingleObject
//       (wh, (fun _ r -> Scheduler.start sr (IVar.fill rV r)),
//        null, timeout, true) |> ignore
//      upcast rV

//    member wh.awaitAsJob: Job<unit> =
//      Job.scheduler () >>= fun sr ->
//      let rV = IVar.Now.create ()
//      ThreadPool.RegisterWaitForSingleObject
//       (wh, (fun _ _ -> Scheduler.start sr (IVar.fill rV ())),
//        null, -1, true) |> ignore
//      upcast rV

  module Async =
    let inline start sr xA (xK: Cont<_>) =
      let success = fun x ->
        xK.Value <- x
        Worker.RunOnThisThread (sr, xK)
      let failure = fun exn ->
        Worker.RunOnThisThread (sr, FailWork (exn, xK))
      Async.StartWithContinuations (xA, success, failure, failure)

    let inline startIn (context: SynchronizationContext) sr xA xK =
      context.Post ((fun _ -> start sr xA xK), null)

    let toJob (xA: Async<'x>) =
      {new Job<'x> () with
        override xJ'.DoJob (wr, xK) =
         match wr.Event with
          | null -> Worker.PushNew (&wr, JobWork<'x> (xJ', xK))
          | _    -> start wr.Scheduler xA xK}

    let toJobOn (context: SynchronizationContext) (xA: Async<'x>) =
      match context with
       | null ->
         toJob xA
       | _ ->
         {new Job<'x> () with
           override xJ'.DoJob (wr, xK) =
            startIn context wr.Scheduler xA xK}

    let toAltOn (context: SynchronizationContext) (xA: Async<'x>) =
      Alt.withNack <| fun nack ->
      {new Job<Alt<'x>> () with
        override xJ'.DoJob (wr, xAK) =
         let sr = wr.Scheduler
         let rI = IVar.Now.create ()
         let ts = new CancellationTokenSource ()
         let success = IVar.fill rI >> Scheduler.start sr
         let failure = IVar.fillFailure rI >> Scheduler.start sr
         let inline start () =
            Async.StartWithContinuations
             (xA, success, failure, failure, ts.Token)
         match context with
          | null ->
            Task.Factory.StartNew (fun _ -> start ()) |> ignore
          | _ -> context.Post ((fun _ -> start ()), null)
         nack.DoJob (&wr, {new Handler<unit, unit> () with
          override uK'.DoWork (_) = ts.Cancel () ; ts.Dispose ()
          override uK'.DoCont (_, _) = ts.Cancel () ; ts.Dispose ()})
         xAK.DoCont (&wr, Alt.tryFinallyFun rI ts.Dispose)}

    let toAlt (xA: Async<'x>) = toAltOn null xA

    let ofJobOn (sr: Scheduler) (xJ: Job<'x>) =
      assert (null <> sr)
      Async.FromContinuations <| fun (x2u, e2u, c2u) ->
        Worker.RunOnThisThread (sr, xJ, {new Cont_State<'x, Cont<unit>>() with
         override xK'.GetProc (wr) = Handler.GetProc (&wr, &xK'.State)
         override xK'.DoHandle (wr, e) = e2u e
         override xK'.DoWork (wr) = x2u xK'.Value
         override xK'.DoCont (wr, x) = x2u x})

    module Global =
      let ofJob (xJ: Job<'x>) =
        ofJobOn (initGlobalScheduler ()) xJ

    type OnWithSchedulerBuilder =
      new (context: SynchronizationContext, scheduler: Scheduler) =
        {Context = context; Scheduler = scheduler}

      val Scheduler: Scheduler
      val Context: SynchronizationContext

      member inline this.Bind (xT: Task<'x>, x2yA: 'x -> Async<'y>) =
        async.Bind (Async.AwaitTask xT, x2yA)
      member inline this.Bind (xJ: Job<'x>, x2yA: 'x -> Async<'y>) =
        async.Bind (ofJobOn this.Scheduler xJ, x2yA)
      member inline this.Bind (xA: Async<'x>, x2yA: 'x -> Async<'y>) =
        async.Bind (xA, x2yA)

      member inline this.Combine (uT: Task<unit>, xA: Async<'x>) =
        async.Combine (Async.AwaitTask uT, xA)
      member inline this.Combine (uJ: Job<unit>, xA: Async<'x>) =
        async.Combine (ofJobOn this.Scheduler uJ, xA)
      member inline this.Combine (uA: Async<unit>, xA: Async<'x>) =
        async.Combine (uA, xA)

      member inline this.Delay (u2xA: unit -> Async<'x>) =
        async.Delay u2xA

      member inline this.For (ts: seq<'x>, x2uA: 'x -> Async<unit>) =
        async.For (ts, x2uA)

      member inline this.Return (t: 'x) = async.Return t

      member inline this.ReturnFrom (xT: Task<'x>) =
        async.ReturnFrom (Async.AwaitTask xT)
      member inline this.ReturnFrom (xJ: Job<'x>) =
        async.ReturnFrom (ofJobOn this.Scheduler xJ)
      member inline this.ReturnFrom (xA: Async<'x>) = async.ReturnFrom xA

      member inline this.TryFinally (xT: Task<'x>, u2u: unit -> unit) =
        async.TryFinally (Async.AwaitTask xT, u2u)
      member inline this.TryFinally (xJ: Job<'x>, u2u: unit -> unit) =
        async.TryFinally (ofJobOn this.Scheduler xJ, u2u)
      member inline this.TryFinally (xA: Async<'x>, u2u: unit -> unit) =
        async.TryFinally (xA, u2u)

      member inline this.TryWith (xT: Task<'x>, e2xA: exn -> Async<'x>) =
        async.TryWith (Async.AwaitTask xT, e2xA)
      member inline this.TryWith (xJ: Job<'x>, e2xA: exn -> Async<'x>) =
        async.TryWith (ofJobOn this.Scheduler xJ, e2xA)
      member inline this.TryWith (xA: Async<'x>, e2xA: exn -> Async<'x>) =
        async.TryWith (xA, e2xA)

      member inline this.Using (x: 'x, x2yA: 'x -> Async<'y>) =
        async.Using (x, x2yA)

      member inline this.While (u2b: unit -> bool, uA: Async<unit>) =
        async.While (u2b, uA)

      member inline this.Zero () = async.Zero ()

      member inline this.Run (xA: Async<'x>) = toJobOn this.Context xA

  let inline asyncOn context scheduler =
    Async.OnWithSchedulerBuilder (context, scheduler)

/////////////////////////////////////////////////////////////////////////

open Extensions
open Job.Infixes

type JobBuilder () =
  member inline job.Bind (xA: Async<'x>, x2yJ: 'x -> Job<'y>) : Job<'y> =
    Async.toJob xA >>= x2yJ
  member inline job.Bind (xT: Task<'x>, x2yJ: 'x -> Job<'y>) : Job<'y> =
    Task.bindJob (xT, x2yJ)
  member inline job.Bind (uT: Task, u2xJ: unit -> Job<'x>) : Job<'x> =
    Task.bindJob (uT, u2xJ)
  member inline job.Bind (xJ: Job<'x>, x2yJ: 'x -> Job<'y>) : Job<'y> =
    xJ >>= x2yJ

  member inline job.Combine (uA: Async<unit>, xJ: Job<'x>) : Job<'x> =
    Async.toJob uA >>. xJ
  member inline job.Combine (uT: Task<unit>, xJ: Job<'x>) : Job<'x> =
    Task.awaitJob uT >>. xJ
  member inline job.Combine (uT: Task, xJ: Job<'x>) : Job<'x> =
    Task.awaitJob uT >>. xJ
  member inline job.Combine (uJ: Job<unit>, xJ: Job<'x>) : Job<'x> = uJ >>. xJ

  member inline job.Delay (u2xJ: unit -> Job<'x>) : Job<'x> = Job.delay u2xJ

  member inline job.For (xs: seq<'x>, x2uJ: 'x -> Job<unit>) : Job<unit> =
    Seq.iterJob x2uJ xs

  member inline job.Return (x: 'x) : Job<'x> = Job.result x

  member inline job.ReturnFrom (xA: Async<'x>) : Job<'x> = Async.toJob xA
  member inline job.ReturnFrom (xT: Task<'x>) : Job<'x> = Task.awaitJob xT
  member inline job.ReturnFrom (uT: Task) : Job<unit> = Task.awaitJob uT
  member inline job.ReturnFrom (xJ: Job<'x>) : Job<'x> = xJ

  member inline job.TryFinally (xA: Async<'x>, u2u: unit -> unit) : Job<'x> =
    Job.tryFinallyFun (Async.toJob xA) u2u
  member inline job.TryFinally (xT: Task<'x>, u2u: unit -> unit) : Job<'x> =
    Job.tryFinallyFun (Task.awaitJob xT) u2u
  member inline job.TryFinally (uT: Task, u2u: unit -> unit) : Job<unit> =
    Job.tryFinallyFun (Task.awaitJob uT) u2u
  member inline job.TryFinally (xJ: Job<'x>, u2u: unit -> unit) : Job<'x> =
    Job.tryFinallyFun xJ u2u

  member inline job.TryWith (xA: Async<'x>, e2xJ: exn -> Job<'x>) : Job<'x> =
    Job.tryWith (Async.toJob xA) e2xJ
  member inline job.TryWith (xT: Task<'x>, e2xJ: exn -> Job<'x>) : Job<'x> =
    Job.tryWith (Task.awaitJob xT) e2xJ
  member inline job.TryWith (uT: Task, e2xJ: exn -> Job<unit>) : Job<unit> =
    Job.tryWith (Task.awaitJob uT) e2xJ
  member inline job.TryWith (xJ: Job<'x>, e2xJ: exn -> Job<'x>) : Job<'x> =
    Job.tryWith xJ e2xJ

  member inline job.Using (x: 'x, x2yJ: 'x -> Job<'y>) : Job<'y>
      when 'x :> IDisposable =
    Job.using x x2yJ

  member inline job.While (u2b: unit -> bool, uJ: Job<unit>) : Job<unit> =
    Job.whileDo u2b uJ

  member inline job.Zero () : Job<unit> = StaticData.unit :> Job<unit>

type EmbeddedJob<'x> = struct
    val Job: Job<'x>
    new (job) = {Job = job}
  end

type EmbeddedJobBuilder () =
  inherit JobBuilder ()
  member this.Run (xJ: Job<'x>) : EmbeddedJob<'x> = EmbeddedJob<'x> (xJ)

[<AutoOpen>]
module TopLevel =
  let job = JobBuilder ()

  let inline run x = Job.Global.run x
  let inline start x = Job.Global.start x
  let inline server x = Job.Global.server x

  let inline asAlt (xA: Alt<'x>) = xA
  let inline asJob (xJ: Job<'x>) = xJ

  let inline ch () = Ch<'x> ()
  let inline mb () = Mailbox<'x> ()
  let inline ivar () = IVar<'x> ()
  let inline ivarFull x = IVar.Now.createFull x
  let inline mvar () = MVar<'x> ()
  let inline mvarFull x = MVar.Now.createFull x

/////////////////////////////////////////////////////////////////////////

module Infixes =
  let inline (<--) (xCh: Ch<'x>) (x: 'x) = ChGive<'x> (xCh, x) :> Alt<unit>
  let inline (<-+) (xCh: Ch<'x>) (x: 'x) = ChSend<'x> (xCh, x) :> Job<unit>
  let inline (<-=) (xI: IVar<'x>) (x: 'x) = IVar<'x>.Fill (xI, x) :> Job<unit>
  let inline (<-=!) (xI: IVar<'x>) (e: exn) = IVar<'x>.FillFailure (xI, e) :> Job<unit>
  let inline (<<-=) (xM: MVar<'x>) (x: 'x) = MVarFill<'x> (xM, x) :> Job<unit>
  let inline (<<-+) (xMb: Mailbox<'x>) (x: 'x) = MailboxSend<'x> (xMb, x) :> Job<unit>

/////////////////////////////////////////////////////////////////////////

module Latch =
  module Now =
    let inline create initial = Latch (initial)
    let inline increment (l: Latch) = l.Increment ()
  let inline decrement (l: Latch) = l.Decrement ()
  let inline await (l: Latch) = l :> Alt<_>
  let within (l2xJ: Latch -> #Job<'x>) = Job.delay <| fun () ->
    let l = Now.create 1
    Job.tryFinallyJob
      (Job.delayWith l2xJ l)
      (decrement l >>. l)
  let holding (l: Latch) (xJ: Job<'x>) = Job.delay <| fun () ->
    Now.increment l
    Job.tryFinallyJob xJ (decrement l)
  let queue (l: Latch) (xJ: Job<'x>) = Job.delay <| fun () ->
    Now.increment l
    Job.queue (Job.tryFinallyJob xJ (decrement l))
  let queueAsPromise (l: Latch) (xJ: Job<'x>) = Job.delay <| fun () ->
    Now.increment l
    Promise.queue (Job.tryFinallyJob xJ (decrement l))
