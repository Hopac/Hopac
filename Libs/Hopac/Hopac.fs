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

type IAsyncDisposable =
  abstract DisposeAsync: unit -> Job<unit>

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
      xK'.uJ.DoJob (&wr, FailCont (xK, e))
    override xK'.DoWork (wr) =
      let xK = xK'.xK
      wr.Handler <- xK
      xK.Value <- xK'.Value
      xK'.uJ.DoJob (&wr, DropCont (xK))
    override xK'.DoCont (wr, x) =
      let xK = xK'.xK
      wr.Handler <- xK
      xK.Value <- x
      xK'.uJ.DoJob (&wr, DropCont (xK))

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

  type [<AbstractClass>] ContQueue<'x> () =
    inherit Cont<'x> ()
    override xK'.GetProc (_) = failwith "Bug"
    override xK'.DoHandle (wr, e) = Handler.DoHandleNull (&wr, e)

  type [<AbstractClass>] WorkQueue () =
    inherit Work ()
    override w'.GetProc (_) = failwith "Bug"
    override w'.DoHandle (wr, e) = Handler.DoHandleNull (&wr, e)

  type Handler<'x, 'y> =
    inherit Cont<'x>
    val mutable yK: Cont<'y>
    new () = {inherit Cont<'x> (); yK = null}
    new (yK) = {inherit Cont<'x> (); yK = yK}
    override xK'.GetProc (wr) = Handler.GetProc (&wr, &xK'.yK)
    override xK'.DoHandle (wr, e) = Handler.DoHandle (xK'.yK, &wr, e)
    override xK'.DoWork (wr) = Handler.Terminate (&wr, xK'.yK)
    override xK'.DoCont (wr, _) = Handler.Terminate (&wr, xK'.yK)

  type Raises<'x> (e) =
    inherit Alt<'x> ()
    override xA'.DoJob (wr, xK) = Handler.DoHandle (xK, &wr, e)
    override xA'.TryAlt (wr, i, xK, xE) =
      if 0 = Pick.PickAndSetNacks (xE.pk, &wr, i) then
        Handler.DoHandle (xK, &wr, e)

/////////////////////////////////////////////////////////////////////////

module IVar =
  module Now =
    let inline create () = IVar<'x> ()
    let inline createFull (x: 'x) = IVar<'x> (x)
    let inline createFailure (e: exn) = IVar<'x> (e)
    [<MethodImpl(MethodImplOptions.NoInlining)>]
    let isFull (xI: IVar<'x>) = xI.Full
    [<MethodImpl(MethodImplOptions.NoInlining)>]
    let get (xI: IVar<'x>) : 'x = xI.Get ()

  let create () = ctor Now.create ()
  let inline fill (xI: IVar<'x>) (x: 'x) = IVar<'x>.Fill (xI, x) :> Job<unit>
  let inline tryFill (xI: IVar<'x>) (x: 'x) = IVar<'x>.TryFill (xI, x) :> Job<unit>
  let inline fillFailure (xI: IVar<'x>) (e: exn) =
    IVar<'x>.FillFailure (xI, e) :> Job<unit>
  let inline read (xI: IVar<'x>) = xI :> Alt<'x>

/////////////////////////////////////////////////////////////////////////

module Alt =
  let inline always (x: 'x) = Always<'x> (x) :> Alt<'x>

  let inline unit () =
    match StaticData.unit with
     | null -> StaticData.Init () ; StaticData.unit
     | unit -> unit

  let inline never () = Never<_>() :> Alt<_>

  let inline zero () =
    match StaticData.zero with
     | null -> StaticData.Init () ; StaticData.zero :> Alt<_>
     | zero -> zero :> Alt<_>

  let inline once x = Once<'x> (x) :> Alt<'x>

  let raises e = Raises (e) :> Alt<_>

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
    let xK = xAK'.xK
    wr.Handler <- xK ; Handler.DoHandle (xK, &wr, e)
   override xAK'.DoWork (wr) =
    let xE = xAK'.xE
    Pick.Unclaim xE.pk
    let xK = xAK'.xK
    wr.Handler <- xK ; xAK'.Value.TryAlt (&wr, xAK'.i, xK, xE)
   override xAK'.DoCont (wr, xA) =
    let xE = xAK'.xE
    Pick.Unclaim xE.pk
    let xK = xAK'.xK
    wr.Handler <- xK ; xA.TryAlt (&wr, xAK'.i, xK, xE)

  let guard (xAJ: Job<#Alt<'x>>) =
    {new Alt<'x> () with
      override xA'.DoJob (wr, xK) =
       xAJ.DoJob (&wr, GuardJobCont xK)
      override xA'.TryAlt (wr, i, xK, xE) =
       Pick.ClaimAndDoJob (xE.pk, &wr, xAJ, GuardCont (i, xK, xE))}

  let inline delay (u2xA: unit -> #Alt<'x>) =
    {new AltDelay<'x> () with
      override xA'.Do () = upcast u2xA ()} :> Alt<_>

  let inline random (u2xA: uint64 -> #Alt<'x>) =
    {new AltRandom<'x> () with
      override xA'.Do (random) = upcast u2xA random} :> Alt<_>

  let inline withNackJob (nack2xAJ: Promise<unit> -> #Job<#Alt<'x>>) =
    {new AltWithNackJob<_, _> () with
      override xA'.Do (nack) = upcast nack2xAJ nack} :> Alt<_>

  let inline withNack (nack2xAJ: Promise<unit> -> #Job<#Alt<'x>>) =
    withNackJob nack2xAJ

  let inline withNackFun (nack2xA: Promise<unit> -> #Alt<'x>) =
    {new AltWithNackFun<_> () with
      override xA'.Do (nack) = upcast nack2xA nack} :> Alt<_>

  let wrapAbortJob (uJ: Job<unit>) (xA: Alt<'x>) : Alt<'x> =
    {new Alt<'x> () with
      override xA'.DoJob (wr, xK) =
       xA.DoJob (&wr, xK)
      override xA'.TryAlt (wr, i, xK, xE) =
       let uK' = {new ContQueue<_> () with
        override uK'.DoWork (wr) =
         let handler = Handler<_, _>()
         wr.Handler <- handler
         uJ.DoJob (&wr, handler)
        override uK'.DoCont (wr, _) = uK'.DoWork (&wr)}
       let pk = xE.pk
       match Pick.ClaimAndAddNack (pk, i) with
        | null -> ()
        | nk ->
          nk.UnsafeAddReader uK'
          Pick.Unclaim pk
          xA.TryAlt (&wr, i, xK, WithNackElse (nk, xE))}

  let wrapAbort (uJ: Job<unit>) (xA: Alt<'x>) : Alt<'x> =
    wrapAbortJob uJ xA

  let wrapAbortFun (u2u: unit -> unit) (xA: Alt<'x>) : Alt<'x> =
    {new Alt<'x> () with
      override xA'.DoJob (wr, xK) =
       xA.DoJob (&wr, xK)
      override xA'.TryAlt (wr, i, xK, xE) =
       let uK' = {new ContQueue<_> () with
        override uK'.DoWork (wr) = u2u ()
        override uK'.DoCont (wr, _) = u2u ()}
       let pk = xE.pk
       match Pick.ClaimAndAddNack (pk, i) with
        | null -> ()
        | nk ->
          nk.UnsafeAddReader uK'
          Pick.Unclaim pk
          xA.TryAlt (&wr, i, xK, WithNackElse (nk, xE))}

  let inline map (x2y: 'x -> 'y) (xA: Alt<'x>) =
    {new AltMap<'x, 'y> () with
      override yA'.Do (x) = x2y x}.InternalInit(xA)

  let inline wrap (x2yJ: 'x -> #Job<'y>) (xA: Alt<'x>) =
    {new AltBind<'x, 'y> () with
      override yA'.Do (x) = upcast x2yJ x}.InternalInit(xA)

  module Infixes =
    let inline either (wr: byref<Worker>) xK (xA1: Alt<_>) (xA2: Alt<_>) =
      xA1.TryAlt (&wr, 0, xK, {new Pick_State<Alt<'x>> () with
       override xE'.TryElse (wr, i) =
        match xE'.State1 with
         | null -> ()
         | xA2 ->
           xE'.State1 <- null
           xA2.TryAlt (&wr, i, xK, xE')}.Init(xA2))

    let inline eitherOr (wr: byref<Worker>) i xK (xE: Else) (xA1: Alt<_>) (xA2: Alt<_>) =
      xA1.TryAlt (&wr, i, xK, {new Else () with
       override xE'.TryElse (wr, i) =
        xA2.TryAlt (&wr, i, xK, xE)}.Init(xE.pk))

    let (<|>?) (xA1: Alt<'x>) (xA2: Alt<'x>) =
      {new Alt<'x> () with
        override xA'.DoJob (wr, xK) = either &wr xK xA1 xA2
        override xA'.TryAlt (wr, i, xK, xE) = eitherOr &wr i xK xE xA1 xA2}

    let (<~>?) (xA1: Alt<'x>) (xA2: Alt<'x>) =
      {new Alt<'x> () with
        override xA'.DoJob (wr, xK) =
         if int (Randomizer.Next (&wr.RandomLo, &wr.RandomHi)) < 0
         then either &wr xK xA1 xA2
         else either &wr xK xA2 xA1
        override xA'.TryAlt (wr, i, xK, xE) =
         if int (Randomizer.Next (&wr.RandomLo, &wr.RandomHi)) < 0
         then eitherOr &wr i xK xE xA1 xA2
         else eitherOr &wr i xK xE xA2 xA1}

    let inline (|>>?) (xA: Alt<'x>) (x2y: 'x -> 'y) =
      {new AltMap<'x, 'y> () with
        override yA'.Do (x) = x2y x}.InternalInit(xA)

    let inline (>>=?) (xA: Alt<'x>) (x2yJ: 'x -> #Job<'y>) =
      {new AltBind<'x, 'y> () with
        override yA'.Do (x) = upcast x2yJ x}.InternalInit(xA)

    let (>>%?) (xA: Alt<_>) (y: 'y) =
      {new Alt<'y> () with
        override yA'.DoJob (wr, yK) =
         yK.Value <- y
         xA.DoJob (&wr, DropCont yK)
        override yA'.TryAlt (wr, i, yK, yE) =
         xA.TryAlt (&wr, i, ValueCont (y, yK), yE)}

    let (>>!?) (xA: Alt<_>) (e: exn) =
      {new Alt<'y> () with
        override yA'.DoJob (wr, yK) =
         xA.DoJob (&wr, FailCont (yK, e))
        override yA'.TryAlt (wr, i, yK, yE) =
         xA.TryAlt (&wr, i, FailCont (yK, e), yE)}

    let (>>.?) (xA: Alt<_>) (yJ: Job<'y>) =
      {new Alt<'y> () with
        override yA'.DoJob (wr, yK) =
         xA.DoJob (&wr, SeqCont (yJ, yK))
        override yA'.TryAlt (wr, i, yK, yE) =
         xA.TryAlt (&wr, i, SeqCont (yJ, yK), yE)}

    let (.>>?) (xA: Alt<'x>) (yJ: Job<_>) =
      {new Alt<'x> () with
        override xA'.DoJob (wr, xK) =
         xA.DoJob (&wr, SkipCont (xK, yJ))
        override xA'.TryAlt (wr, i, xK, xE) =
         xA.TryAlt (&wr, i, SkipCont (xK, yJ), xE)}

    let (<+>?) (xA: Alt<'x>) (yA: Alt<'y>) : Alt<'x * 'y> =
      (xA >>=? fun x -> yA |>>? fun y -> (x, y)) <|>?
      (yA >>=? fun y -> xA |>>? fun x -> (x, y))

  let Ignore (xA: Alt<_>) =
    {new Alt<unit> () with
      override uA'.DoJob (wr, uK) =
       xA.DoJob (&wr, DropCont uK)
      override uA'.TryAlt (wr, i, uK, uE) =
       xA.TryAlt (&wr, i, DropCont uK, uE)}

  let choose (xAs: seq<#Alt<'x>>) =
    {new Alt<'x> () with
      override xA'.DoJob (wr, xK) =
       let xAs = xAs.GetEnumerator ()
       if xAs.MoveNext () then
         xAs.Current.TryAlt (&wr, 0, xK, {new Pick_State<IEnumerator<#Alt<_>>> () with
          override xE'.TryElse (wr, i) =
           let xAs = xE'.State1
           if xAs.MoveNext () then
             xAs.Current.TryAlt (&wr, i, xK, xE')
           else
             xAs.Dispose ()
             xE'.State1 <- null}.Init(xAs))
       else
         xAs.Dispose ()
      override xA'.TryAlt (wr, i, xK, xE) =
       let xAs = xAs.GetEnumerator ()
       if xAs.MoveNext () then
         xAs.Current.TryAlt (&wr, i, xK, {new Else () with
          override xE'.TryElse (wr, i) =
           if xAs.MoveNext () then
             xAs.Current.TryAlt (&wr, i, xK, xE')
           else
             xAs.Dispose ()
             xE.TryElse (&wr, i)}.Init(xE.pk))
       else
         xAs.Dispose ()
         xE.TryElse (&wr, i)}

  let choosy (xAs: array<#Alt<'x>>) =
    if 0 < xAs.Length then
      {new Alt<'x> () with
        override xA'.DoJob (wr, xK) =
         xAs.[0].TryAlt (&wr, 0, xK, {new Pick_State<int> () with
          override xE'.TryElse (wr, i) =
           let j = xE'.State1 + 1
           if j < xAs.Length  then
             xE'.State1 <- j
             xAs.[j].TryAlt (&wr, i, xK, xE')}.Init())
        override xA'.TryAlt (wr, i, xK, xE) =
         xAs.[0].TryAlt (&wr, i, xK, {new Else_State<int> () with
          override xE'.TryElse (wr, i) =
           let j = xE'.State1 + 1
           if j < xAs.Length then
             xE'.State1 <- j
             xAs.[j].TryAlt (&wr, i, xK, xE')
           else
             xE.TryElse (&wr, i)}.Init(xE.pk))}
    else
      never ()

  let inline shuffle (wr: byref<Worker>) (xAs: array<_>) j =
    let j' = Randomizer.NextInRange (&wr.RandomLo, &wr.RandomHi, j, xAs.Length)
    let xA = xAs.[j']
    xAs.[j'] <- xAs.[j]
    xA

  let chooser (xAs: seq<#Alt<'x>>) =
    {new Alt<'x> () with
      override xA'.DoJob (wr, xK) =
       let xAs = Seq.toArray xAs
       if 0 < xAs.Length then
         (shuffle &wr xAs 0).TryAlt (&wr, 0, xK, {new Pick_State<_, array<#Alt<_>>> () with
          override xE'.TryElse (wr, i) =
           let j = xE'.State1
           let xAs = xE'.State2
           if j < xAs.Length then
             xE'.State <- j+1
             (shuffle &wr xAs j).TryAlt (&wr, i, xK, xE')
           else
             xE'.State2 <- null}.Init(1, xAs))
      override xA'.TryAlt (wr, i, xK, xE) =
       let xAs = Seq.toArray xAs
       if 0 < xAs.Length then
         (shuffle &wr xAs 0).TryAlt (&wr, i, xK, {new Else_State<_> () with
           override xE'.TryElse (wr, i) =
            let j = xE'.State1
            if j < xAs.Length then
              xE'.State1 <- j+1
              (shuffle &wr xAs j).TryAlt (&wr, i, xK, xE')
            else
              xE.TryElse (&wr, i)}.Init(xE.pk, 1))
       else
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
       xA.TryAlt (&wr, i, xK, {new Else () with
        override xE'.TryElse (wr, i) =
         wr.Handler <- yK
         yE.TryElse (&wr, i)}.Init(yE.pk))}

  let tryFinallyFun (xA: Alt<'x>) (u2u: unit -> unit) =
    {new Alt<'x> () with
      override xA'.DoJob (wr, xK) =
       let xK' = TryFinallyFunCont (u2u, xK)
       wr.Handler <- xK'
       xA.DoJob (&wr, xK')
      override xA'.TryAlt (wr, i, xK, xE) =
       let xK' = TryFinallyFunCont (u2u, xK)
       wr.Handler <- xK'
       xA.TryAlt (&wr, i, xK', {new Else () with
        override xE'.TryElse (wr, i) =
         wr.Handler <- xK
         xE.TryElse (&wr, i)}.Init(xE.pk))}

  let tryFinallyJob (xA: Alt<'x>) (uJ: Job<unit>) =
    {new Alt<'x> () with
      override xA'.DoJob (wr, xK) =
       let xK' = TryFinallyJobCont (uJ, xK)
       wr.Handler <- xK'
       xA.DoJob (&wr, xK')
      override xA'.TryAlt (wr, i, xK, xE) =
       let xK' = TryFinallyJobCont (uJ, xK)
       wr.Handler <- xK'
       xA.TryAlt (&wr, i, xK', {new Else () with
        override xE'.TryElse (wr, i) =
         wr.Handler <- xK
         xE.TryElse (&wr, i)}.Init(xE.pk))}

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
    let create =
      match StaticData.createScheduler with
       | null -> StaticData.Init () ; StaticData.createScheduler
       | create -> create
    create.Invoke
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

  let startIgnore (sr: Scheduler) (xJ: Job<'x>) =
    Worker.RunOnThisThread (sr, xJ, Handler<'x, unit> ())

  let inline start (sr: Scheduler) (uJ: Job<unit>) =
    startIgnore sr uJ

  let queueIgnore (sr: Scheduler) (xJ: Job<'x>) =
    Scheduler.PushNew (sr, {new WorkQueue () with
     override w'.DoWork (wr) =
      let handler = Handler<_, _> ()
      wr.Handler <- handler
      xJ.DoJob (&wr, handler)})

  let inline queue sr (uJ: Job<unit>) =
    queueIgnore sr uJ

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
    StaticData.Init ()
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
      Ch<'x>.Send (initGlobalScheduler (), xCh, x)
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
      Mailbox<'x>.Send (initGlobalScheduler (), xMb, x)
  let create () = ctor Now.create ()
  let inline send (xMb: Mailbox<'x>) (x: 'x) =
    MailboxSend<'x> (xMb, x) :> Job<unit>
  let inline take (xMb: Mailbox<'x>) = xMb :> Alt<'x>

/////////////////////////////////////////////////////////////////////////

module Job =
  module Global =
    let startWithActions eF xF xJ =
      Scheduler.startWithActions (initGlobalScheduler ()) eF xF xJ
    let startIgnore xJ = Scheduler.startIgnore (initGlobalScheduler ()) xJ
    let inline start (uJ: Job<unit>) = startIgnore uJ
    let queueIgnore xJ = Scheduler.queueIgnore (initGlobalScheduler ()) xJ
    let inline queue (uJ: Job<unit>) = queueIgnore uJ
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

  let inline lift (x2y: 'x -> 'y) (x: 'x) =
    {new JobThunk<'y> () with
      override yJ'.Do () = x2y x} :> Job<_>

  let inline thunk (u2x: unit -> 'x) =
    {new JobThunk<'x> () with
      override xJ'.Do () = u2x ()} :> Job<_>

  let Ignore (xJ: Job<_>) =
    {new Job<unit>() with
      override uJ.DoJob (wr, uK) =
       xJ.DoJob (&wr, DropCont (uK))}

  let forNIgnore (n: int) (xJ: Job<_>) =
    {new Job<unit> () with
      override uJ'.DoJob (wr, uK) =
       {new Cont_State<_, _> () with
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
            uK.DoWork (&wr)}.Init(n).DoWork (&wr)}
  let inline forN n (uJ: Job<unit>) =
    forNIgnore n uJ

  let inline forUpToIgnore (i0: int) (i1: int) (i2xJ: int -> #Job<_>) =
    {new JobFor<_, _, _> () with
      override uJ'.Init (i) =
        let i' = i0 in if i' <= i1 then i <- i' + 1; upcast i2xJ i' else null
      override uJ'.Next (_, i) =
        let i' = i  in if i' <= i1 then i <- i' + 1; upcast i2xJ i' else null} :> Job<unit>
  let inline forUpTo i0 i1 (i2uJ: int -> #Job<unit>) =
    forUpToIgnore i0 i1 i2uJ

  let inline forDownToIgnore (i0: int) (i1: int) (i2xJ: int -> #Job<_>) =
    {new JobFor<_, _, _> () with
      override uJ.Init (i) =
        let i' = i0 in if i' >= i1 then i <- i' - 1; upcast i2xJ i0 else null
      override uJ.Next (_, i) =
        let i' = i  in if i' >= i1 then i <- i' - 1; upcast i2xJ i' else null} :> Job<unit>
  let inline forDownTo i0 i1 (i2uJ: int -> #Job<unit>) =
    forDownToIgnore i0 i1 i2uJ

  type ForeverCont<'x, 'y> =
    inherit Handler<'x, 'y>
    val mutable xJ: Job<'x>
    new (xJ) = {inherit Handler<'x, 'y> (); xJ = xJ}
    new (xJ, yK) = {inherit Handler<'x, 'y> (yK); xJ = xJ}
    override xK'.DoWork (wr) = xK'.xJ.DoJob (&wr, xK')
    override xK'.DoCont (wr, _) = xK'.xJ.DoJob (&wr, xK')

  let foreverIgnore (xJ: Job<_>) =
    {new Job<'y> () with
      override yJ'.DoJob (wr, yK_) = ForeverCont<_, _>(xJ, yK_).DoWork (&wr)}
  let inline forever (uJ: Job<unit>) =
    foreverIgnore uJ

  let inline iterate (x: 'x) (x2xJ: 'x -> #Job<'x>) =
    {new JobRun<'y> () with
      override yJ'.Do (yK) =
        {new ContIterate<'x, 'y> () with
          override xK'.Do (x) = upcast x2xJ x}.InternalInit (x, yK) :> Work} :> Job<_>

  let inline whileDoDelay (cond: unit -> bool) (u2xJ: unit -> #Job<'x>) =
    {new JobWhileDoDelay<'x> () with
      override uJ'.Do () =
        if cond () then upcast u2xJ () else null} :> Job<_>
  let inline whileDoIgnore (cond: unit -> bool) (xJ: Job<_>) =
    whileDoDelay cond (fun () -> xJ)
  let inline whileDo cond (uJ: Job<unit>) =
    whileDoIgnore cond uJ

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
    {new JobBind<'x, 'y> () with
      override yJ'.Do (x) = upcast x2yJ x}.InternalInit(xJ)

  let inline join (xJJ: Job<#Job<'x>>) = JobJoin<_, _>().InternalInit(xJJ)

  let inline map (x2y: 'x -> 'y) (xJ: Job<'x>) =
    {new JobMap<'x, 'y> () with
      override yJ'.Do (x) = x2y x}.InternalInit(xJ)

  let inline unit () = Alt.unit () :> Job<_>

  let abort () = Never<_>() :> Job<_>

  let raises (e: exn) = Raises (e) :> Job<_>

  let inline whenDo (b: bool) (uJ: Job<unit>) =
    if b then uJ else unit ()

  ///////////////////////////////////////////////////////////////////////

  module Random =
    let inline bind (u2xJ: uint64 -> #Job<'x>) =
      {new JobRandomBind<_> () with
        override xJ'.Do (random) = upcast u2xJ random} :> Job<'x>

    let inline map (u2x: uint64 -> 'x) =
      {new JobRandomMap<_> () with
        override xJ'.Do (random) = u2x random} :> Job<'x>

    let inline get () =
      match StaticData.random with
       | null -> StaticData.Init () ; StaticData.random
       | random -> random

  ///////////////////////////////////////////////////////////////////////

  module Infixes =
    let inline (>>=) (xJ: Job<'x>) (x2yJ: 'x -> #Job<'y>) =
      {new JobBind<'x, 'y> () with
        override yJ'.Do (x) = upcast x2yJ x}.InternalInit(xJ)

    let inline (>=>) (x2yJ: 'x -> #Job<'y>) (y2zJ: 'y -> #Job<'z>) (x: 'x) : Job<'z> =
      x2yJ x >>= y2zJ

    let (>>.) (xJ: Job<_>) (yJ: Job<'y>) =
      {new Job<'y> () with
        override yJ'.DoJob (wr, yK) =
         xJ.DoJob (&wr, SeqCont (yJ, yK))}

    let (.>>) (xJ: Job<'x>) (yJ: Job<_>) =
      {new Job<'x> () with
        override xJ'.DoJob (wr, xK) =
         xJ.DoJob (&wr, SkipCont (xK, yJ))}

    let inline (|>>) (xJ: Job<'x>) (x2y: 'x -> 'y) =
      {new JobMap<'x, 'y> () with
        override yJ'.Do (x) = x2y x}.InternalInit(xJ)

    let (>>%) (xJ: Job<_>) (y: 'y) =
      {new Job<'y> () with
        override yJ'.DoJob (wr, yK) =
         yK.Value <- y
         xJ.DoJob (&wr, DropCont yK)}

    let (>>!) (xJ: Job<_>) (e: exn) =
      {new Job<'y> () with
        override yJ'.DoJob (wr, yK_) =
         xJ.DoJob (&wr, {new Cont_State<_, _> () with
          override xK'.GetProc (wr) = Handler.GetProc (&wr, &xK'.State)
          override xK'.DoHandle (wr, e) = Handler.DoHandle (xK'.State, &wr, e)
          override xK'.DoWork (wr) = Handler.DoHandle (xK'.State, &wr, e)
          override xK'.DoCont (wr, _) = Handler.DoHandle (xK'.State, &wr, e)}.Init(yK_))}

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
         Worker.PushNew (&wr, {new Cont_State<_, Job<'x>> () with
          override xK'.GetProc (wr) = yK'.GetProc (&wr)
          override xK'.DoHandle (wr, e) = yK'.DoHandle (&wr, e)
          override xK'.DoCont (wr, a) = yK'.DoOtherCont (&wr, a)
          override xK'.DoWork (wr) =
           match xK'.State with
            | null -> yK'.DoOtherCont (&wr, xK'.Value)
            | xJ ->
              xK'.State <- null
              xJ.DoJob (&wr, xK')}.Init(xJ))
         yJ.DoJob (&wr, yK')}

  ///////////////////////////////////////////////////////////////////////

  let inline tryIn (xJ: Job<'x>) (x2yJ: 'x -> #Job<'y>) (e2yJ: exn -> #Job<'y>) =
    {new JobTryIn<'x, 'y> () with
      override yJ'.DoCont () = {new JobTryInCont<_, _> () with
        override yJ'.DoIn (x) = upcast x2yJ x
        override yJ'.DoExn (e) = upcast e2yJ e}}.InternalInit(xJ)

  let inline tryInDelay (u2xJ: unit -> #Job<'x>) (x2yJ: 'x -> #Job<'y>) (e2yJ: exn -> #Job<'y>) =
    {new JobTryInDelay<'x, 'y> () with
      override yJ'.DoDelay () = upcast u2xJ ()
      override yJ'.DoCont () = {new JobTryInCont<_, _> () with
       override yJ'.DoIn (x) = upcast x2yJ x
       override yJ'.DoExn (e) = upcast e2yJ e}} :> Job<_>

  let inline tryWith (xJ: Job<'x>) (e2xJ: exn -> #Job<'x>) =
    {new JobTryWith<'x> () with
      override xJ'.DoCont() = {new ContTryWith<'x> () with
       override xK'.DoExn (e) = upcast e2xJ e}}.InternalInit(xJ)

  let inline tryWithDelay (u2xJ: unit -> #Job<'x>) (e2xJ: exn -> #Job<'x>) =
    {new JobTryWithDelay<'x> () with
      override xJ'.Do () = upcast u2xJ ()
      override xJ'.DoCont() = {new ContTryWith<'x> () with
       override xK'.DoExn (e) = upcast e2xJ e}} :> Job<_>

  let tryFinallyFun (xJ: Job<'x>) (u2u: unit -> unit) =
    {new Job<'x> () with
      override xJ'.DoJob (wr, xK_) =
       let xK' = TryFinallyFunCont (u2u, xK_)
       wr.Handler <- xK'
       xJ.DoJob (&wr, xK')}

  let tryFinallyFunDelay (u2xJ: unit -> #Job<'x>) (u2u: unit -> unit) =
    {new Job<'x> () with
      override xJ'.DoJob (wr, xK_) =
       let xK' = TryFinallyFunCont (u2u, xK_)
       wr.Handler <- xK'
       u2xJ().DoJob (&wr, xK')}

  let tryFinallyJob (xJ: Job<'x>) (uJ: Job<unit>) =
    {new Job<'x> () with
      override xJ'.DoJob (wr, xK_) =
       let xK' = TryFinallyJobCont (uJ, xK_)
       wr.Handler <- xK'
       xJ.DoJob (&wr, xK')}

  let tryFinallyJobDelay (u2xJ: unit -> #Job<'x>) (uJ: Job<unit>) =
    {new Job<'x> () with
      override xJ'.DoJob (wr, xK_) =
       let xK' = TryFinallyJobCont (uJ, xK_)
       wr.Handler <- xK'
       u2xJ().DoJob (&wr, xK')}

  let usingAsync (x: 'x when 'x :> IAsyncDisposable) (x2yJ: 'x -> #Job<'y>) =
    {new Job<'y> () with
      override yJ'.DoJob (wr, yK_) =
       let yK' = TryFinallyJobCont (x.DisposeAsync (), yK_)
       wr.Handler <- yK'
       (x2yJ x).DoJob (&wr, yK')}

  let using (x: 'x when 'x :> IDisposable) (x2yJ: 'x -> #Job<'y>) =
    // REMINDER: Dispose() of managed resources is an optimization.  Do not
    // implement Finalize() for managed resources.  See:
    //   http://joeduffyblog.com/2005/04/08/dg-update-dispose-finalization-and-resource-management/
    {new Job<'y> () with
      override yJ'.DoJob (wr, yK_) =
       let yK' = {new Cont_State<'y, _> () with
        override yK'.GetProc (wr) = Handler.GetProc (&wr, &yK'.State)
        override yK'.DoHandle (wr, e) =
         let yK = yK'.State
         wr.Handler <- yK ; x.Dispose () ; Handler.DoHandle (yK, &wr, e)
        override yK'.DoWork (wr) =
         let yK = yK'.State
         wr.Handler <- yK ; x.Dispose () ; yK.DoCont (&wr, yK'.Value)
        override yK'.DoCont (wr, y) =
         let yK = yK'.State
         wr.Handler <- yK ; x.Dispose () ; yK.DoCont (&wr, y)}.Init(yK_)
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

  let startIgnore (xJ: Job<'x>) =
    {new Job<unit> () with
      override uJ'.DoJob (wr, uK) =
       Worker.Push (&wr, uK)
       Job.Do (xJ, &wr, Handler<_, _> ())}
  let inline start (uJ: Job<unit>) =
    startIgnore uJ

  let queueIgnore (xJ: Job<'x>) =
    {new Job<unit> () with
      override uJ'.DoJob (wr, uK) =
       Worker.PushNew (&wr, {new WorkQueue () with
        override w'.DoWork (wr) =
         let handler = Handler<_, _> ()
         wr.Handler <- handler
         xJ.DoJob (&wr, handler)})
       Work.Do (uK, &wr)}
  let inline queue (uJ: Job<unit>) =
    queueIgnore uJ

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
      | _ -> Worker.RunOnThisThread (sr, xK')
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
     Handler.DoHandleNull (&wr, e)
    override xK'.DoWork (wr) = xK'.Term (&wr)
    override xK'.DoCont (wr, _) = xK'.Term (&wr)

  let startWithFinalizerIgnore (fJ: Job<unit>) (xJ: Job<_>) =
    {new Job<unit> () with
      override uJ'.DoJob (wr, uK) =
       Worker.Push (&wr, uK)
       Job.Do (xJ, &wr, Finalizer<_> (wr.Scheduler, fJ))}
  let inline startWithFinalizer fJ (uJ: Job<unit>) =
    startWithFinalizerIgnore fJ uJ

  ///////////////////////////////////////////////////////////////////////

  let foreverServer (xJ: Job<unit>) =
    {new Job<unit> () with
      override uJ'.DoJob (wr, uK) =
       Worker.PushNew (&wr, ForeverCont<_, _> (xJ))
       Work.Do (uK, &wr)}

  let inline iterateServer (x: 'x) (x2xJ: 'x -> #Job<'x>) =
    {new JobStart () with
      override uJ'.Do () =
       {new ContIterate<'x, unit> () with
         override xK'.Do (x) = upcast x2xJ x}.InternalInit(x, null) :> Work} :> Job<_>

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

  let seqIgnore (xJs: seq<#Job<_>>) =
    {new Job<unit> () with
      override self.DoJob (wr, uK) =
       let xJs = xJs.GetEnumerator ()
       if xJs.MoveNext () then
         let xK' = {new Cont<_> () with
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

  type [<AbstractClass>] ConCollect<'x, 'y> () =
    inherit Work ()
    [<DefaultValue>] val mutable Lock: WorkQueueLock
    [<DefaultValue>] val mutable N: int
    [<DefaultValue>] val mutable Exns: ResizeArray<exn>
    [<DefaultValue>] val mutable xs: IEnumerator<'x>
    [<DefaultValue>] val mutable ysK: Cont<ResizeArray<'y>>
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
    member this.Init (xs, ysK) = this.xs <- xs; this.ysK <- ysK ; this

  let conCollect (xJs: seq<#Job<'x>>) =
    {new Job<ResizeArray<'x>> () with
      override xsJ'.DoJob (wr, xsK_) =
       xsK_.Value <- ResizeArray<_> ()
       let cc' = {new ConCollect<_, 'x> () with
         override cc'.DoWork (wr) =
          let mutable nth = 0
          let xs = cc'.xs
          while xs.MoveNext () do
            let xJ = xs.Current :> Job<_>
            cc'.Lock.ExitAndEnter (&wr, cc')
            cc'.ysK.Value.Add Unchecked.defaultof<_>
            let i = Util.dec &nth
            Worker.PushNew (&wr, {new Cont_State<_, Job<_>, _> () with
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
                 xJ.DoJob (&wr, xK')}.Init(xJ, i))
          xs.Dispose ()
          cc'.xs <- null
          ConCollect<_, 'x>.Done (cc', &wr)}.Init(xJs.GetEnumerator (), xsK_)
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

  let conIgnore (xJs: seq<#Job<_>>) =
    {new Job<unit> () with
      override uJ.DoJob (wr, uK) =
       let xJs = xJs.GetEnumerator ()
       let join = ConIgnore (xJs, uK)
       wr.Handler <- join
       while xJs.MoveNext () do
         let xJ = xJs.Current :> Job<_>
         ConIgnore.Inc join
         Worker.PushNew (&wr, {new Cont_State<_, Job<_>> () with
          override xK'.GetProc (wr) = join.uK.GetProc (&wr)
          override xK'.DoHandle (wr, e) = ConIgnore.OutsideDoHandle (join, &wr, e)
          override xK'.DoCont (wr, _) = ConIgnore.Dec (join, &wr)
          override xK'.DoWork (wr) =
           match xK'.State with
            | null ->
              ConIgnore.Dec (join, &wr)
            | xJ ->
              xK'.State <- null
              xJ.DoJob (&wr, xK')}.Init(xJ))
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

  module Scheduler =
    let inline bind (s2xJ: Scheduler -> #Job<'x>) =
      {new JobSchedulerBind<_> () with
        override xJ'.Do (scheduler) = upcast s2xJ scheduler} :> Job<_>

    let inline get () =
      match StaticData.scheduler with
       | null -> StaticData.Init () ; StaticData.scheduler
       | scheduler -> scheduler

    let inline switchToWorker () =
      match StaticData.switchToWorker with
       | null -> StaticData.Init () ; StaticData.switchToWorker
       | switch -> switch

  ///////////////////////////////////////////////////////////////////////

  let paranoid (xJ: Job<'x>) =
    {new Job<'x> () with
      override xJ'.DoJob (wr, xK) = xJ.DoJob (&wr, xK)}

/////////////////////////////////////////////////////////////////////////

module Promise =
  let start (xJ: Job<'x>) =
    {new Job<Promise<'x>> () with
      override self.DoJob (wr, xPrK) =
       let pr = Promise<'x> ()
       pr.State <- Promise<'x>.Running
       xPrK.Value <- pr
       Worker.Push (&wr, xPrK)
       Job.Do (xJ, &wr, Promise<'x>.Fulfill (pr))}
  let queue (xJ: Job<'x>) =
    {new Job<Promise<'x>> () with
      override self.DoJob (wr, xPrK) =
       let pr = Promise<'x> ()
       pr.State <- Promise<'x>.Running
       Worker.PushNew (&wr, Promise<'x>.Fulfill (pr, xJ))
       Cont.Do (xPrK, &wr, pr)}
  module Now =
    let inline delay (xJ: Job<'x>) = Promise<'x> (xJ)
    let inline withValue (x: 'x) = Promise<'x> (x)
    let inline withFailure (e: exn) = Promise<'x> (e)
    [<MethodImpl(MethodImplOptions.NoInlining)>]
    let never () = let p = Promise<'x> () in p.State <- Promise<'x>.Running ; p
    [<MethodImpl(MethodImplOptions.NoInlining)>]
    let isFulfilled (xP: Promise<'x>) = xP.Full
    [<MethodImpl(MethodImplOptions.NoInlining)>]
    let get (xP: Promise<'x>) = xP.Get ()
  module Infixes =
    open Job.Infixes
    open Alt.Infixes
    let inline (<|>*) xA1 xA2 = xA1 <|>? xA2 |> Now.delay
    let inline (>>=*) xJ x2yJ = xJ >>= x2yJ |> Now.delay
    let inline (>>.*) xJ yJ = xJ >>. yJ |> Now.delay
    let inline (.>>*) xJ yJ = xJ .>> yJ |> Now.delay
    let inline (|>>*) xJ x2y = xJ |>> x2y |> Now.delay
    let inline (>>%*) xJ y = xJ >>% y |> Now.delay
    let inline (>>!*) xJ e = xJ >>! e |> Now.delay
  let inline read (xPr: Promise<'x>) = xPr :> Alt<'x>

/////////////////////////////////////////////////////////////////////////

module Proc =
  let startIgnore (xJ: Job<_>) =
    {new Job<Proc> () with
      override pJ'.DoJob (wr, pK) =
       let proc = Proc ()
       pK.Value <- proc
       Worker.Push (&wr, pK)
       let pf = ProcFinalizer<_> (wr.Scheduler, proc)
       wr.Handler <- pf
       Job.Do (xJ, &wr, pf)}
  let inline start (uJ: Job<unit>) =
    startIgnore uJ

  let queueIgnore (xJ: Job<_>) =
    {new Job<Proc> () with
      override pJ'.DoJob (wr, pK) =
       let proc = Proc ()
       Worker.PushNew (&wr, {new WorkQueue () with
        override w'.DoWork (wr) =
         let pf = ProcFinalizer<_> (wr.Scheduler, proc)
         wr.Handler <- pf
         xJ.DoJob (&wr, pf)})
       Cont.Do (pK, &wr, proc)}
  let inline queue (uJ: Job<unit>) =
    queueIgnore uJ

  let inline self () =
    match StaticData.proc with
     | null -> StaticData.Init () ; StaticData.proc
     | proc -> proc

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
      new (t, uK) = {inherit WorkTimed (t); uK=uK}
      new (t, me, pk, uK) = {inherit WorkTimed (t, me, pk); uK=uK}

    let outOfRange ticks =
      failwithf "Timeout out of range (ticks = %d)" ticks

    let timeOutTicks ticks =
      let ms = (ticks + 9999L) / 10000L // Rounds up.
      if ticks <= 0L then
        if -10000L = ticks then
          Alt.zero ()
        elif 0L = ticks then
          Alt.unit ()
        else
          outOfRange ticks
      elif 21474836470000L < ticks then
        outOfRange ticks
      else
        let ms = int ms
        {new Alt<unit> () with
          override uA'.DoJob (wr, uK) =
           (initGlobalTimer ()).SynchronizedPushTimed
            (WorkTimedUnitCont (Environment.TickCount + ms, uK))
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
    xM >>=? (x2xy >> fun (x, y) -> fill xM x >>% y)
  let inline modifyJob (x2xyJ: 'x -> #Job<'x * 'y>) (xM: MVar<'x>) =
    xM >>=? (x2xyJ >=> fun (x, y) -> fill xM x >>% y)
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
           (x2yJ xs.[0]).DoJob (&wr, {new Cont_State<_, _> () with
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
               ysK.DoWork (&wr)}.Init(0))
         else
           Cont.Do (ysK, &wr, [||])}

    let iterJobIgnore (x2yJ: 'x -> #Job<_>) (xs: array<'x>) =
      {new Job<unit> () with
        override uJ'.DoJob (wr, uK) =
         Work.Do ({new Cont_State<_,_> () with
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
             uK.DoWork (&wr)}.Init(0), &wr)}
    let inline iterJob (x2uJ: 'x -> #Job<unit>) xs =
      iterJobIgnore x2uJ xs

  module Seq =
    let iterJobIgnore (x2yJ: 'x -> #Job<_>) (xs: seq<'x>) =
      {new Job<unit> () with
        override uJ'.DoJob (wr, uK) =
         let xs = xs.GetEnumerator ()
         let yK' = {new Cont<_> () with
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
    let inline iterJob (x2uJ: 'x -> #Job<unit>) xs =
      iterJobIgnore x2uJ xs

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
      let iterJobIgnore (x2yJ: 'x -> #Job<_>) (xs: seq<'x>) =
        {new Job<unit> () with
          override uJ'.DoJob (wr, uK) =
           let xs = xs.GetEnumerator ()
           let join = ConIgnore (xs, uK)
           wr.Handler <- join
           while xs.MoveNext () do
             let x = xs.Current
             ConIgnore.Inc join
             Worker.PushNew (&wr, {new Cont_State<_, _, _ -> #Job<_>> () with
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
                  (x2yJ x).DoJob (&wr, yK')}.Init(x, x2yJ))
           xs.Dispose ()
           join.xs <- null
           ConIgnore.Done (join, &wr)}
      let inline iterJob (x2uJ: 'x -> #Job<unit>) xs =
        iterJobIgnore x2uJ xs

      let mapJob (x2yJ: 'x -> #Job<'y>) (xs: seq<'x>) =
        {new Job<ResizeArray<'y>> () with
          override ysJ'.DoJob (wr, ysK_) =
           ysK_.Value <- ResizeArray<_> ()
           let cc' = {new ConCollect<'x, 'y> () with
             override cc'.DoWork (wr) =
              let mutable nth = 0
              let xs = cc'.xs
              while xs.MoveNext () do
                let x = xs.Current
                cc'.Lock.ExitAndEnter (&wr, cc')
                cc'.ysK.Value.Add Unchecked.defaultof<_>
                let i = Util.dec &nth
                Worker.PushNew (&wr, {new Cont_State<_, _, _, _ -> #Job<_>> () with
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
                     (x2yJ x).DoJob (&wr, yK')}.Init(x, i, x2yJ))
              xs.Dispose ()
              cc'.xs <- null
              ConCollect<'x, 'y>.Done (cc', &wr)}.Init(xs.GetEnumerator (), ysK_)
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
      {new BindTaskWithResult<'x, 'y> () with
        override yJ'.Do (x) = upcast x2yJ x}.InternalInit(xT)

    static member inline bindJob (uT: Task, u2xJ: unit -> #Job<'x>) =
      {new BindTask<'x> () with
        override xJ'.Do () = upcast u2xJ ()}.InternalInit(uT)

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
//      Job.Scheduler.bind <| fun sr ->
//      let rV = IVar.Now.create ()
//      ThreadPool.QueueUserWorkItem (fun _ ->
//        Scheduler.start sr
//         (try IVar.fill rV (thunk ()) with e -> IVar.fillFailure rV e))
//      |> ignore
//      rV

//  type WaitHandle with
//    member wh.awaitAsJob (timeout: TimeSpan) : Job<bool> =
//      Job.Scheduler.bind <| fun sr ->
//      let rV = IVar.Now.create ()
//      ThreadPool.RegisterWaitForSingleObject
//       (wh, (fun _ r -> Scheduler.start sr (IVar.fill rV r)),
//        null, timeout, true) |> ignore
//      rV

//    member wh.awaitAsJob: Job<unit> =
//      Job.Scheduler.bind <| fun sr ->
//      let rV = IVar.Now.create ()
//      ThreadPool.RegisterWaitForSingleObject
//       (wh, (fun _ _ -> Scheduler.start sr (IVar.fill rV ())),
//        null, -1, true) |> ignore
//      rV

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

    let inline passOn (context: SynchronizationContext) x f =
      match context with
       | null -> f x
       | ctxt -> ctxt.Post ((fun _ -> f x), null)

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
      Async.FromContinuations <| fun (x2u, e2u, _) ->
        let ctx = SynchronizationContext.Current
        Worker.RunOnThisThread (sr, xJ, {new Cont_State<'x, Cont<unit>>() with
         override xK'.GetProc (wr) = Handler.GetProc (&wr, &xK'.State)
         override xK'.DoHandle (wr, e) = passOn ctx e e2u
         override xK'.DoWork (wr) = passOn ctx xK'.Value x2u
         override xK'.DoCont (wr, x) = passOn ctx x x2u})

    type [<AbstractClass>] OnWithSchedulerBuilder () =
      abstract Scheduler: Scheduler
      abstract Context: SynchronizationContext

      member inline this.Bind (xT: Task<'x>, x2yA: 'x -> Async<'y>) =
        async.Bind (Async.AwaitTask xT, x2yA)
      member inline this.Bind (xJ: Job<'x>, x2yA: 'x -> Async<'y>) =
        async.Bind (ofJobOn this.Scheduler xJ, x2yA)
      member inline this.Bind (xA: Async<'x>, x2yA: 'x -> Async<'y>) =
        async.Bind (xA, x2yA)

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

      member inline this.TryFinally (xA: Async<'x>, u2u: unit -> unit) =
        async.TryFinally (xA, u2u)

      member inline this.TryWith (xA: Async<'x>, e2xA: exn -> Async<'x>) =
        async.TryWith (xA, e2xA)

      member inline this.Using (x: 'x, x2yA: 'x -> Async<'y>) =
        async.Using (x, x2yA)

      member inline this.While (u2b: unit -> bool, uA: Async<unit>) =
        async.While (u2b, uA)

      member inline this.Zero () = async.Zero ()

      member inline this.Run (xA: Async<'x>) = toJobOn this.Context xA

    let getMain () =
      match StaticData.main with
       | null -> failwith "Hopac: Main synchronization context not set."
       | ctxt -> ctxt

    module Global =
      let ofJob (xJ: Job<'x>) =
        ofJobOn (initGlobalScheduler ()) xJ

      let onMain () = {new OnWithSchedulerBuilder () with
        override this.Context = getMain ()
        override this.Scheduler = initGlobalScheduler ()}

    let setMain (context: SynchronizationContext) =
      let inline check ctxt =
        if ctxt <> context then
          failwith "Hopac: Main synchronization context set inconsistently."
      match StaticData.main with
       | null ->
         lock typeof<StaticData> <| fun () ->
         match StaticData.main with
          | null -> StaticData.main <- context
          | ctxt -> check ctxt
       | ctxt -> check ctxt

  let inline asyncOn context scheduler =
    {new Async.OnWithSchedulerBuilder () with
      override this.Context = context
      override this.Scheduler = scheduler}

  exception OnCompleted

  type ObserveOnce<'x> =
    // Initial = 0, Subscribed = 1, Disposed = 2
    [<DefaultValue>] val mutable State: int
    [<DefaultValue>] val mutable dispose: IDisposable
    val mutable scheduler: Scheduler
    val mutable context: SynchronizationContext
    val mutable xK: Cont<'x>
    val mutable pk: Pick
    new (scheduler, context, xK, pk) =
      {scheduler=scheduler; context=context; xK=xK; pk=pk}
    static member inline DisposeHere (this: ObserveOnce<'x>) =
      match this.dispose with
       | null -> ()
       | disp -> disp.Dispose ()
    static member inline Dispose (this: ObserveOnce<'x>) =
      this.State <- 2
      match this.dispose with
       | null -> ()
       | disp -> Async.passOn this.context () disp.Dispose
    static member inline Commit (this: ObserveOnce<'x>, onCommit: unit -> unit) =
      if 2 <> this.State then
        this.State <- 2
        ObserveOnce<'x>.DisposeHere this
        if 0 = Pick.DoPickOpt this.pk then
          onCommit ()
    static member inline Post (this: ObserveOnce<'x>, observable: IObservable<'x>) =
      if 0 = this.State then
        Async.passOn this.context () <| fun () ->
        this.dispose <- observable.Subscribe this
        if 0 <> Interlocked.CompareExchange (&this.State, 1, 0) then
          ObserveOnce<'x>.DisposeHere this
    interface IObserver<'x> with
      override this.OnError e =
        ObserveOnce<'x>.Commit (this, fun () ->
          Worker.RunOnThisThread (this.scheduler, FailWork (e, this.xK)))
      override this.OnCompleted () = (this :> IObserver<'x>).OnError OnCompleted
      override this.OnNext x =
        ObserveOnce<'x>.Commit (this, fun () ->
          let xK = this.xK
          xK.Value <- x
          Worker.RunOnThisThread (this.scheduler, xK))

  type IObservable<'x> with
    member this.onceAltOn (context: SynchronizationContext) =
      {new Alt<'x> () with
        override xA'.DoJob (wr, xK) =
         let obs = ObserveOnce<'x> (wr.Scheduler, context, xK, null)
         ObserveOnce<'x>.Post (obs, this)
        override xA'.TryAlt (wr, i, xK, xE) =
         let pk = xE.pk
         let obs = ObserveOnce<'x> (wr.Scheduler, context, xK, pk)
         let uns = {new ContQueue<_> () with
          override uK'.DoWork (_) = ObserveOnce<'x>.Dispose obs
          override uK'.DoCont (_, _) = ObserveOnce<'x>.Dispose obs}
         match Pick.ClaimAndAddNack (pk, i) with
          | null -> ()
          | nk ->
            ObserveOnce<'x>.Post (obs, this)
            nk.UnsafeAddReader uns
            Pick.Unclaim pk
            let i = i+1
            nk.I1 <- i
            xE.TryElse (&wr, i)}
    member this.onceAltOnMain = this.onceAltOn (Async.getMain ())
    member this.onceAlt = this.onceAltOn null

/////////////////////////////////////////////////////////////////////////

open Extensions
open Job.Infixes

type JobBuilder () =
  member inline job.Bind (xO: IObservable<'x>, x2yJ: 'x -> Job<'y>) : Job<'y> =
    xO.onceAlt >>= x2yJ
  member inline job.Bind (xA: Async<'x>, x2yJ: 'x -> Job<'y>) : Job<'y> =
    Async.toJob xA >>= x2yJ
  member inline job.Bind (xT: Task<'x>, x2yJ: 'x -> Job<'y>) : Job<'y> =
    Task.bindJob (xT, x2yJ)
  member inline job.Bind (uT: Task, u2xJ: unit -> Job<'x>) : Job<'x> =
    Task.bindJob (uT, u2xJ)
  member inline job.Bind (xJ: Job<'x>, x2yJ: 'x -> Job<'y>) : Job<'y> =
    xJ >>= x2yJ
  member inline job.Combine (uJ: Job<unit>, xJ: Job<'x>) : Job<'x> = uJ >>. xJ
  member inline job.Delay (u2xJ: unit -> Job<'x>) : Job<'x> = Job.delay u2xJ
  member inline job.For (xs: seq<'x>, x2uJ: 'x -> Job<unit>) : Job<unit> =
    Seq.iterJob x2uJ xs
  member inline job.Return (x: 'x) : Job<'x> = Job.result x
  member inline job.ReturnFrom (xO: IObservable<'x>) = xO.onceAlt :> Job<_>
  member inline job.ReturnFrom (xA: Async<'x>) : Job<'x> = Async.toJob xA
  member inline job.ReturnFrom (xT: Task<'x>) : Job<'x> = Task.awaitJob xT
  member inline job.ReturnFrom (uT: Task) : Job<unit> = Task.awaitJob uT
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
  member inline job.Zero () : Job<unit> = Job.unit ()

type EmbeddedJob<'x> = struct
    val Job: Job<'x>
    new (job) = {Job = job}
  end

type EmbeddedJobBuilder () =
  inherit JobBuilder ()
  member this.Run (xJ: Job<'x>) : EmbeddedJob<'x> = EmbeddedJob<'x> (xJ)

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
    [<MethodImpl(MethodImplOptions.NoInlining)>]
    let create initial = Latch (initial)
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
  let queue (l: Latch) (xJ: Job<unit>) = Job.delay <| fun () ->
    Now.increment l
    Job.queue (Job.tryFinallyJob xJ (decrement l))
  let queueAsPromise (l: Latch) (xJ: Job<'x>) = Job.delay <| fun () ->
    Now.increment l
    Promise.queue (Job.tryFinallyJob xJ (decrement l))
