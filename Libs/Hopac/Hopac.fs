// Copyright (C) by Housemarque, Inc.

namespace Hopac

open Hopac.Core
open System
open System.Diagnostics
open System.Threading

/////////////////////////////////////////////////////////////////////////

[<AutoOpen>]
module Util =
  let inline inc (i: byref<int>) : int =
    let j = i+1 in i <- j ; j
  let inline dec (i: byref<int>) : int =
    let j = i-1 in i <- j ; j

  let inline forward (e: exn) : 'a =
    raise (exn ("forwarded", e))

  let inline ctor f x =
    {new Job<_>() with
      override self.DoJob (wr, rK) =
       rK.DoCont (&wr, f x)}

  type TryInCont<'x, 'y> (x2yJ: 'x -> Job<'y>,
                          e2yJ: exn -> Job<'y>,
                          yK: Cont<'y>) =
    inherit Cont<'x> ()
    override self.DoHandle (wr, e) =
     wr.Handler <- yK
     (e2yJ e).DoJob (&wr, yK)
    override self.DoCont (wr, x) =
     wr.Handler <- yK
     (x2yJ x).DoJob (&wr, yK)
    override self.TryNext (wr, i, pk) =
     wr.Handler <- yK // XXX
     yK.TryNext (&wr, i, pk)

/////////////////////////////////////////////////////////////////////////

module Handler =
  let doHandle (e: exn) =
    Console.WriteLine("Unhandled exception: {0}", e)

/////////////////////////////////////////////////////////////////////////

module IVar =
  module Now =
    let inline create () = IVar ()
  let create () = ctor Now.create ()
  let inline fill (v: IVar<'a>) (x: 'a) : Job<unit> =
    IVarFill<'a>(v, x) :> Job<unit>
  let inline read (v: IVar<'a>) : Job<'a> =
    v :> Job<'a>
  module Alt =
    let inline read (v: IVar<'a>) : Alt<'a> =
      v :> Alt<'a>

/////////////////////////////////////////////////////////////////////////

module Ch =
  module Now =
    let inline create () = Ch ()
  let create () = ctor Now.create ()
  let inline give (ch: Ch<'a>) (x: 'a) : Job<unit> =
    ChGive<'a> (ch, x) :> Job<unit>
  let inline take (ch: Ch<'a>) : Job<'a> =
    ch :> Job<'a>
  module Alt =
    let inline give (ch: Ch<'a>) (x: 'a) : Alt<unit> =
      ChGive<'a>(ch, x) :> Alt<unit>
    let inline take (ch: Ch<'a>) : Alt<'a> =
      ch :> Alt<'a>

/////////////////////////////////////////////////////////////////////////

module Mailbox =
  module Now =
    let inline create () = Mailbox ()
  let create () = ctor Now.create ()
  let inline send (mb: Mailbox<_>) x : Job<unit> =
    MailboxSend<'a>(mb, x) :> Job<unit>
  let inline take (mb: Mailbox<_>) : Job<'a> =
    mb :> Job<'a>
  module Alt =
    let inline take (mb: Mailbox<'a>) : Alt<'a> =
      mb :> Alt<'a>

/////////////////////////////////////////////////////////////////////////

module Promise =
  let start (aJ: Job<'a>) : Job<Promise<'a>> =
    {new Job<_>() with
      override self.DoJob (wr, rK) =
       rK.DoCont (&wr, Promise<'a>(&wr, aJ))}
  module Now =
    let inline delay (aJ: Job<'a>) : Promise<'a> =
      Promise<'a>(aJ)
    let inline withValue (a: 'a) : Promise<'a> =
      Promise<'a>(a)
    let inline withFailure (e: exn) : Promise<'a> =
      Promise<'a>(e)
  let delay (aJ: Job<'a>) : Job<Promise<'a>> =
    ctor Now.delay aJ
  let inline read (p: Promise<'a>) : Job<'a> =
    p :> Job<'a>
  module Alt =
    let inline read (p: Promise<'a>) : Alt<'a> =
      p :> Alt<'a>
    
/////////////////////////////////////////////////////////////////////////

module Alt =
  let inline always x =
    Always<_>(x) :> Alt<_>

  let never () =
    {new Alt<'x> () with
      override self.DoJob (wr, _) = ()
      override self.TryAlt (wr, i, pk, aK) = aK.TryNext (&wr, i, pk)}

  let inline GuardJobCont (xK: Cont<'x>) =
    {new Cont<Alt<'x>> () with
      override self.DoHandle (wr, e) = xK.DoHandle (&wr, e)
      override self.DoWork (wr) = self.Value.DoJob (&wr, xK)
      override self.DoCont (wr, xA) = xA.DoJob (&wr, xK)}

  let guard (xAJ: Job<Alt<'x>>) =
    {new Alt<'x>() with
      override self.DoJob (wr, xK) =
       xAJ.DoJob (&wr, GuardJobCont xK)
      override self.TryAlt (wr, i, pk, xK) =
       xAJ.DoJob (&wr, {new Cont<_>() with
        override self.DoHandle (wr, e) = xK.DoHandle (&wr, e)
        override self.DoWork (wr) = self.Value.TryAlt (&wr, i, pk, xK)
        override self.DoCont (wr, xA) = xA.TryAlt (&wr, i, pk, xK)})}

  let delay (u2xA: unit -> Alt<'x>) =
    {new Alt<'x>() with
      override self.DoJob (wr, xK) = (u2xA ()).DoJob (&wr, xK)
      override self.TryAlt (wr, i, pk, xK) = (u2xA ()).TryAlt (&wr, i, pk, xK)}

  let inline pick (xA: Alt<'x>) : Job<'x> =
    xA :> Job<'x>

  let WithNackCont (pk: Pick, xK: Cont<'x>) =
    {new Cont<Alt<'x>> () with
      override self.DoHandle (wr, e) = xK.DoHandle (&wr, e)
      override self.DoCont (wr, xE) =
       let nk = pk.Nacks;
       xE.TryAlt (&wr, nk.I0, pk, {new Cont<_>() with
        override self.DoHandle (wr, e) = xK.DoHandle (&wr, e)
        override self.DoWork (wr) = xK.DoCont (&wr, self.Value)
        override self.DoCont (wr, x) = xK.DoCont (&wr, x)
        override self.TryNext (wr, i, pk) =
         nk.I1 <- i
         xK.TryNext (&wr, i, pk)})}

  let withNack (nack2xAJ: Alt<unit> -> Job<Alt<'x>>) : Alt<'x> =
    {new Alt<'x>() with
      override self.DoJob (wr, xK) =
       (nack2xAJ (never ())).DoJob (&wr, GuardJobCont xK)
      override self.TryAlt (wr, i, pk, xK) =
       match Pick.AddNack (pk, i) with
        | null -> ()
        | nk -> (nack2xAJ nk).DoJob (&wr, WithNackCont (pk, xK))}

  module Infixes =
    type ChoiceCont<'x> (xA2: Alt<'x>, xK: Cont<'x>) =
      inherit Cont<'x> ()
      override self.DoHandle (wr, e) = xK.DoHandle (&wr, e)
      override self.DoWork (wr) = xK.DoCont (&wr, self.Value)
      override self.DoCont (wr, x) = xK.DoCont (&wr, x) 
      override self.TryNext (wr, i, pk) = xA2.TryAlt (&wr, i, pk, xK)

    let (<|>) (xA1: Alt<'x>) (xA2: Alt<'x>) =
      {new Alt<_>() with
        override self.DoJob (wr, xK) =
         xA1.TryAlt (&wr, 0, Pick (), ChoiceCont (xA2, xK))
        override self.TryAlt (wr, i, pk, xK) =
         xA1.TryAlt (&wr, i, pk, ChoiceCont (xA2, xK))}

    let inline MapCont (x2y: 'x -> 'y, yK: Cont<'y>) =
      {new Cont<'x> () with
        override self.DoHandle (wr, e) = yK.DoHandle (&wr, e)
        override self.DoWork (wr) = yK.DoCont (&wr, x2y self.Value)
        override self.DoCont (wr, x) = yK.DoCont (&wr, x2y x)
        override self.TryNext (wr, i, pk) = yK.TryNext (&wr, i, pk)}

    let (>->) (xA: Alt<'x>) (x2y: 'x -> 'y) =
      {new Alt<_>() with
        override self.DoJob (wr, yK) =
         xA.DoJob (&wr, MapCont (x2y, yK))
        override self.TryAlt (wr, i, pk, yK) =
         xA.TryAlt (&wr, i, pk, MapCont (x2y, yK))}

    let inline BindCont (x2yJ: 'x -> Job<'y>, yK: Cont<'y>) =
      {new Cont<'x> () with
        override self.DoHandle (wr, e) = yK.DoHandle (&wr, e)
        override self.DoWork (wr) = (x2yJ self.Value).DoJob (&wr, yK)
        override self.DoCont (wr, x) = (x2yJ x).DoJob (&wr, yK)
        override self.TryNext (wr, i, pk) = yK.TryNext (&wr, i, pk)}

    let (>=>) (xA: Alt<'x>) (x2yJ: 'x -> Job<'y>) =
      {new Alt<_>() with
        override self.DoJob (wr, yK) =
         xA.DoJob (&wr, BindCont (x2yJ, yK))
        override self.TryAlt (wr, i, pk, yK) =
         xA.TryAlt (&wr, i, pk, BindCont (x2yJ, yK))}

  type ChooseCont<'x> (xAs: System.Collections.Generic.IEnumerator<Alt<'x>>, xK: Cont<'x>) =
    inherit Cont<'x> ()
    override self.DoHandle (wr, e) = xK.DoHandle (&wr, e)
    override self.DoWork (wr) = xK.DoCont (&wr, self.Value)
    override self.DoCont (wr, x) = xK.DoCont (&wr, x)
    override self.TryNext (wr, i, pk) =
     if xAs.MoveNext () then
       xAs.Current.TryAlt (&wr, i, pk, self)
     else
       xK.TryNext (&wr, i, pk)

  let choose (xAs: seq<Alt<'x>>) : Alt<'x> =
    {new Alt<_>() with
      override self.DoJob (wr, xK) =
       let xAs = xAs.GetEnumerator ()
       if xAs.MoveNext () then
         xAs.Current.TryAlt (&wr, 0, Pick (), ChooseCont (xAs, xK))
      override self.TryAlt (wr, i, pk, xK) =
       let xAs = xAs.GetEnumerator ()
       if xAs.MoveNext () then
         xAs.Current.TryAlt (&wr, i, pk, ChooseCont (xAs, xK))
       else
         xK.TryNext (&wr, i, pk)}

  let tryIn (xA: Alt<'x>) (x2yJ: 'x -> Job<'y>) (e2yJ: exn -> Job<'y>) : Alt<'y> =
    {new Alt<_>() with
      override self.DoJob (wr, yK) =
       let xK = TryInCont (x2yJ, e2yJ, yK)
       wr.Handler <- xK
       xA.DoJob (&wr, xK)
      override self.TryAlt (wr, i, pk, yK) =
       let xK = TryInCont (x2yJ, e2yJ, yK)
       wr.Handler <- xK
       xA.TryAlt (&wr, i, pk, xK)}

  type [<AllowNullLiteral>] WorkTimedUnitCont =
    inherit WorkTimed
    val uK: Cont<unit>
    override self.DoHandle (wr, e) = self.uK.DoHandle (&wr, e)
    override self.DoWork (wr) = self.uK.DoCont (&wr, ())
    new (t, me, pk, uK) = {inherit WorkTimed (t, me, pk); uK=uK}

  let timeOut (span: System.TimeSpan) : Alt<unit> =
    let ms = span.Ticks / 10000L
    if ms < 0L || 2147483647L < ms then
      failwith "TimeSpan out of allowed range"
    let ms = int ms
    {new Alt<_>() with
      override self.DoJob (wr, uK) =
       Scheduler.SynchronizedPushTimed
        (&wr, WorkTimedUnitCont (Environment.TickCount + ms, 0, null, uK))
      override self.TryAlt (wr, i, pk, uK) =
       Scheduler.SynchronizedPushTimed
        (&wr, WorkTimedUnitCont (Environment.TickCount + ms, i, pk, uK))
       uK.TryNext (&wr, i+1, pk)}

/////////////////////////////////////////////////////////////////////////

module Job =
  module Now =
    let startWithActions (eK: exn -> unit)
                         (aK: 'a -> unit)
                         (aJ: Job<'a>) : unit =
      Scheduler.Init ()
      Worker.RunOnThisThread (JobWork (aJ, {new Cont<_>() with
       override self.DoHandle (wr, e) = eK e
       override self.DoCont (wr, a) = aK a}))

    let start (aJ: Job<'a>) : unit =
      Scheduler.Init ()
      Worker.RunOnThisThread (JobWork (aJ, {new Cont<_> () with
       override self.DoHandle (_, e) = Handler.doHandle e
       override self.DoWork (_) = ()
       override self.DoCont (_, _) = ()}))

    let run (aJ: Job<'a>) : 'a =
      Scheduler.Init ()
      use event = new ManualResetEventSlim ()
      let aK = {new Cont_State<_, _> () with
       override self.DoHandle (wr, e) = self.State <- e ; event.Set ()
       override self.DoWork (wr) = event.Set ()
       override self.DoCont (wr, a) = self.Value <- a ; event.Set ()}
      Worker.RunOnThisThread (JobWork (aJ, aK))
      event.Wait ()
      match aK.State with
       | null -> aK.Value
       | e -> Util.forward e

  ///////////////////////////////////////////////////////////////////////

  let delay (u2aJ: unit -> Job<'a>) : Job<'a> =
    {new Job<_>() with
      override self.DoJob (wr, aK) =
       (u2aJ ()).DoJob (&wr, aK)}

  let delayWith (a2bJ: 'a -> Job<'b>) (a: 'a) : Job<'b> =
    {new Job<_>() with
      override self.DoJob (wr, bK) =
       (a2bJ a).DoJob (&wr, bK)}

  let lift (a2b: 'a -> 'b) (a: 'a) : Job<'b> =
    {new Job<_>() with
      override self.DoJob (wr, bK) = bK.DoCont (&wr, a2b a)}

  let thunk (u2a: unit -> 'a) : Job<'a> =
    {new Job<_>() with
      override self.DoJob (wr, aK) = aK.DoCont (&wr, u2a ())}

  let forN (n: int) (aJ: Job<'a>) =
    {new Job<_>() with
      override self.DoJob (wr, uK) =
       let loop =
         {new Cont_State<_, _>(n) with
           override self.DoHandle (wr, e) = uK.DoHandle (&wr, e)
           override self.DoCont (wr, _) = self.DoWork (&wr)
           override self.DoWork (wr) =
            let mutable m = self.State
            if 0 < m then
              m <- m - 1
              self.State <- m
              if 511 = (m &&& 511) then
                Worker.Push (&wr, JobWork (aJ, self))
              else
                aJ.DoJob (&wr, self)
            else
              uK.DoCont (&wr, ())}
       loop.DoWork (&wr)}

  module Internal =
    let inline mkFor more next i0 i1 (i2aJ: _ -> Job<_>) =
      {new Job<_>() with
        override self.DoJob (wr, uK) =
         let loop =
           {new Cont_State<_, _>(i0) with
             override self.DoHandle (wr, e) = uK.DoHandle (&wr, e)
             override self.DoCont (wr, _) = self.DoWork (&wr)
             override self.DoWork (wr) =
              let i = self.State
              if more i i1 then
                self.State <- next i
                (i2aJ i).DoJob (&wr, self)
              else
                uK.DoCont (&wr, ())}
         loop.DoWork (&wr)}

  let forUpTo (i0: int) (i1: int) (i2aJ: int -> Job<'a>) : Job<unit> =
    Internal.mkFor (fun i i1 -> i <= i1) (fun i -> i + 1) i0 i1 i2aJ

  let forDownTo (i0: int) (i1: int) (i2aJ: int -> Job<'a>) : Job<unit> =
    Internal.mkFor (fun i i1 -> i1 <= i) (fun i -> i - 1) i0 i1 i2aJ

  let forever (aJ: Job<'a>) : Job<'b> =
    {new Job<_>() with
      override self.DoJob (wr, bK) =
       let loop = {new Cont<_>() with
         override self.DoHandle (wr, e) = bK.DoHandle (&wr, e)
         override self.DoWork (wr) = aJ.DoJob (&wr, self)
         override self.DoCont (wr, _) = aJ.DoJob (&wr, self)}
       aJ.DoJob (&wr, loop)}

  let iterate (a: 'a) (a2aJ: 'a -> Job<'a>) =
    {new Job<_>() with
      override self.DoJob (wr, bK) =
       (a2aJ a).DoJob (&wr, {new Cont<_>() with
         override self.DoHandle (wr, e) = bK.DoHandle (&wr, e)
         override self.DoWork (wr) = (a2aJ self.Value).DoJob (&wr, self)
         override self.DoCont (wr, a) = (a2aJ a).DoJob (&wr, self)})}

  let whileDo (cond: unit -> bool) (body: Job<'a>) =
    {new Job<_>() with
      override self.DoJob (wr, uK) =
       if cond () then
         body.DoJob (&wr, {new Cont<_> () with
           override loop.DoHandle (wr, e) = uK.DoHandle (&wr, e)
           override loop.DoCont (wr, _) =
            if cond () then body.DoJob (&wr, loop) else uK.DoCont (&wr, ())
           override loop.DoWork (wr) =
            if cond () then body.DoJob (&wr, loop) else uK.DoCont (&wr, ())})
       else
         uK.DoCont (&wr, ())}

  let result (x: 'x) =
    {new Job<'x> () with
      override self.DoJob (wr, xK) =
       xK.DoCont (&wr, x)}

  let unit = result ()

  let inline whenDo (b: bool) (body: Job<unit>) : Job<unit> =
    if b then body else unit

  ///////////////////////////////////////////////////////////////////////

  module Infixes =
    let (>>=) (aJ: Job<'a>) (a2bJ: 'a -> Job<'b>) : Job<'b> =
      {new Job<_>() with
        override self.DoJob (wr, bK) =
         aJ.DoJob (&wr, {new Cont<_>() with
          override self.DoHandle (wr, e) = bK.DoHandle (&wr, e)
          override self.DoWork (wr) = (a2bJ self.Value).DoJob (&wr, bK)
          override self.DoCont (wr, a) = (a2bJ a).DoJob (&wr, bK)})}

    let (>>.) (aJ: Job<'a>) (bJ: Job<'b>) : Job<'b> =
      {new Job<_>() with
        override self.DoJob (wr, bK) =
         aJ.DoJob (&wr, {new Cont<_>() with
          override self.DoHandle (wr, e) = bK.DoHandle (&wr, e)
          override self.DoWork (wr) = bJ.DoJob (&wr, bK)
          override self.DoCont (wr, _) = bJ.DoJob (&wr, bK)})}

    type DropCont<'x, 'y> (xK: Cont<'x>) =
      inherit Cont<'y> ()
      override self.DoHandle (wr, e) = xK.DoHandle (&wr, e)
      override self.DoWork (wr) = xK.DoCont (&wr, xK.Value)
      override self.DoCont (wr, _) = xK.DoCont (&wr, xK.Value)

    let (.>>) (aJ: Job<'a>) (bJ: Job<'b>) : Job<'a> =
      {new Job<_>() with
        override self.DoJob (wr, aK) =
         aJ.DoJob (&wr, {new Cont<_>() with
          override self.DoHandle (wr, e) = aK.DoHandle (&wr, e)
          override self.DoWork (wr) =
           aK.Value <- self.Value
           bJ.DoJob (&wr, DropCont aK)
          override self.DoCont (wr, a) =
           aK.Value <- a
           bJ.DoJob (&wr, DropCont aK)})}

    let (|>>) (aJ: Job<'a>) (a2b: 'a -> 'b) : Job<'b> =
      {new Job<_>() with
        override self.DoJob (wr, bK) =
         aJ.DoJob (&wr, {new Cont<_>() with
          override self.DoHandle (wr, e) = bK.DoHandle (&wr, e)
          override self.DoWork (wr) = bK.DoCont (&wr, a2b self.Value)
          override self.DoCont (wr, a) = bK.DoCont (&wr, a2b a)})}

    let (>>%) (aJ: Job<'a>) (b: 'b) =
      {new Job<_>() with
        override self.DoJob (wr, bK) =
         bK.Value <- b
         aJ.DoJob (&wr, {new Cont<_>() with
          override self.DoHandle (wr, e) = bK.DoHandle (&wr, e)
          override self.DoWork (wr) = bK.DoCont (&wr, bK.Value)
          override self.DoCont (wr, _) = bK.DoCont (&wr, bK.Value)})}

    let (>>!) (aJ: Job<'a>) (e: exn) =
      {new Job<_>() with
        override self.DoJob (wr, bK) =
         bK.DoHandle (&wr, e)}

    type PairCont<'x, 'y> (yJ: Job<'y>, xyK: Cont<'x * 'y>) =
      inherit Cont<'x> ()
      override self.DoHandle (wr, e) = xyK.DoHandle (&wr, e)
      override self.DoCont (wr, a) =
       yJ.DoJob (&wr, {new Cont<_>() with
        override self.DoHandle (wr, e) = xyK.DoHandle (&wr, e)
        override self.DoCont (wr, b) = xyK.DoCont (&wr, (a, b))})

    let (<&>) (aJ: Job<'a>) (bJ: Job<'b>) =
      {new Job<'a * 'b>() with
        override self.DoJob (wr, abK) =
         aJ.DoJob (&wr, PairCont (bJ, abK))}

    let (<*>) (aJ: Job<'a>) (bJ: Job<'b>) : Job<'a * 'b> =
      {new Job<_>() with
        override self.DoJob (wr, abK) =
          if 0 <= Scheduler.Waiters then
            let bK = ParTuple<'a, 'b> (abK)
            Worker.Push (&wr, {new Cont_State<_, _>(aJ) with
             override self.DoHandle (wr, e) = bK.DoHandle (&wr, e)
             override self.DoCont (wr, a) = bK.DoOtherCont (&wr, a)
             override self.DoWork (wr) =
              match self.State with
               | null -> bK.DoOtherCont (&wr, self.Value)
               | aJ ->
                 self.State <- null
                 aJ.DoJob (&wr, self)})
            bJ.DoJob (&wr, bK)
          else
            aJ.DoJob (&wr, PairCont (bJ, abK))}

  ///////////////////////////////////////////////////////////////////////

  type DelayWithWork<'x, 'y> (x2yJ: 'x -> Job<'y>, x: 'x, yK: Cont<'y>) =
    inherit Work ()
    override self.DoHandle (wr, e) = yK.DoHandle (&wr, e)
    override self.DoWork (wr) = (x2yJ x).DoJob (&wr, yK)

  let tryIn (aJ: Job<'a>) (a2bJ: 'a -> Job<'b>) (e2bJ: exn -> Job<'b>) : Job<'b> =
    {new Job<'b>() with
      override self.DoJob (wr, bK) =
       let aK = TryInCont (a2bJ, e2bJ, bK)
       wr.Handler <- aK
       aJ.DoJob (&wr, aK)}

  let tryWith (aJ: Job<'a>) (e2aJ: exn -> Job<'a>) : Job<'a> =
    {new Job<'a>() with
      override self.DoJob (wr, aK) =
       let aK' = {new Cont<_> () with
        override self.DoHandle (wr, e) =
         wr.Handler <- aK
         (e2aJ e).DoJob (&wr, aK)
        override self.DoCont (wr, a) =
         wr.Handler <- aK
         aK.DoCont (&wr, a)}
       wr.Handler <- aK'
       aJ.DoJob (&wr, aK')}

  let tryFinally (aJ: Job<'a>) (u2u: unit -> unit) : Job<'a> =
    {new Job<'a>() with
      override self.DoJob (wr, aK) =
       let aK' = {new Cont<_> () with
        override self.DoHandle (wr, e) =
         wr.Handler <- aK
         u2u ()
         aK.DoHandle (&wr, e)
        override self.DoCont (wr, a) =
         wr.Handler <- aK
         u2u ()
         aK.DoCont (&wr, a)}
       wr.Handler <- aK'
       aJ.DoJob (&wr, aK')}

  let using (a: 'a when 'a :> System.IDisposable) (a2bJ: 'a -> Job<'b>) : Job<'b> =
    {new Job<'b>() with
      override self.DoJob (wr, bK) =
       let bK' = {new Cont<_> () with
        override self.DoHandle (wr, e) =
         wr.Handler <- bK
         a.Dispose ()
         bK.DoHandle (&wr, e)
        override self.DoCont (wr, b) =
         wr.Handler <- bK
         a.Dispose ()
         bK.DoCont (&wr, b)}
       wr.Handler <- bK'
       (a2bJ a).DoJob (&wr, bK')}

  let catch (aJ: Job<'a>) : Job<Choice<'a, exn>> =
    {new Job<Choice<'a, exn>>() with
      override self.DoJob (wr, rK) =
       let aK = {new Cont<_> () with
        override self.DoHandle (wr, e) =
         wr.Handler <- rK
         rK.DoCont (&wr, Choice2Of2 e)
        override self.DoCont (wr, a) =
         wr.Handler <- rK
         rK.DoCont (&wr, Choice1Of2 a)}
       wr.Handler <- aK
       aJ.DoJob (&wr, aK)}

  ///////////////////////////////////////////////////////////////////////

  let start (aJ: Job<'a>) : Job<unit> =
    {new Job<unit>() with
      override self.DoJob (wr, uK) =
       let aK = {new Cont<_> () with
         override aK.DoHandle (_, e) = Handler.doHandle e
         override aK.DoWork (_) = ()
         override aK.DoCont (_, _) = ()}
       Worker.Push (&wr, {new Work () with
         override w.DoHandle (_, e) = Handler.doHandle e
         override w.DoWork (wr) = aJ.DoJob (&wr, aK)})
       uK.DoCont (&wr, ())}

  ///////////////////////////////////////////////////////////////////////

  let seqCollect (xJs: seq<Job<'x>>) : Job<seq<'x>> =
    {new Job<_>() with
      override self.DoJob (wr, xsK) =
       let xJs = xJs.GetEnumerator ()
       let xs = ResizeArray<_>()
       let loop =
         {new Cont<_>() with
           override self.DoHandle (wr, e) = xsK.DoHandle (&wr, e)
           override self.DoCont (wr, x) =
            xs.Add x
            if xJs.MoveNext () then
              xJs.Current.DoJob (&wr, self)
            else
              xsK.DoCont (&wr, xs)}
       if xJs.MoveNext () then
         xJs.Current.DoJob (&wr, loop)
       else
         xsK.DoCont (&wr, xs)}

  type [<Sealed>] ParCollect<'y> =
    inherit Handler
    val mutable Lock: SpinlockWithOwner
    val mutable n: int
    val ys: ResizeArray<'y>
    val ysK: Cont<seq<'y>>
    val mutable es: ResizeArray<exn>
    static member inline Dec (self: ParCollect<_>, owner: Work, wr: byref<Worker>) : unit =
      if Util.dec &self.n = 0 then
        let ysK = self.ysK
        wr.Handler <- ysK
        match self.es with
         | null -> self.ysK.DoCont (&wr, self.ys)
         | es -> self.ysK.DoHandle (&wr, AggregateException es)
      else
        SpinlockWithOwner.Exit (owner)
    static member inline Add (self: ParCollect<_>, visitor: Work, wr: byref<Worker>, i, y) =
      self.Lock.Enter (visitor)
      self.ys.[i] <- y
      ParCollect<_>.Dec (self, visitor, &wr)
    override self.DoHandle (wr, e) =
     let owner = SpinlockWithOwner.Owner ()
     self.Lock.Enter (owner)
     match self.es with
      | null ->
        let es = ResizeArray<_>()
        es.Add e
        self.es <- es
      | es ->
        es.Add e
     if Util.dec &self.n = 0 then
       wr.Handler <- self.ysK
       self.ysK.DoHandle (&wr, AggregateException self.es)
     else
       SpinlockWithOwner.Exit (owner)
    new (ysK) = {
      inherit Handler ()
      Lock = Unchecked.defaultof<_>
      n = 1
      ys = ResizeArray<_>()
      ysK = ysK
      es = null
    }

  let parCollect (xJs: seq<Job<'a>>) : Job<seq<'a>> =
    {new Job<_>() with
      override self.DoJob (wr, xsK) =
       let st = ParCollect<_>(xsK)
       let owner = st.Lock.Init ()
       let mutable nth = 0
       wr.Handler <- st
       let xJs = xJs.GetEnumerator ()
       while xJs.MoveNext () do
         let xJ = xJs.Current
         st.Lock.ExitAndEnter (owner)
         st.n <- st.n + 1
         st.ys.Add Unchecked.defaultof<_>
         let i = nth
         nth <- i + 1
         Worker.Push (&wr, {new Cont_State<_, _>(xJ) with
          override self.DoHandle (wr, e) = st.DoHandle (&wr, e)
          override self.DoCont (wr, y) = ParCollect<_>.Add (st, self, &wr, i, y)
          override self.DoWork (wr) =
           match self.State with
            | null -> ParCollect<_>.Add (st, self, &wr, i, self.Value)
            | xJ ->
              self.State <- null
              xJ.DoJob (&wr, self)})
       ParCollect<_>.Dec (st, owner, &wr)}

  type ParIgnore =
    val mutable n: int
    val mutable exns: ResizeArray<exn> 
    val uK: Cont<unit>
    new (uK) = {n = 1; exns = null; uK = uK}
    static member inline Inc (self: ParIgnore) =
      Interlocked.Increment &self.n |> ignore
    static member inline Dec (self: ParIgnore, wr: byref<Worker>) =
      if 0 = Interlocked.Decrement &self.n then
        match self.exns with
         | null ->
           self.uK.DoCont (&wr, ())
         | exns ->
           self.uK.DoHandle (&wr, AggregateException exns)
    static member inline Exn (self: ParIgnore, wr: byref<Worker>, e: exn) =
      lock self <| fun () ->
        let exns =
          match self.exns with
           | null ->
             let exns = ResizeArray<_>()
             self.exns <- exns
             exns
           | exns -> exns
        exns.Add e
      ParIgnore.Dec (self, &wr)

  let parIgnore (xJs: seq<Job<'a>>) : Job<unit> =
    {new Job<unit>() with
      override self.DoJob (wr, uK) =
       let join = ParIgnore (uK)
       let xJs = xJs.GetEnumerator ()
       while xJs.MoveNext () do
         let xJ = xJs.Current
         ParIgnore.Inc join
         Worker.Push (&wr, {new Cont_State<_, _>(xJ) with
          override self.DoHandle (wr, e) = ParIgnore.Exn (join, &wr, e)
          override self.DoCont (wr, y) = self.DoWork (&wr)
          override self.DoWork (wr) =
           match self.State with
            | null ->
              ParIgnore.Dec (join, &wr)
            | xJ ->
              self.State <- null
              xJ.DoJob (&wr, self)})
       ParIgnore.Dec (join, &wr)}

  ///////////////////////////////////////////////////////////////////////

  let doAsyncCallback = AsyncCallback (fun ar ->
    match ar.AsyncState with
      | :? WorkWithReady<IAsyncResult> as ta ->
        ta.Ready(ar);
      | _ ->
        failwith "Bug")

  type AsyncBeginEnd<'x>(doEnd, xK: Cont<'x>) =
    inherit WorkWithReady<IAsyncResult> ()
    override self.DoHandle (wr, e) = xK.DoHandle (&wr, e)
    override self.DoWork (wr) = xK.DoCont (&wr, doEnd self.Value)

  let fromBeginEnd (doBegin: AsyncCallback * obj -> IAsyncResult)
                   (doEnd: IAsyncResult -> 'x) : Job<'x> =
    {new Job<_>() with
      override self.DoJob (wr, xK) =
       let rv = AsyncBeginEnd<'x> (doEnd, xK)
       doBegin (doAsyncCallback, rv) |> ignore}

  ///////////////////////////////////////////////////////////////////////

  let sleep (span: TimeSpan) : Job<unit> =
    Alt.timeOut span :> Job<unit>

/////////////////////////////////////////////////////////////////////////

module Lock =
  module Now =
    let inline create () = Lock ()
  let create () = ctor Now.create ()
  let inline duringFun (l: Lock) (aF: unit -> 'a) : Job<'a> =
    LockDuringFun<'a>(l, aF) :> Job<'a>
  let inline duringJob (l: Lock) (aJ: Job<'a>) : Job<'a> =
    LockDuringJob<'a>(l, aJ) :> Job<'a>

/////////////////////////////////////////////////////////////////////////

module MVar =
  open Job.Infixes
  module Now =
    let inline create () = MVar ()
    let inline createFull x = MVar (x)
  let create () = ctor Now.create ()
  let createFull x = ctor Now.createFull x
  let inline fill (v: MVar<'a>) (x: 'a) : Job<unit> =
    MVarFill<'a>(v, x) :> Job<unit>
  let inline take (v: MVar<'a>) : Job<'a> =
    v :> Job<'a>
  let inline modify x2xyJob v =
    take v >>= x2xyJob >>= fun (x, y) ->
    fill v x >>% y
  module Alt =
    let inline take (v: MVar<'a>) : Alt<'a> =
      v :> Alt<'a>

/////////////////////////////////////////////////////////////////////////

module Extensions =
  open Job

  module Array =
    let mapJ (x2yJ: 'x -> Job<'y>) (xs: array<'x>) =
      {new Job<_>() with
        override self.DoJob (wr, ysK) =
         if 0 < xs.Length then
           let ys = Array.zeroCreate xs.Length
           let loop =
             {new Cont_State<_, _>(0) with
               override self.DoHandle (wr, e) = ysK.DoHandle (&wr, e)
               override self.DoCont (wr, y) =
                let j = self.State
                ys.[j] <- y
                let j = j + 1
                if j < xs.Length then
                  self.State <- j
                  (x2yJ xs.[j]).DoJob (&wr, self)
                else
                  ysK.DoCont (&wr, ys)}
           (x2yJ xs.[0]).DoJob (&wr, loop)
         else
           ysK.DoCont (&wr, [||])}

    let iterJ (x2yJ: 'a -> Job<'b>) (xs: array<'a>) =
      {new Job<_>() with
        override self.DoJob (wr, uK) =
          let loop =
            {new Cont_State<_,_>(0) with
              override self.DoHandle (wr, e) = uK.DoHandle (&wr, e)
              override self.DoCont (wr, _) = self.DoWork (&wr)
              override self.DoWork (wr) =
                let i = self.State
                if i < xs.Length then
                  let x = xs.[i] 
                  self.State <- i+1
                  (x2yJ x).DoJob (&wr, self)
                else
                  uK.DoCont (&wr, ())}
          loop.DoWork (&wr)}

  module Seq =
    let iterJ (x2yJ: 'a -> Job<'b>) (xs: seq<'a>) =
      {new Job<_>() with
        override self.DoJob (wr, uK) =
         let xs = xs.GetEnumerator ()
         let loop =
           {new Cont<_>() with
             override self.DoHandle (wr, e) = uK.DoHandle (&wr, e)
             override self.DoCont (wr, _) = self.DoWork (&wr)
             override self.DoWork (wr) =
               if xs.MoveNext () then
                 (x2yJ xs.Current).DoJob (&wr, self)
               else
                 uK.DoCont (&wr, ())}
         loop.DoWork (&wr)}

    let mapJ (x2yJ: 'x -> Job<'y>) (xs: seq<'x>) =
      {new Job<seq<'y>>() with
        override self.DoJob (wr, ysK) =
         let xs = xs.GetEnumerator ()
         let ys = ResizeArray<_>()
         let loop =
           {new Cont<_>() with
             override self.DoHandle (wr, e) = ysK.DoHandle (&wr, e)
             override self.DoCont (wr, y) =
              ys.Add y
              if xs.MoveNext () then
                (x2yJ xs.Current).DoJob (&wr, self)
              else
                ysK.DoCont (&wr, ys)}
         if xs.MoveNext () then
           (x2yJ xs.Current).DoJob (&wr, loop)
         else
           ysK.DoCont (&wr, ys)}

    let foldJ (xy2xJ: 'x -> 'y -> Job<'x>) (x: 'x) (ys: seq<'y>) =
      {new Job<'x>() with
        override self.DoJob (wr, xK) =
         let ys = ys.GetEnumerator ()
         let loop =
           {new Cont<_>() with
             override self.DoHandle (wr, e) = xK.DoHandle (&wr, e)
             override self.DoCont (wr, x) =
              if ys.MoveNext () then
                (xy2xJ x ys.Current).DoJob (&wr, self)
              else
                xK.DoCont (&wr, x)}
         if ys.MoveNext () then
           (xy2xJ x ys.Current).DoJob (&wr, loop)
         else
           xK.DoCont (&wr, x)}

    module Parallel =
      let iterJ (x2yJ: 'a -> Job<'b>) (xs: seq<'a>) : Job<unit> =
        {new Job<_>() with
          override self.DoJob (wr, uK) =
           let join = ParIgnore (uK)
           let xs = xs.GetEnumerator ()
           while xs.MoveNext () do
             let x = xs.Current
             ParIgnore.Inc join
             Worker.Push (&wr, {new Cont_State<_, _> () with
              override self.DoHandle (wr, e) = ParIgnore.Exn (join, &wr, e)
              override self.DoCont (wr, _) = ParIgnore.Dec (join, &wr)
              override self.DoWork (wr) =
               if self.State then
                 ParIgnore.Dec (join, &wr)
               else
                 self.State <- true
                 (x2yJ x).DoJob (&wr, self)})
           ParIgnore.Dec (join, &wr)}

      let mapJ (x2yJ: 'a -> Job<'b>) (xs: seq<'a>) : Job<seq<'b>> =
        {new Job<_>() with
          override self.DoJob (wr, ysK) =
           let st = ParCollect (ysK)
           let owner = st.Lock.Init ()
           let xs = xs.GetEnumerator ()
           wr.Handler <- st
           let mutable nth = 0
           while xs.MoveNext () do
             let x = xs.Current
             st.Lock.ExitAndEnter (owner)
             st.n <- st.n + 1
             st.ys.Add Unchecked.defaultof<_>
             let i = nth
             nth <- i + 1
             Worker.Push (&wr, {new Cont_State<_, _, _> (x, true) with
              override self.DoHandle (wr, e) = st.DoHandle (&wr, e)
              override self.DoCont (wr, y) = ParCollect<_>.Add (st, self, &wr, i, y)
              override self.DoWork (wr) =
               if self.State2 then
                 let x = self.State1
                 self.State1 <- Unchecked.defaultof<_>
                 self.State2 <- false
                 (x2yJ x).DoJob (&wr, self)
               else
                 ParCollect<_>.Add (st, self, &wr, i, self.Value)})
           ParCollect<_>.Dec (st, owner, &wr)}

  ///////////////////////////////////////////////////////////////////////
  
  type [<Sealed>] Task =
    static member inline awaitJ (task: System.Threading.Tasks.Task<'x>) : Job<'x> =
      AwaitTaskWithResult<'x> (task) :> Job<'x>

    static member inline awaitJ (task: System.Threading.Tasks.Task) : Job<unit> =
      AwaitTask (task) :> Job<unit>

/////////////////////////////////////////////////////////////////////////

open Extensions
open Job.Infixes

type [<Sealed>] JobBuilder () =
  member inline x.Bind (aJ: Job<'a>, a2bJ: 'a -> Job<'b>) : Job<'b> =
    aJ >>= a2bJ
  member inline x.Combine (uJ: Job<unit>, aJ: Job<'a>) : Job<'a> = uJ >>. aJ
  member inline x.Delay (u2aJ: unit -> Job<'a>) : Job<'a> = Job.delay u2aJ
  member inline x.For (aS: seq<'a>, a2uJ: 'a -> Job<unit>) : Job<unit> =
    Seq.iterJ a2uJ aS
  member inline x.For (aS: array<'a>, a2uJ: 'a -> Job<unit>) : Job<unit> =
    Array.iterJ a2uJ aS
  member inline x.Return (a: 'a) : Job<'a> = Job.result a
  member inline x.ReturnFrom (aJ: Job<'a>) : Job<'a> = aJ
  member inline x.TryFinally (aJ: Job<'a>, u2u: unit -> unit) : Job<'a> =
    Job.tryFinally aJ u2u
  member inline x.TryWith (aJ: Job<'a>, e2aJ: exn -> Job<'a>) : Job<'a> =
    Job.tryWith aJ e2aJ
  member inline x.Using (a: 'a, a2bJ: 'a -> Job<'b>) : Job<'b>
      when 'a :> System.IDisposable =
    Job.using a a2bJ
  member inline x.While (u2b: unit -> bool, uJ: Job<unit>) : Job<unit> =
    Job.whileDo u2b uJ
  member inline x.Zero () : Job<unit> = Job.unit

[<AutoOpen>]
module TopLevel =
  let job = JobBuilder ()
