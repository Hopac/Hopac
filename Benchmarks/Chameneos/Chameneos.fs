// Copyright (C) by Housemarque, Inc.

module Chameneos

open Hopac
open Hopac.Infixes
open Hopac.Extensions
open Hopac.Alt.Infixes
open Hopac.Job.Infixes
open System
open System.Diagnostics

/////////////////////////////////////////////////////////////////////////

type Color =
  | R = 0
  | B = 1
  | Y = 2

let inline complement a b =
  match (a, b) with
   | (Color.B, Color.R) -> Color.Y
   | (Color.B, Color.Y) -> Color.R
   | (Color.R, Color.B) -> Color.Y
   | (Color.R, Color.Y) -> Color.B
   | (Color.Y, Color.B) -> Color.R
   | (Color.Y, Color.R) -> Color.B
   | (x,             _) -> x

let colorsAll = [|Color.B; Color.R; Color.Y|]
let colors10 = [|
  Color.B; Color.R; Color.Y; Color.R; Color.Y;
  Color.B; Color.R; Color.Y; Color.R; Color.B|]

/////////////////////////////////////////////////////////////////////////

module HopacLock =
  type [<AllowNullLiteral>] Chameneos =
    val mutable Color: Color
    val WakeUp: MVar<Color>
    new (color) = {Color = color; WakeUp = mvar ()}

  type MeetingPlace =
    inherit Lock
    val mutable NumMeets: int
    val mutable Waiter: Chameneos
    new (numMeets) = {inherit Lock (); NumMeets = numMeets; Waiter = null}
  
  let bench (colors: array<Color>) numMeets = Job.delay <| fun () ->
    let mp = MeetingPlace (numMeets)
    let resultsMS = ch ()
    colors
    |> Seq.iterJob (fun myColor ->
       let me = Chameneos (myColor)
       let myMeets = ref 0
       let cont = ref true
       Job.queue
        (Job.whileDo (fun () -> !cont)
          (Lock.duringFun mp (fun () ->
             if 0 = mp.NumMeets then
               cont := false
               null
             else
               match mp.Waiter with
                | null ->
                  mp.Waiter <- me
                  null
                | other ->
                  mp.Waiter <- null
                  mp.NumMeets <- mp.NumMeets - 1
                  other) >>= function
            | null ->
              if !cont then
                me.WakeUp |>> fun otherColor ->
                myMeets := !myMeets + 1
                me.Color <- complement me.Color otherColor
              else
                resultsMS <-- !myMeets :> Job<_>
            | other ->
              let otherColor = other.Color
              other.WakeUp <<-= me.Color |>> fun () ->
              myMeets := !myMeets + 1
              me.Color <- complement me.Color otherColor))) >>= fun () ->
    Seq.foldJob
     (fun sum _ -> resultsMS |>> fun n -> sum+n)
     0
     (seq {1 .. colors.Length})

  let run numMeets =
    printf "Lock: "
    let timer = Stopwatch.StartNew ()
    let (n, m) = run <| job {
      let! pn = Promise.queue (bench colors10 numMeets)
      let! pm = Promise.start (bench colorsAll numMeets)
      return! pn <&> pm
    }
    let d = timer.Elapsed
    printf "%d %d %fs (%d, %d)\n" numMeets Environment.ProcessorCount d.TotalSeconds n m

/////////////////////////////////////////////////////////////////////////

module HopacMV =
  type Chameneos =
    | Chameneos of Color * MVar<Chameneos>

  type MeetingPlace =
    | Empty of int
    | Waiter of int * Chameneos

  let bench (colors: array<Color>) numMeets = Job.delay <| fun () ->
    let resultsMS = ch ()
    let meetingPlace = mvar ()
    meetingPlace <<-= Empty numMeets >>= fun () ->
    colors
    |> Seq.iterJob (fun myColor ->
       let me = mvar ()
       let myMeets = ref 0
       let myColor = ref myColor
       let cont = ref true
       Job.queue
        (Job.whileDo (fun () -> !cont)
          (meetingPlace >>= function
            | (Empty 0) as state ->
              meetingPlace <<-= state >>= fun () ->
              cont := false
              resultsMS <-- !myMeets
            | Empty n ->
              meetingPlace <<-= Waiter (n, Chameneos (!myColor, me)) >>= fun () ->
              me |>> fun (Chameneos (otherColor, other)) ->
              myMeets := !myMeets + 1
              myColor := complement (!myColor) otherColor
            | Waiter (n, Chameneos (otherColor, other)) ->
              other <<-= Chameneos (!myColor, me) >>= fun () ->
              meetingPlace <<-= Empty (n-1) |>> fun () ->
              myMeets := !myMeets + 1
              myColor := complement (!myColor) otherColor))) >>= fun () ->
    Seq.foldJob
     (fun sum _ -> resultsMS |>> fun n -> sum+n)
     0
     (seq {1 .. colors.Length})

  let run numMeets =
    printf "MVar: "
    let timer = Stopwatch.StartNew ()
    let (n, m) = run <| job {
      let! pn = Promise.queue (bench colors10 numMeets)
      let! pm = Promise.start (bench colorsAll numMeets)
      return! pn <&> pm
    }
    let d = timer.Elapsed
    printf "%d %d %fs (%d, %d)\n" numMeets Environment.ProcessorCount d.TotalSeconds n m
    
/////////////////////////////////////////////////////////////////////////

module HopacAlt =
  [<AutoOpen>]
  module CountedSwapChTypes =
    type CountedSwapCh<'a> = {
      mutable N: int
      Ch: Ch<'a * Ch<Option<'a>>>
      Done: IVar<Option<'a>>
    }

  module CountedSwapCh =
    let create n : Job<CountedSwapCh<'x>> = Job.thunk <| fun () ->
      {N = n; Ch = ch (); Done = ivar ()}

    let swap (csch: CountedSwapCh<'x>) (msgOut: 'x) : Alt<Option<'x>> =
      (csch.Ch >>=? fun (msgIn, outCh) ->
       let n = System.Threading.Interlocked.Decrement &csch.N
       if n > 0 then
         outCh <-+ Some msgOut >>% Some msgIn
       elif n = 0 then
         csch.Done <-= None >>= fun () ->
         outCh <-+ Some msgOut >>% Some msgIn
       else
         outCh <-+ None >>% None) <|>?
      (Alt.delay <| fun () ->
       let inCh = ch ()
       (csch.Ch <-- (msgOut, inCh) >>=? fun () -> inCh :> Job<_>) <|>?
       csch.Done)

  module Creature =
    let run (swap: Color -> Alt<Option<Color>>) (myColor: Color) : Job<int> =
      let rec loop myColor myMeets =
        swap myColor >>= function
          | None ->
            Job.result myMeets
          | Some otherColor ->
            loop (complement myColor otherColor) (myMeets + 1)
      loop myColor 0

  let bench (colors: array<Color>) numMeets = job {
    let! place = CountedSwapCh.create numMeets
    return! colors
            |> Seq.Con.mapJob (Creature.run (CountedSwapCh.swap place))
            |>> Seq.sum
  }

  let run numMeets =
    printf "Alt:  "
    let timer = Stopwatch.StartNew ()
    let (n, m) = run <| job {
      let! pn = Promise.queue (bench colors10 numMeets)
      let! pm = Promise.start (bench colorsAll numMeets)
      return! pn <&> pm
    }
    let d = timer.Elapsed
    printf "%d %d %fs (%d, %d)\n" numMeets Environment.ProcessorCount d.TotalSeconds n m

/////////////////////////////////////////////////////////////////////////

let cleanup () =
  for i=1 to 5 do
    GC.Collect ()
    Threading.Thread.Sleep 50

do for n in [600; 6000; 60000; 600000; 6000000] do
     HopacLock.run n ; cleanup ()
     HopacMV.run n ; cleanup ()
     HopacAlt.run n ; cleanup ()
