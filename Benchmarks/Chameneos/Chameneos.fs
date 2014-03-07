// Copyright (C) by Housemarque, Inc.

module Chameneos

open Hopac
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
    new (color) = {Color = color; WakeUp = MVar.Now.create ()}

  type MeetingPlace =
    inherit Lock
    val mutable NumMeets: int
    val mutable Waiter: Chameneos
    new (numMeets) = {inherit Lock (); NumMeets = numMeets; Waiter = null}
  
  let bench (colors: array<Color>) numMeets = Job.delay <| fun () ->
    let mp = MeetingPlace (numMeets)
    let resultsMS = Ch.Now.create ()
    colors
    |> Seq.iterJob (fun myColor ->
       let me = Chameneos (myColor)
       let myMeets = ref 0
       let cont = ref true
       Job.start
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
                MVar.take me.WakeUp |>> fun otherColor ->
                myMeets := !myMeets + 1
                me.Color <- complement me.Color otherColor
              else
                Ch.give resultsMS (!myMeets)
            | other ->
              let otherColor = other.Color
              MVar.fill other.WakeUp me.Color |>> fun () ->
              myMeets := !myMeets + 1
              me.Color <- complement me.Color otherColor))) >>= fun () ->
    Seq.foldJob
     (fun sum _ -> Ch.take resultsMS |>> fun n -> sum+n)
     0
     (seq {1 .. colors.Length})

  let run numMeets =
    printf "Lock: "
    let timer = Stopwatch.StartNew ()
    let (n, m) = run (bench colorsAll numMeets <*> bench colors10 numMeets)
    let d = timer.Elapsed
    printf "%d %d %fs (%d, %d)\n" numMeets Environment.ProcessorCount d.TotalSeconds n m

/////////////////////////////////////////////////////////////////////////

module HopacMV =
  type Chameneos =
    | Chameneos of Color * MVar<Chameneos>

  type MeetingPlace =
    | Empty of int
    | Waiter of int * Chameneos

  let bench (colors: array<Color>) numMeets =
    MVar.create () >>= fun meetingPlace ->
    MVar.fill meetingPlace (Empty numMeets) >>= fun () ->
    Ch.create () >>= fun resultsMS ->
    colors
    |> Seq.iterJob (fun myColor ->
       MVar.create () >>= fun me ->
       let myMeets = ref 0
       let myColor = ref myColor
       let cont = ref true
       Job.start
        (Job.whileDo (fun () -> !cont)
          (MVar.take meetingPlace >>= function
            | (Empty 0) as state ->
              MVar.fill meetingPlace state >>= fun () ->
              cont := false
              Ch.give resultsMS (!myMeets)
            | Empty n ->
              MVar.fill meetingPlace (Waiter (n, Chameneos (!myColor, me))) >>= fun () ->
              MVar.take me |>> fun (Chameneos (otherColor, other)) ->
              myMeets := !myMeets + 1
              myColor := complement (!myColor) otherColor
            | Waiter (n, Chameneos (otherColor, other)) ->
              MVar.fill other (Chameneos (!myColor, me)) >>= fun () ->
              MVar.fill meetingPlace (Empty (n-1)) |>> fun () ->
              myMeets := !myMeets + 1
              myColor := complement (!myColor) otherColor))) >>= fun () ->
    Seq.foldJob
     (fun sum _ -> Ch.take resultsMS |>> fun n -> sum+n)
     0
     (seq {1 .. colors.Length})

  let run numMeets =
    printf "MVar: "
    let timer = Stopwatch.StartNew ()
    let (n, m) = run (bench colorsAll numMeets <*> bench colors10 numMeets)
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
    let create n : Job<CountedSwapCh<'x>> = job {
      let ch = Ch.Now.create ()
      let d = IVar.Now.create ()
      return {N = n; Ch = ch; Done = d}
    }

    let swap (csch: CountedSwapCh<'x>) (msgOut: 'x) : Alt<Option<'x>> =
      (Ch.Alt.take csch.Ch >=> fun (msgIn, outCh) ->
       let n = System.Threading.Interlocked.Decrement &csch.N
       if n > 0 then
         Ch.send outCh (Some msgOut) >>% Some msgIn
       elif n = 0 then
         IVar.fill csch.Done None >>= fun () ->
         Ch.send outCh (Some msgOut) >>% Some msgIn
       else
         Ch.send outCh None >>% None) <|>
      (Alt.delay <| fun () ->
       let inCh = Ch.Now.create ()
       (Ch.Alt.give csch.Ch (msgOut, inCh) >=> fun () ->
        Ch.take inCh) <|>
       IVar.Alt.read csch.Done)

  module Creature =
    let run (swap: Color -> Alt<Option<Color>>)
            (report: int -> Job<unit>)
            (myColor: Color) =
      let rec loop myColor myMeets =
        swap myColor |> Alt.pick >>= function
          | None ->
            report myMeets
          | Some otherColor ->
            loop (complement myColor otherColor) (myMeets + 1)
      Job.start (loop myColor 0)

  let bench (colors: array<Color>) numMeets finishCh = job {
    let! place = CountedSwapCh.create numMeets
    let results = Ch.Now.create ()
    for color in colors do
      do! Creature.run
           (CountedSwapCh.swap place)
           (Ch.give results)
           color
    let! n =
      Seq.foldJob
       (fun sum _ ->
          Ch.take results |>> fun n ->
          sum+n)
       0
       colors
    do! Ch.give finishCh n
  }

  let run numMeets =
    printf "Alt:  "
    let timer = Stopwatch.StartNew ()
    let (n, m) =
      run <| job {
        let finishCh = Ch.Now.create ()
        do! Job.start (bench colors10 numMeets finishCh)
        do! Job.start (bench colorsAll numMeets finishCh)
        let! n = Ch.take finishCh
        let! m = Ch.take finishCh
        return (n, m)
      }
    let d = timer.Elapsed
    printf "%d %d %fs (%d, %d)\n" numMeets Environment.ProcessorCount d.TotalSeconds n m

/////////////////////////////////////////////////////////////////////////

let cleanup () =
  for i=1 to 10 do
    Runtime.GCSettings.LargeObjectHeapCompactionMode <- Runtime.GCLargeObjectHeapCompactionMode.CompactOnce
    GC.Collect ()
    Threading.Thread.Sleep 50

do for n in [600; 6000; 60000; 600000; 6000000] do
     HopacLock.run n ; cleanup ()
     HopacMV.run n ; cleanup ()
     HopacAlt.run n ; cleanup ()
