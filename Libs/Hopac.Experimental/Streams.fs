// Copyright (C) by Vesa Karvonen

namespace Hopac.Experimental

open System
open System.Collections.Generic
open Hopac
open Hopac.Infixes
open Hopac.Job.Infixes
open Hopac.Alt.Infixes

type Stream<'x> =
  | Nil
  | Cons of Value: 'x * Next: Alt<Stream<'x>>

type Streams<'x> = Alt<Stream<'x>>

module Streams =
  let inline nil<'x> = Job.result Nil :> Job<Stream<'x>>
  let inline cons x xs = Job.result (Cons (x, xs)) :> Job<Stream<_>>

  let zero<'x> = Alt.always Nil :> Streams<'x>

  let one x = Alt.always (Cons (x, zero<_>)) :> Streams<'x>

  let inline memo x = Promise.Now.delayAsAlt x
  let inline (>>=?*) x f = x >>=? f |> memo
  let inline (|>>?*) x f = x |>>? f |> memo
  let inline (<|>*) x y = x <|> y |> memo

  let rec ofEnum (xs: IEnumerator<'x>) = memo << Job.thunk <| fun () ->
    if xs.MoveNext () then
      Cons (xs.Current, ofEnum xs)
    else
      xs.Dispose ()
      Nil

  let ofSeq (xs: seq<'xs>) = memo << Job.delay <| fun () ->
    upcast ofEnum (xs.GetEnumerator ())

  let rec ofAlt xA = xA |>>?* fun x -> Cons (x, ofAlt xA)

  let rec merge ls rs =
    mergeSwap ls rs <|>* mergeSwap rs ls
  and mergeSwap ls rs =
    ls |>>? function
       | Nil -> Nil
       | Cons (l, ls) -> Cons (l, merge rs ls)

  let rec append (ls: Streams<_>) (rs: Streams<_>) =
    ls >>=?* function
       | Nil -> upcast rs
       | Cons (l, ls) -> cons l (append ls rs)

  let rec choose x2yO xs =
    xs >>=?* function
       | Nil -> nil
       | Cons (x, xs) ->
         match x2yO x with
          | None -> upcast choose x2yO xs
          | Some y -> cons y (choose x2yO xs)

  let rec map x2y xs =
    xs |>>?* function
       | Nil -> Nil
       | Cons (x, xs) -> Cons (x2y x, map x2y xs)

  let rec joinWith (join: Streams<_> -> Streams<_> -> Streams<_>)
                   (xxs: Streams<Streams<_>>) =
    xxs >>=?* function
        | Nil -> nil
        | Cons (xs, xxs) -> upcast join xs (joinWith join xxs)

  let mergeMap x2ys xs = xs |> map x2ys |> joinWith merge
  let appendMap x2ys xs = xs |> map x2ys |> joinWith append

  let rec switchOn ys xs =
    ys <|>*
    xs >>=? function
       | Nil -> nil
       | Cons (x, xs) -> cons x (switchOn ys xs)

  let takeUntil evt = switchOn (evt >>%? Nil)

  let rec catchOnce (e2xs: _ -> Streams<_>) xs =
    Job.tryIn xs
      <| function Nil -> nil
                | Cons (x, xs) -> cons x (catchOnce e2xs xs)
      <| fun e -> upcast e2xs e
    |> memo

  let rec collectLatest (x2ys: 'x -> Streams<'y>) (xs: Streams<'x>) =
    xs >>=?* function
       | Nil -> nil
       | Cons (x, xs) ->
         upcast append (takeUntil xs (x2ys x)) (collectLatest x2ys xs)

  let rec throttle timeout xs =
    xs >>=?* function
       | Nil -> nil
       | Cons (x, xs) -> throttleGot1 timeout xs x
  and throttleGot1 timeout xs x =
    (timeout >>=? fun _ -> cons x (throttle timeout xs)) <|>
    (xs >>=? function
        | Nil -> cons x zero
        | Cons (x, xs) -> throttleGot1 timeout xs x)

  let rec zipXY f x xs y ys =
    let gotx = xs >>=? function Nil -> nil | Cons (x, xs) -> zipXY f x xs y ys
    let goty = ys >>=? function Nil -> nil | Cons (y, ys) -> zipXY f x xs y ys
    cons (f x y) (gotx <|>* goty)
  let zipX f x xs ys = ys >>=? function Nil -> nil | Cons (y, ys) -> zipXY f x xs y ys
  let zipY f xs y ys = xs >>=? function Nil -> nil | Cons (x, xs) -> zipXY f x xs y ys
  let zipWith f xs ys =
    let gotx = xs >>=? function Nil -> nil | Cons (x, xs) -> upcast zipX f x xs ys
    let goty = ys >>=? function Nil -> nil | Cons (y, ys) -> upcast zipY f xs y ys
    gotx <|>* goty
