// Copyright (C) by Vesa Karvonen

namespace Hopac.Experimental

open System
open Hopac

type Stream<'x> =
  | Nil
  | Cons of Value: 'x * Next: Alt<Stream<'x>>

type Streams<'x> = Alt<Stream<'x>>

module Streams =
  val zero<'x> : Streams<'x>
  val one: 'x -> Streams<'x>

  val ofSeq: seq<'x> -> Streams<'x>
  val ofAlt: Alt<'x> -> Streams<'x>

  val subscribingTo: IObservable<'x> -> (Streams<'x> -> Job<'y>) -> Job<'y>

  val toObservable: Streams<'x> -> IObservable<'x>

  val merge: Streams<'x> -> Streams<'x> -> Streams<'x>
  val append: Streams<'x> -> Streams<'x> -> Streams<'x>

  val choose: ('x -> option<'y>) -> Streams<'x> -> Streams<'y>

  val map: ('x -> 'y) -> Streams<'x> -> Streams<'y>

  val joinWith: (Streams<'x> -> Streams<'y> -> Streams<'y>)
             -> Streams<Streams<'x>> -> Streams<'y>

  val mergeMap: ('x -> Streams<'y>) -> Streams<'x> -> Streams<'y>
  val appendMap: ('x -> Streams<'y>) -> Streams<'x> -> Streams<'y>

  val switchOn: Streams<'x> -> Streams<'x> -> Streams<'x>

  val takeUntil: Alt<_> -> (Streams<'x> -> Streams<'x>)

  val catchOnce: (exn -> Streams<'x>) -> Streams<'x> -> Streams<'x>

  val collectLatest: ('x -> Streams<'y>) -> Streams<'x> -> Streams<'y>

  val throttle: Alt<_> -> Streams<'x> -> Streams<'x>

  val zipWith: ('x -> 'y -> 'z) -> Streams<'x> -> Streams<'y> -> Streams<'z>
