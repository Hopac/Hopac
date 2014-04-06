// Copyright (C) by Housemarque, Inc.

namespace Models

module ActorModel =
  type ActorThread<'a, 'x>
  val (>>=): ActorThread<'a, 'x> -> ('x -> ActorThread<'a, 'y>) -> ActorThread<'a, 'y>
  val result: 'x -> ActorThread<'a, 'x>
  val receive: ActorThread<'a, 'a>
  type Actor<'a>
  val self: ActorThread<'a, Actor<'a>>
  val start: ActorThread<'a, unit> -> Actor<'a>
  val send: Actor<'a> -> 'a -> unit

module HopacModel =
  type Job<'x>
  val (>>=): Job<'x> -> ('x -> Job<'y>) -> Job<'y>
  val result: 'x -> Job<'x>
  val start: Job<unit> -> unit
  type Ch<'x>
  val ch: unit -> Ch<'x>
  val give: Ch<'x> -> 'x -> Job<unit>
  val take: Ch<'x> -> Job<'x>
