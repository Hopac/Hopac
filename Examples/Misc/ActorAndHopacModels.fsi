// Copyright (C) by Housemarque, Inc.

namespace Models

module ActorModel =
  type AT<'a, 'x>
  val (>>=): AT<'a, 'x> -> ('x -> AT<'a, 'y>) -> AT<'a, 'y>
  val result: 'x -> AT<'a, 'x>
  val receive: AT<'a, 'a>
  type Actor<'a>
  val self: AT<'a, Actor<'a>>
  val start: AT<'a, unit> -> Actor<'a>
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
