// Copyright (C) by Vesa Karvonen

namespace Hopac.Experimental

open System
open Hopac

/// Represents a discrete event stream.
type Alts<'x>

/// Operations for programming with discrete event streams.
module Alts =
  val consume: onNext: ('x -> Job<unit>) -> Alts<'x> -> Job<unit>

  val zero: unit -> Alts<'x>

  val result: 'x -> Alts<'x>

  val merge: Alts<'x> -> Alts<'x> -> Alts<'x>

  val bindJob: Alts<'x> -> ('x -> Job<Alts<'y>>) -> Alts<'y>
  val bindFun: Alts<'x> -> ('x ->     Alts<'y> ) -> Alts<'y>

  val mapJob: ('x -> Job<'y>) -> Alts<'x> -> Alts<'y>
  val mapFun: ('x ->     'y ) -> Alts<'x> -> Alts<'y>

  val noDups: Alts<'x> -> Alts<'x> when 'x : equality

  val throttle: timeOut: Alt<unit> -> Alts<'x> -> Alts<'x>

  val foldJob: ('s -> 'x -> Job<'s>) -> 's -> Alts<'x> -> Alts<'s>
  val foldFun: ('s -> 'x ->     's ) -> 's -> Alts<'x> -> Alts<'s>
