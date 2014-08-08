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

  val processJob: onFirst:      ('x -> Job<'s * option<'y>>)
               -> onNext: ('s -> 'x -> Job<'s * option<'y>>)
               -> Alts<'x>
               -> Alts<'y>

  val scanJob: ('s -> 'x -> Job<'s>) -> 's -> Alts<'x> -> Alts<'s>
  val scanFun: ('s -> 'x ->     's ) -> 's -> Alts<'x> -> Alts<'s>

  val chooseJob: ('x -> Job<option<'y>>) -> Alts<'x> -> Alts<'y>

  val throttle: timeOut: Alt<unit> -> Alts<'x> -> Alts<'x>

  val mapJob: ('x -> Job<'y>) -> Alts<'x> -> Alts<'y>
  val mapFun: ('x ->     'y ) -> Alts<'x> -> Alts<'y>

  val noDups: Alts<'x> -> Alts<'x> when 'x : equality
