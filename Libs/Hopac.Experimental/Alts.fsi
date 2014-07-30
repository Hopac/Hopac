// Copyright (C) by Vesa Karvonen

namespace Hopac.Experimental

open System
open Hopac

/// Represents a concurrent stream.
type Alts<'x>

/// Operations for programming with concurrent streams.
module Alts =
  val consume: onNext: ('x -> Job<unit>)
            -> onError: (exn -> Job<unit>)
            -> onDone: Job<unit>
            -> Alts<'x>
            -> Job<unit>

  val zero: unit -> Alts<'x>

  val result: 'x -> Alts<'x>

  val merge: Alts<'x> -> Alts<'x> -> Alts<'x>

  val bindFun: Alts<'x> -> ('x -> Alts<'y>) -> Alts<'y>

  val mapJob: ('x -> Job<'y>) -> Alts<'x> -> Alts<'y>
  val mapFun: ('x ->     'y ) -> Alts<'x> -> Alts<'y>

  val noDups: Alts<'x> -> Alts<'x> when 'x : equality

  val throttle: TimeSpan -> Alts<'x> -> Alts<'x>

  val foldFun: ('s -> 'x -> 's) -> 's -> Alts<'x> -> Alts<'s>
