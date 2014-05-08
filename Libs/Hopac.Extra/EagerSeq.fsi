// Copyright (C) by Housemarque, Inc.

namespace Hopac.Extra

open Hopac

/// A type of sequence where the producer is being run eagerly and the results
/// it produces are memoized.
type EagerSeq<'x>

/// Operations on eager sequences.
module EagerSeq =
  module Now =
    val empty: unit -> EagerSeq<'x>
    val singleton: 'x -> EagerSeq<'x>

  val collectJob: ('x -> Job<EagerSeq<'y>>) -> EagerSeq<'x> -> Job<EagerSeq<'y>>

  val chooseFun: ('x -> option<'y>) -> EagerSeq<'x> -> Job<EagerSeq<'y>>
  val chooseJob: ('x -> Job<option<'y>>) -> EagerSeq<'x> -> Job<EagerSeq<'y>>

  val filterFun: ('x -> bool) -> EagerSeq<'x> -> Job<EagerSeq<'x>>

  val generateFun: (unit -> option<'x>) -> Job<EagerSeq<'x>>
  val generateJob: Job<option<'x>> -> Job<EagerSeq<'x>>

  val iterFun: ('x -> unit) -> EagerSeq<'x> -> Job<unit>
  val iterJob: ('x -> Job<_>) -> EagerSeq<'x> -> Job<unit>

  val mapFun: ('x -> 'y) -> EagerSeq<'x> -> Job<EagerSeq<'y>>
  val mapJob: ('x -> Job<'y>) -> EagerSeq<'x> -> Job<EagerSeq<'y>>

  val ofSeq: seq<'x> -> Job<EagerSeq<'x>>

  val toSeq: EagerSeq<'x> -> Job<ResizeArray<'x>>

  val tryPickFun: ('x -> option<'y>) -> EagerSeq<'x> -> Job<option<'y>>

  val unfoldFun: ('s -> option<'x * 's>) -> 's -> Job<EagerSeq<'x>>
  val unfoldJob: ('s -> Job<option<'x * 's>>) -> 's -> Job<EagerSeq<'x>>
