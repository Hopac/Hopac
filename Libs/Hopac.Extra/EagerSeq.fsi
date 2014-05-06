// Copyright (C) by Housemarque, Inc.

namespace Hopac.Extra

open Hopac

/// A type of sequence where the producer is being run eagerly and the results
/// it produces are memoized.
type EagerSeq<'x>

/// Operations on eager sequences.
module EagerSeq =

  val choose: ('x -> Job<option<'y>>) -> EagerSeq<'x> -> Job<EagerSeq<'y>>

  val collect: Job<option<'x>> -> Job<EagerSeq<'x>>

  val filter: ('x -> bool) -> EagerSeq<'x> -> Job<EagerSeq<'x>>

  val iter: ('x -> Job<_>) -> EagerSeq<'x> -> Job<unit>

  val map: ('x -> Job<'y>) -> EagerSeq<'x> -> Job<EagerSeq<'y>>

  val toSeq: EagerSeq<'x> -> Job<ResizeArray<'x>>

  val unfold: ('s -> Job<option<'x * 's>>) -> 's -> Job<EagerSeq<'x>>
