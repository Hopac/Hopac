// Copyright (C) by Housemarque, Inc.

namespace Hopac.Experimental

open Hopac

/// A type of sequence where the producer is being run eagerly and the results
/// it produces are memoized.
type EagerSeq<'x>

/// Operations on eager sequences.
module EagerSeq =
  /// Immediate or non-workflow operations on eager sequences.
  module Now =
    /// Creates an empty sequence.
    val empty: unit -> EagerSeq<'x>

    /// Creates a sequence containing the given element.
    val singleton: 'x -> EagerSeq<'x>

  /// XXX
  val collectJob: ('x -> Job<EagerSeq<'y>>) -> EagerSeq<'x> -> Job<EagerSeq<'y>>

  /// XXX
  val chooseFun: ('x -> option<'y>) -> EagerSeq<'x> -> Job<EagerSeq<'y>>
  /// XXX
  val chooseJob: ('x -> Job<option<'y>>) -> EagerSeq<'x> -> Job<EagerSeq<'y>>

  /// XXX
  val filterFun: ('x -> bool) -> EagerSeq<'x> -> Job<EagerSeq<'x>>

  /// XXX
  val generateFun: (unit -> option<'x>) -> Job<EagerSeq<'x>>
  /// XXX
  val generateJob: Job<option<'x>> -> Job<EagerSeq<'x>>

  /// XXX
  val iterFun: ('x -> unit) -> EagerSeq<'x> -> Job<unit>
  /// XXX
  val iterJob: ('x -> Job<_>) -> EagerSeq<'x> -> Job<unit>

  /// XXX
  val mapFun: ('x -> 'y) -> EagerSeq<'x> -> Job<EagerSeq<'y>>
  /// XXX
  val mapJob: ('x -> Job<'y>) -> EagerSeq<'x> -> Job<EagerSeq<'y>>

  /// XXX
  val ofSeq: seq<'x> -> Job<EagerSeq<'x>>

  /// XXX
  val toSeq: EagerSeq<'x> -> Job<ResizeArray<'x>>

  /// XXX
  val tryPickFun: ('x -> option<'y>) -> EagerSeq<'x> -> Job<option<'y>>

  /// XXX
  val unfoldFun: ('s -> option<'x * 's>) -> 's -> Job<EagerSeq<'x>>
  /// XXX
  val unfoldJob: ('s -> Job<option<'x * 's>>) -> 's -> Job<EagerSeq<'x>>
