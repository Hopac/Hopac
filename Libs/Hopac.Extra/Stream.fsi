// Copyright (C) by Housemarque, Inc.

namespace Hopac.Extra

open Hopac

/// Operations for programming with (synchronous) streams.  Both the inputs and
/// the outputs of these stream combinators are given explicitly so that
/// streams can be combined flexibly.
module Stream =
  /// For flexibility, input to stream combinators is represented as
  /// alternatives.  Typically the alternative takes a message on a channel, but
  /// other alternatives can also be used.
  type In<'x> = Alt<'x>

  /// For flexibility, output of stream combinators is represented as action
  /// job constructors.  Typically the job gives a message on a channel, but
  /// other actions can also be used.
  type Out<'x> = 'x -> Job<unit>

  /// Creates a new channel and passes it as the output action to the given
  /// partially applied stream.
  val inline imp: (Out<'x> -> #Job<unit>) -> Job<In<'x>>

  /// Creates a server that forwards those messages from the given input to the
  /// output that satisfy the given predicate.
  val filterFun: ('x -> bool) -> In<'x> -> Out<'x> -> Job<unit>
  
  /// Creates a server that forwards those messages from the given input to the
  /// output that satisfy the given predicate.
  val filterJob: ('x -> #Job<bool>) -> In<'x> -> Out<'x> -> Job<unit>

  /// Creates a server that iterates the given operation to create a stream of
  /// messages.
  val iterateFun: 'x -> ('x -> 'x) -> Out<'x> -> Job<unit>

  /// Creates a server that iterates the given operation to create a stream of
  /// messages.
  val iterateJob: 'x -> ('x -> #Job<'x>) -> Out<'x> -> Job<unit>

  /// Creates a server that maps messages from the given input to the given
  /// output with the given operation.
  val mapFun: ('x -> 'y ) -> In<'x> -> Out<'y> -> Job<unit>
  
  /// Creates a server that maps messages from the given input to the given
  /// output with the given operation.
  val mapJob: ('x -> #Job<'y>) -> In<'x> -> Out<'y> -> Job<unit>

  /// Creates a server that maps messages from the given pair of inputs to the
  /// given output with the given operation.
  val sumWithFun: ('x -> 'y -> 'z) -> In<'x> -> In<'y> -> Out<'z> -> Job<unit>

  /// Creates a server that maps messages from the given pair of inputs to the
  /// given output with the given operation.
  val sumWithJob: ('x -> 'y -> #Job<'z>) -> In<'x> -> In<'y> -> Out<'z> -> Job<unit>
