// Copyright (C) by Housemarque, Inc.

namespace Hopac.Extra

open Hopac

/// Operations for programming with (synchronous) streams.  Both the inputs and
/// the outputs of these stream combinators are given explicitly so that
/// streams can be combined flexibly.
module Stream =
  /// For flexibility, input to stream combinators is represented as
  /// alternatives.  Typically the alternative takes a message on a
  /// channel, but other alternatives can be used.
  type In<'x> = Alt<'x>

  /// For flexibility, output of stream combinators is represented as action
  /// alternative constructors.  Typically the alternative gives a message on a
  /// channel.
  type Out<'x> = 'x -> Alt<unit>

  /// Creates a new channel and passes it as the output action to the given
  /// partially applied stream.
  val inline imp: (Out<'x> -> Job<unit>) -> Job<In<'x>>

  val filterFun: ('x ->     bool ) -> In<'x> -> Out<'x> -> Job<unit>
  val filterJob: ('x -> Job<bool>) -> In<'x> -> Out<'x> -> Job<unit>

  val iterateFun: 'x -> ('x ->     'x ) -> Out<'x> -> Job<unit>
  val iterateJob: 'x -> ('x -> Job<'x>) -> Out<'x> -> Job<unit>

  val mapFun: ('x ->     'y ) -> In<'x> -> Out<'y> -> Job<unit>
  val mapJob: ('x -> Job<'y>) -> In<'x> -> Out<'y> -> Job<unit>

  val sumWithFun: ('x -> 'y ->     'z ) -> In<'x> -> In<'y> -> Out<'z> -> Job<unit>
  val sumWithJob: ('x -> 'y -> Job<'z>) -> In<'x> -> In<'y> -> Out<'z> -> Job<unit>
