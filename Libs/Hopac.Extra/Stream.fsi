// Copyright (C) by Housemarque, Inc.

namespace Hopac.Extra

open Hopac

module Ch =
  module Stream =
    val inline imp: (Ch<'x> -> Job<unit>) -> Job<Ch<'x>>
    val sumWith: ('x -> 'y -> Job<'z>) -> Ch<'x> -> Ch<'y> -> Ch<'z> -> Job<unit>
    val iterate: 'x -> ('x -> Job<'x>) -> Ch<'x> -> Job<unit>
    val filter: ('x -> Job<bool>) -> Ch<'x> -> Ch<'x> -> Job<unit>
