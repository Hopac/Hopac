// Copyright (C) by Housemarque, Inc.

namespace Misc

open Hopac

module Alts =
  val sumWith: ('x -> 'y -> Job<'z>) -> Alt<'x> -> Alt<'y> -> Alt<'z>

module BufferedChViaPick =
  type Buffer<'a>
  val create: unit -> Job<Buffer<'a>>
  val insert: Buffer<'a> -> 'a -> Job<unit>
  val remove: Buffer<'a> -> Job<'a>
