// Copyright (C) by Housemarque, Inc.

namespace Hopac.Extra

open Hopac

[<AutoOpen>]
module SerializerTypes =
  type Serializer

module Serializer =
  val create: Job<Serializer>
  val serialize: Serializer -> Job<'a> -> Job<'a>
