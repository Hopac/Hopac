// Copyright (C) by Housemarque, Inc.

namespace Hopac.Extra

open Hopac

[<AutoOpen>]
module SelectableQueueTypes =
  type SelectableQueue<'a>

module SelectableQueue =
  val create: Unit -> Job<SelectableQueue<'a>>
  module Alt =
    val send: SelectableQueue<'a> -> 'a -> Alt<unit>
    val take: SelectableQueue<'a> -> ('a -> bool) -> Alt<'a>
