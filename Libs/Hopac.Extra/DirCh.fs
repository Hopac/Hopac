// Copyright (C) by Housemarque, Inc.

namespace Hopac.Extra

open Hopac
open Hopac.Job.Infixes

[<AutoOpen>]
module DirChTypes =
  type InCh<'a> = InCh of Ch<'a>
  type OutCh<'a> = OutCh of Ch<'a>

module DirCh =
  let create () = Ch.create () |>> fun ch -> (InCh ch, OutCh ch)
  let take (InCh ch) = Ch.take ch
  let give (OutCh ch) x = Ch.give ch x
  module Alt =
    let take (InCh ch) = Ch.Alt.take ch
    let give (OutCh ch) x = Ch.Alt.give ch x
