// Copyright (C) by Housemarque, Inc.

namespace Hopac.Experimental

open Hopac
open Hopac.Infixes

type InCh<'a> = InCh of Ch<'a>
type OutCh<'a> = OutCh of Ch<'a>

module DirCh =
  module Now =
    let create () =
      let xCh = Ch ()
      (InCh xCh, OutCh xCh)
  let create () = Job.thunk Now.create
  let take (InCh ch) = asAlt ch
  let give (OutCh ch) x = ch *<- x
