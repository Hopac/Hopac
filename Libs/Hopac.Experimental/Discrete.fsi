// Copyright (C) by Vesa Karvonen

namespace Hopac.Experimental

open Hopac

module Alt =
  /// WARNING: This module currently does not work properly in the sense that
  /// the combinators create space leaks.  It is possible to eliminate the space
  /// leak, by modifying Hopac, but this has not yet been done.
  module Discrete =
    val merge: Alt<'x> -> Alt<'x> -> Alt<'x>
    val mergeMap: ('x -> Alt<'y>) -> Alt<'x> -> Alt<'y>
    val choose: ('x -> option<'y>) -> Alt<'x> -> Alt<'y>
    val filter: ('x -> bool) -> Alt<'x> -> Alt<'x>
    val map: ('x -> 'y) -> Alt<'x> -> Alt<'y>
    val throttle: timeout: Alt<_> -> Alt<'x> -> Alt<'x>
