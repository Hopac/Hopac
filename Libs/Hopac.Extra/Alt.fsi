// Copyright (C) by Housemarque, Inc.

/// The Hopac.Extra library provides additional utilities for programming with
/// Hopac.
namespace Hopac.Extra

open Hopac

/// Additional operations on alternatives.
module Alt =
  /// Returns a new alternative that upon picking time makes it so that the
  /// given job will be started if the given alternative isn't the one being
  /// picked.  Note that "wrapAbort" is a derived operation and is implemented
  /// via "withNack".
  val wrapAbort: Job<unit> -> Alt<'x> -> Alt<'x>

  /// Additional infix operators on alternatives.
  module Infixes =
    /// An alternative that is equivalent to first picking either one of the
    /// given alternatives and then picking the other alternative.  Note that
    /// this is not the same as picking the alternatives in a single
    /// transaction.  Such an operation would require a more complex
    /// synchronization protocol like with the so called Transactional Events.
    val (<+>): Alt<'a> -> Alt<'b> -> Alt<'a * 'b>
