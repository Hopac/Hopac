// Copyright (C) by Vesa Karvonen

namespace Hopac

open System

[<AutoOpen>]
module Hopac =
  type Stream<'x> = Stream.Stream<'x>

  let job = JobBuilder ()
  let onMain = Extensions.Async.Global.onMain ()

  let run x = Job.Global.run x
  let inline runDelay u2xJ = run <| Job.delay u2xJ
  let startIgnore x = Job.Global.startIgnore x
  let startDelay x = Job.Global.startIgnore <| Job.delay x
  let start x = Job.Global.start x
  let queueIgnore x = Job.Global.queueIgnore x
  let queueDelay x = Job.Global.queueIgnore <| Job.delay x
  let queue x = Job.Global.queue x
  let server x = Job.Global.server x

  let queueAsTask x = Job.Global.queueAsTask x
  let startAsTask x = Job.Global.startAsTask x
  let startWithActions e2u x2u xJ = Job.Global.startWithActions e2u x2u xJ

  let inline asAlt (xA: Alt<'x>) = xA
  let inline asJob (xJ: Job<'x>) = xJ

  let timeOut x = Timer.Global.timeOut x
  let timeOutMillis x = Timer.Global.timeOutMillis x
  let idle = Timer.Global.idle

  let inline memo (xJ: Job<'x>) = Promise<'x> xJ
