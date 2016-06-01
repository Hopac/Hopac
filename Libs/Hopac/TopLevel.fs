// Copyright (C) by Vesa Karvonen

namespace Hopac

open System

[<AutoOpen>]
module TopLevel =
  type Stream<'x> = Stream.Stream<'x>

  let job = JobBuilder ()
  let onMain = Extensions.Async.Global.onMain ()

  let inline run x = Job.Global.run x
  let inline startIgnore x = Job.Global.startIgnore x
  let inline startDelay x = Job.Global.startIgnore <| Job.delay x
  let inline start x = Job.Global.start x
  let inline queueIgnore x = Job.Global.queueIgnore x
  let inline queueDelay x = Job.Global.queueIgnore <| Job.delay x
  let inline queue x = Job.Global.queue x
  let inline server x = Job.Global.server x

  let inline asAlt (xA: Alt<'x>) = xA
  let inline asJob (xJ: Job<'x>) = xJ

  let inline timeOut x = Timer.Global.timeOut x
  let inline timeOutMillis x = Timer.Global.timeOutMillis x

  let inline memo x = Promise.Now.delay x
