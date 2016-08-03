// Copyright (C) by Vesa Karvonen

namespace Hopac

open System

[<AutoOpen>]
module Hopac =
  type Stream<'x> = Stream.Stream<'x>

  let job = JobBuilder ()
  let onMain = Extensions.Async.Global.onMain ()

  let inline run x = Job.Global.run x
  let inline startIgnore x = Job.Global.startIgnore x
  let inline startDelay x = Job.Global.startIgnore <| Job.delay x
  let inline start x = Job.Global.start x
  let inline startWithActions e2u x2u xJ = Job.Global.startWithActions e2u x2u xJ
  let inline queueIgnore x = Job.Global.queueIgnore x
  let inline queueDelay x = Job.Global.queueIgnore <| Job.delay x
  let inline queue x = Job.Global.queue x
  let inline server x = Job.Global.server x

  let inline asAlt (xA: Alt<'x>) = xA
  let inline asJob (xJ: Job<'x>) = xJ

  let inline timeOut x = Timer.Global.timeOut x
  let inline timeOutMillis x = Timer.Global.timeOutMillis x

  let inline memo (xJ: Job<'x>) = Promise<'x> xJ

[<Obsolete "`TopLevel` has been renamed as `Hopac`.">]
module TopLevel =
  [<Obsolete "`TopLevel` has been renamed as `Hopac`.">]
  type Stream<'x> = Stream.Stream<'x>
  [<Obsolete "`TopLevel` has been renamed as `Hopac`.">]
  let job = Hopac.job
  [<Obsolete "`TopLevel` has been renamed as `Hopac`.">]
  let onMain = Hopac.onMain
  [<Obsolete "`TopLevel` has been renamed as `Hopac`.">]
  let inline run x = Hopac.run x
  [<Obsolete "`TopLevel` has been renamed as `Hopac`.">]
  let inline startIgnore x = Hopac.startIgnore x
  [<Obsolete "`TopLevel` has been renamed as `Hopac`.">]
  let inline startDelay x = Hopac.startDelay x
  [<Obsolete "`TopLevel` has been renamed as `Hopac`.">]
  let inline start x = Hopac.start x
  [<Obsolete "`TopLevel` has been renamed as `Hopac`.">]
  let inline queueIgnore x = Hopac.queueIgnore x
  [<Obsolete "`TopLevel` has been renamed as `Hopac`.">]
  let inline queueDelay x = Hopac.queueDelay x
  [<Obsolete "`TopLevel` has been renamed as `Hopac`.">]
  let inline queue x = Hopac.queue x
  [<Obsolete "`TopLevel` has been renamed as `Hopac`.">]
  let inline server x = Hopac.server x
  [<Obsolete "`TopLevel` has been renamed as `Hopac`.">]
  let inline asAlt (xA: Alt<'x>) = xA
  [<Obsolete "`TopLevel` has been renamed as `Hopac`.">]
  let inline asJob (xJ: Job<'x>) = xJ
  [<Obsolete "`TopLevel` has been renamed as `Hopac`.">]
  let inline timeOut x = Hopac.timeOut x
  [<Obsolete "`TopLevel` has been renamed as `Hopac`.">]
  let inline timeOutMillis x = Hopac.timeOutMillis x
  [<Obsolete "`TopLevel` has been renamed as `Hopac`.">]
  let inline memo x = Hopac.memo x
