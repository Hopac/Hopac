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

  [<Obsolete "Use `Ch ()` rather than `ch ()`.">]
  let inline ch () = Ch<'x> ()
  [<Obsolete "Use `Mailbox ()` rather than `mb ()`.">]
  let inline mb () = Mailbox<'x> ()
  [<Obsolete "Use `IVar ()` rather than `ivar ()`.">]
  let inline ivar () = IVar<'x> ()
  [<Obsolete "Use `IVar (x)` rather than `ivarFull ()`.">]
  let inline ivarFull x = IVar.Now.createFull x
  [<Obsolete "Use `MVar ()` rather than `mvar ()`.">]
  let inline mvar () = MVar<'x> ()
  [<Obsolete "Use `MVar (x)` rather than `mvarFull x`.">]
  let inline mvarFull x = MVar.Now.createFull x
