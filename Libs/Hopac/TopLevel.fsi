// Copyright (C) by Vesa Karvonen

namespace Hopac

open System
open Hopac

////////////////////////////////////////////////////////////////////////////////

/// Convenience bindings for programming with Hopac.
[<AutoOpen>]
module Hopac =
  /// Represents a non-deterministic stream of values called a choice stream.
  type Stream<'x> = Stream.Stream<'x>

  //# Computation expression builders

  /// Default expression builder for jobs.
  val job: JobBuilder

  /// Builder for running an async workflow on the main synchronization context
  /// and interoperating with Hopac.  The application must call
  /// `Hopac.Extensions.Async.setMain` to configure Hopac with the main
  /// synchronization context.
#if DOC
  ///
  /// The builder has been constructed by calling
  /// `Hopac.Extensions.Async.Global.onMain ()`.
#endif
  val onMain: Extensions.Async.OnWithSchedulerBuilder

  //# Spawning jobs

  /// Queues the given job for execution.  See also: `start`, `server`.
#if DOC
  ///
  /// Note that using this function in a job workflow is not optimal and you
  /// should use `Job.queue` instead.
#endif
  val inline queue:                Job<unit> -> unit

  /// Queues the given job for execution.  `queueIgnore xJ` is equivalent to
  /// `Job.Ignore xJ |> queue`.
  val inline queueIgnore:          Job<_>    -> unit

  /// Queues the given delayed job for execution.  `queueDelay u2xJ` is
  /// equivalent to `queueIgnore <| Job.delay u2xJ`.
  val inline queueDelay: (unit -> #Job<_>)   -> unit

  /// Starts running the given job like `start`, but the given job is known
  /// never to return normally, so the job can be spawned in an even more
  /// lightweight manner.
#if DOC
  ///
  /// Note that using this function in a job workflow is not optimal and you
  /// should use `Job.server` instead.
#endif
  val inline server:               Job<Void> -> unit

  /// Starts running the given job, but does not wait for the job to finish.
  /// See also: `queue`, `server`.
#if DOC
  ///
  /// Note that using this function in a job workflow is not optimal and you
  /// should use `Job.start` instead.
#endif
  val inline start:                Job<unit> -> unit

  /// Starts running the given job, but does not wait for the job to finish.
  /// `startIgnore xJ` is equivalent to `Job.Ignore xJ |> start`.
  val inline startIgnore:          Job<_>    -> unit

  /// Starts running the given delayed job, but does not wait for the job to
  /// finish.  `startDelay u2xJ` is equivalent to `startIgnore <| Job.delay
  /// u2xJ`.
  val inline startDelay: (unit -> #Job<_>)   -> unit

  /// Starts running the given job, but does not wait for the job to finish.
  /// Upon the failure or success of the job, one of the given actions is called
  /// once.
#if DOC
  ///
  /// Note that using this function in a job workflow is not optimal and you
  /// should instead use `Job.start` with the desired exception handling
  /// construct (e.g. `Job.tryIn` or `Job.catch`).
#endif
  val inline startWithActions: (exn -> unit) -> ('x -> unit) -> Job<'x> -> unit

  /// Starts running the given job and then blocks the current thread waiting
  /// for the job to either return successfully or fail.  See also: `start`.
#if DOC
  ///
  /// WARNING: Use of `run` should be considered carefully, because calling
  /// `run` from an arbitrary thread can cause deadlock.
  ///
  /// `run` is mainly provided for conveniently running Hopac code from F#
  /// Interactive and can also be used as an entry point to the Hopac runtime in
  /// console applications.  In Windows applications, for example, `run` should
  /// not be called from the GUI thread.
  ///
  /// A call of `run xJ` is safe when the call is not made from within a Hopac
  /// worker thread and the job `xJ` does not perform operations that might
  /// block or that might directly, or indirectly, need to communicate with the
  /// thread from which `run` is being called.
  ///
  /// Note that using this function from within a job workflow should never be
  /// needed, because within a workflow the result of a job can be obtained by
  /// binding.
#endif
  val inline run: Job<'x> -> 'x

  //# Timeouts

  /// Creates an alternative that, after instantiation, becomes available after
  /// the specified time span.
#if DOC
  ///
  /// Note that the timer mechanism is simply not intended for high precision
  /// timing and the resolution of the underlying mechanism is very coarse
  /// (Windows system ticks).
  ///
  /// Also note that you do not need to create a new timeout alternative every
  /// time you need a timeout with a specific time span.  For example, you can
  /// create a timeout for one second
  ///
  ///> let after1s = timeOut <| TimeSpan.FromSeconds 1.0
  ///
  /// and then use that timeout many times
  ///
  ///>     makeRequest ^=> fun rp -> ...
  ///> <|> after1s     ^=> fun () -> ...
  ///
  /// Timeouts, like other alternatives, can also directly be used as job level
  /// operations.  For example, using the above definition of `after1s`
  ///
  ///> after1s >>= fun () -> ...
  ///
  /// has the effect of sleeping for one second.
  ///
  /// It is an idiomatic approach with Hopac to rely on garbage collection to
  /// clean up concurrent jobs than can no longer make progress.  It is
  /// therefore important to note that a server loop
  ///
  ///> let rec serverLoop ... =
  ///>       ...
  ///>   <|> timeOut ... ^=> ... serverLoop ...
  ///>   <|> ...
  ///
  /// that always waits for a timeout is held live by the timeout.  Such servers
  /// need to support an explicit kill protocol.
  ///
  /// When a timeout is used as a part of a non-deterministic choice, e.g.
  /// `timeOut span <|> somethingElse`, and some other alternative is committed
  /// to before the timeout expires, the memory held by the timeout can be
  /// released by the timer mechanism.  However, when a timeout is not part of a
  /// non-deterministic choice, e.g.
  ///
  ///> timeOut span >>=. gotTimeout *<= () |> start
  ///
  /// no such clean up can be performed.  If there is a possibility that such
  /// timeouts are kept alive beyond their usefulness, it may be possible to
  /// arrange for the timeouts to be released by making them part of a
  /// non-deterministic choice:
  ///
  ///>     timeOut span ^=> IVar.tryFill gotTimeoutOrDoneOtherwise
  ///> <|> gotTimeoutOrDoneOtherwise
  ///>  |> start
  ///
  /// The idea is that the `gotTimeoutOrDoneOtherwise` is filled, using
  /// `IVar.tryFill` as soon as the timeout is no longer useful.  This allows
  /// the timer mechanism to release the memory held by the timeout.
#endif
  val inline timeOut:       TimeSpan -> Alt<unit>

  /// `timeOutMillis n` is equivalent to `timeOut << TimeSpan.FromMilliseconds
  /// <| float n`.
  val inline timeOutMillis: int      -> Alt<unit>

  //# Promises

  /// Creates a promise whose value is computed lazily with the given job when
  /// an attempt is made to read the promise.  This is the same function as
  /// `Promise.Now.delay`.
  val inline memo: Job<'x> -> Promise<'x>

  //# Type ascription helpers

  /// Use object as alternative.  This function is a NOP and is provided as a
  /// kind of syntactic alternative to using a type ascription or an `upcast`.
  val inline asAlt: Alt<'x> -> Alt<'x>

  /// Use object as job.  This function is a NOP and is provided as a kind of
  /// syntactic alternative to using a type ascription or an `upcast`.
  val inline asJob: Job<'x> -> Job<'x>

[<Obsolete "`TopLevel` has been renamed as `Hopac`.">]
module TopLevel =
  [<Obsolete "`TopLevel` has been renamed as `Hopac`.">]
  type Stream<'x> = Stream.Stream<'x>
  [<Obsolete "`TopLevel` has been renamed as `Hopac`.">]
  val job: JobBuilder
  [<Obsolete "`TopLevel` has been renamed as `Hopac`.">]
  val onMain: Extensions.Async.OnWithSchedulerBuilder
  [<Obsolete "`TopLevel` has been renamed as `Hopac`.">]
  val inline queue:                Job<unit> -> unit
  [<Obsolete "`TopLevel` has been renamed as `Hopac`.">]
  val inline queueIgnore:          Job<_>    -> unit
  [<Obsolete "`TopLevel` has been renamed as `Hopac`.">]
  val inline queueDelay: (unit -> #Job<_>)   -> unit
  [<Obsolete "`TopLevel` has been renamed as `Hopac`.">]
  val inline server:               Job<Void> -> unit
  [<Obsolete "`TopLevel` has been renamed as `Hopac`.">]
  val inline start:                Job<unit> -> unit
  [<Obsolete "`TopLevel` has been renamed as `Hopac`.">]
  val inline startIgnore:          Job<_>    -> unit
  [<Obsolete "`TopLevel` has been renamed as `Hopac`.">]
  val inline startDelay: (unit -> #Job<_>)   -> unit
  [<Obsolete "`TopLevel` has been renamed as `Hopac`.">]
  val inline run: Job<'x> -> 'x
  [<Obsolete "`TopLevel` has been renamed as `Hopac`.">]
  val inline timeOut:       TimeSpan -> Alt<unit>
  [<Obsolete "`TopLevel` has been renamed as `Hopac`.">]
  val inline timeOutMillis: int      -> Alt<unit>
  [<Obsolete "`TopLevel` has been renamed as `Hopac`.">]
  val inline memo: Job<'x> -> Promise<'x>
  [<Obsolete "`TopLevel` has been renamed as `Hopac`.">]
  val inline asAlt: Alt<'x> -> Alt<'x>
  [<Obsolete "`TopLevel` has been renamed as `Hopac`.">]
  val inline asJob: Job<'x> -> Job<'x>
