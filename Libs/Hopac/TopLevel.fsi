// Copyright (C) by Vesa Karvonen

namespace Hopac

open System
open Hopac

/// Convenience bindings for programming with Hopac.
[<AutoOpen>]
module TopLevel =
  /// Represents a non-deterministic stream of values called a choice stream.
  type Stream<'x> = Stream.Stream<'x>

  /// Default expression builder for jobs.
  val job: JobBuilder

  /// Builder for running an async workflow on the main synchronization context
  /// and interoperating with the Hopac global scheduler.  The application must
  /// call `Hopac.Extensions.Async.setMain` to configure Hopac with the main
  /// synchronization context.
#if DOC
  ///
  /// The builder has been constructed by calling
  /// `Hopac.Extensions.Async.Global.onMain ()`.
#endif
  val onMain: Extensions.Async.OnWithSchedulerBuilder

  /// Starts running the given job on the global scheduler and then blocks the
  /// current thread waiting for the job to either return successfully or fail.
  /// See also: `start`.
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
  ///
  /// This is the same function as `Job.Global.run`.
#endif
  val inline run: Job<'x> -> 'x

  /// Starts running the given job on the global scheduler, but does not wait
  /// for the job to finish.  See also: `queue`, `server`.
#if DOC
  ///
  /// Note that using this function in a job workflow is not optimal and you
  /// should use `Job.start` instead.
  ///
  /// This is the same function as `Job.Global.start`.
#endif
  val inline start: Job<unit> -> unit

  /// Starts running the given job on the global scheduler, but does not wait
  /// for the job to finish.  `startIgnore xJ` is equivalent to `Job.Ignore xJ
  /// |> start`.
  val inline startIgnore: Job<_> -> unit

  /// Starts running the given delayed job on the global scheduler, but does not
  /// wait for the job to finish.  `startDelay u2xJ` is equivalent to
  /// `startIgnore <| Job.delay u2xJ`.
  val inline startDelay: (unit -> #Job<_>) -> unit

  /// Queues the given job for execution on the global scheduler.  See also:
  /// `start`, `server`.
#if DOC
  ///
  /// Note that using this function in a job workflow is not optimal and you
  /// should use `Job.queue` instead.
  ///
  /// This is the same function as `Job.Global.queue`.
#endif
  val inline queue: Job<unit> -> unit

  /// Queues the given job for execution on the global scheduler.  `queueIgnore
  /// xJ` is equivalent to `Job.Ignore xJ |> queue`.
  val inline queueIgnore: Job<_> -> unit

  /// Queues the given delayed job for execution on the global scheduler.
  /// `queueDelay u2xJ` is equivalent to `queueIgnore <| Job.delay u2xJ`.
  val inline queueDelay: (unit -> #Job<_>) -> unit

  /// Starts running the given job on the global scheduler like `start`, but the
  /// given job is known never to return normally, so the job can be spawned in
  /// an even more lightweight manner.
#if DOC
  ///
  /// Note that using this function in a job workflow is not optimal and you
  /// should use `Job.server` instead.
  ///
  /// This is the same function as `Job.Global.server`.
#endif
  val inline server: Job<Void> -> unit

  /// Use object as alternative.  This function is a NOP and is provided as a
  /// kind of syntactic alternative to using a type ascription or an `upcast`.
  val inline asAlt: Alt<'x> -> Alt<'x>

  /// Use object as job.  This function is a NOP and is provided as a kind of
  /// syntactic alternative to using a type ascription or an `upcast`.
  val inline asJob: Job<'x> -> Job<'x>

  /// Creates a new channel.  This is the same function as `Ch.Now.create`.
  val inline ch: unit -> Ch<'x>

  /// Creates a new mailbox.  This is the same function as
  /// `Mailbox.Now.create`.
  val inline mb: unit -> Mailbox<'x>

  /// Creates a new write once variable.  This is the same function as
  /// `IVar.Now.create`.
  val inline ivar: unit -> IVar<'x>

  /// Creates a new write once variable with the given value.  This is the same
  /// function as `IVar.Now.createFull`.
  val inline ivarFull: 'x -> IVar<'x>

  /// Creates a serialized variable that is initially empty.  This is the same
  /// function as `MVar.Now.create`.
  val inline mvar: unit -> MVar<'x>

  /// Creates a new serialized variable that initially contains the given value.
  /// This is the same function as `MVar.Now.createFull`.
  val inline mvarFull: 'x -> MVar<'x>

  /// Creates a timeout for the specified time span.  This is the same function
  /// as `Timer.Global.timeOut`.
  val inline timeOut: TimeSpan -> Alt<unit>

  /// Creates a timeout for the specified number of milliseconds.  This is the
  /// same function as `Timer.Global.timeOutMillis`.
  val inline timeOutMillis: int -> Alt<unit>

  /// Creates a promise whose value is computed lazily with the given job when
  /// an attempt is made to read the promise.  This is the same function as
  /// `Promise.Now.delay`.
  val inline memo: Job<'x> -> Promise<'x>
