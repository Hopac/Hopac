// Copyright (C) by Housemarque, Inc.

/// Hopac is a library for F# with the aim of making it easier to write correct,
/// modular and efficient parallel, asynchronous, concurrent and reactive
/// programs.  The design of Hopac draws inspiration from languages such as
/// Concurrent ML and Cilk.  Similar to Concurrent ML, Hopac provides message
/// passing primitives and supports the construction of first-class synchronous
/// abstractions, see `Alt<_>`.  Parallel jobs (lightweight threads) in Hopac
/// are created using techniques similar to the F# Async framework, see
/// `Job<_>`.  Hopac runs parallel jobs using a work distributing scheduler in a
/// non-preemptive fashion.  Hopac also includes an implementation of choice
/// streams, see `Stream.Stream<_>`, that offers a simple approach to reactive
/// programming.
///
/// Before you begin using Hopac, make sure that you have configured your F#
/// interactive and your application to use server garbage collection.  By
/// default, .Net uses single-threaded workstation garbage collection, which
/// makes it impossible for parallel programs to scale.
///
/// The documentation of many of the primitives contains a reference
/// implementation.  In most cases, actual implementations are optimized by
/// taking advantage of internal implementation details and may be significantly
/// faster than the reference implementation.  The reference implementations are
/// given for a number of reasons.  First of all, they hopefully help to better
/// understand the semantics of the primitives.  In some cases, the reference
/// implementations also demonstrate how you can interface Hopac with other
/// systems without the need to extend the primitives of Hopac.  The reference
/// implementations can also be seen as examples of how various primitives can
/// be used to implement more complex operations.
///
/// Here is a diagram of some of Hopac's types:
///
///>                            Job<_>
///>                             |
///>                            Alt<_>
///>                             |
///>       +-------+--------+----+-----+--------+--------+
///>       |       |        |          |        |        |
///>      Ch<_>  Latch   Mailbox<_>  MVar<_>  Proc   Promise<_>
///>                                                     |
///>                                                   IVar<_>
///
/// Parts of the documentation will refer to certain functions that are not
/// provided as part of the library, but are defined as follows:
///
///> let always x _ = x
namespace Hopac

open System.Threading
open System.Threading.Tasks
open System

////////////////////////////////////////////////////////////////////////////////

/// A type that has no public constructors to indicate that a job or function
/// does not return normally.
type Void

////////////////////////////////////////////////////////////////////////////////

/// An experimental interface for asynchronously disposable resources.  See
/// also: `usingAsync`.
#if DOC
///
/// The point of this interface is that resources using jobs may not be easily
/// synchronously disposable.  Expressing the dispose operation as a job allows
/// the operation to wait for parallel, asynchronous and concurrent operations
/// to complete.
///
/// Note that simply calling `run (x.DisposeAsync ())` defeats the purpose of
/// this interface, unless it is known that the call is not made from a Hopac
/// worker thread and no communication is needed between that thread and the
/// dispose job.
#endif
type IAsyncDisposable =
  /// Returns a job that needs to be executed to dispose the resource.  The
  /// returned job should wait until the resource is properly disposed.
#if DOC
  ///
  /// Note that simply calling `DisposeAsync` must not immediately dispose the
  /// resource.  For example, the following pattern is incorrect:
  ///
  ///> override this.DisposeAsync () = this.DisposeFlag <- true ; Job.unit ()
  ///
  /// A typical correct disposal pattern could look something like this:
  ///
  ///> override this.DisposeAsync () =
  ///>   IVar.tryFill requestDisposeIVar () >>=.
  ///>   completedDisposeIVar
  ///
  /// The above first signals the server to dispose by filling a synchronous
  /// variable.  This is non-blocking and does not leak resources.  Then the
  /// above waits until the server has signaled that disposal is complete.  If
  /// disposal has already been requested, the first operation does nothing.
  /// Note that two separate variables are used.
  ///
  /// The server loop corresponding to the above could look like this:
  ///
  ///> let rec serverLoop ... =
  ///>   ...
  ///>   let disposeAlt () =
  ///>     requestDisposeIVar ^=> fun () ->
  ///>     ...
  ///>     completedDisposeIVar *<= ()
  ///>   ...
  ///>   ... <|> disposeAlt () <|> ...
  ///
  /// In some cases it may be preferable to have the server loop take requests
  /// mainly from a single channel (or mailbox):
  ///
  ///> let rec serverLoop ... =
  ///>   requestCh >>= function
  ///>     ...
  ///>     | RequestDispose ->
  ///>       ...
  ///>       completedDisposeIVar *<= ()
  ///>     ...
  ///
  /// In such a case, one can still use the above two variable disposal pattern
  /// by spawning a job that forwards the disposal request to the server request
  /// channel before the server loop is started:
  ///
  ///> requestDisposeIVar >>=. requestCh *<- RequestDispose |> start
  ///
  /// Alternatively, it is usually acceptable to simply send an asynchronous
  /// dispose request to the server:
  ///
  ///> override this.DisposeAsync () =
  ///>   requestCh *<+ RequestDispose >>=.
  ///>   completedDisposeIVar
#endif
  abstract DisposeAsync: unit -> Job<unit>

////////////////////////////////////////////////////////////////////////////////

#if DOC
/// Represents a lightweight thread of execution.
///
/// Jobs are defined using expression builders like the `JobBuilder`, accessible
/// via the `Hopac.job` binding, or using monadic combinators and can then be
/// executed using e.g. `Hopac.run`.
///
/// For example, here is a function that creates a job that computes Fibonacci
/// numbers:
///
///> let rec fib n = job {
///>   if n < 2L then
///>     return n
///>   else
///>     let! (x, y) = fib (n-2L) <*> fib (n-1L)
///>     return x+y
///> }
///
/// It can be run, for example, by using the global scheduler:
///
///> > fib 30L |> run ;;
///> val it : int = 832040L
///
/// If you ran the above above examples, you just did the equivalent of running
/// roughly your first million parallel jobs using Hopac.
type Job<'x>
#endif

/// Operations on jobs.
module Job =
  /// Operations on the global scheduler.
#if DOC
  ///
  /// Note that in a typical program there should only be a few points (maybe
  /// just one) where jobs are started or run outside of job workflows.
#endif
  [<Obsolete "`Job.Global` module will be removed. Use the `Hopac` module.">]
  module internal Global =
    /// Queues the job for execution on the global scheduler.  See also:
    /// `start`, `server`.
#if DOC
    ///
    /// Note that using this function in a job workflow is not optimal and you
    /// should use `Job.queue` instead.
#endif
    [<Obsolete "`Job.Global` module will be removed. Use the `Hopac` module.">]
    val inline queue:       Job<unit> -> unit

    /// Queues the job for execution on the global scheduler.  `queueIgnore xJ`
    /// is equivalent to `Job.Ignore xJ |> queue`.
    [<Obsolete "`Job.Global` module will be removed. Use the `Hopac` module.">]
    val queueIgnore: Job<_>    -> unit

    /// Like `Job.Global.start`, but the given job is known never to return
    /// normally, so the job can be spawned in an even more lightweight manner.
#if DOC
    ///
    /// Note that using this function in a job workflow is not optimal and you
    /// should use `Job.server` instead.
#endif
    [<Obsolete "`Job.Global` module will be removed. Use the `Hopac` module.">]
    val server:      Job<Void> -> unit

    /// Starts running the given job on the global scheduler, but does not wait
    /// for the job to finish.  See also: `queue`, `server`.
#if DOC
    ///
    /// Note that using this function in a job workflow is not optimal and you
    /// should use `Job.start` instead.
#endif
    [<Obsolete "`Job.Global` module will be removed. Use the `Hopac` module.">]
    val inline start:       Job<unit> -> unit

    /// Starts running the given job on the global scheduler, but does not wait
    /// for the job to finish.  `startIgnore xJ` is equivalent to `Job.Ignore xJ
    /// |> start`.
    [<Obsolete "`Job.Global` module will be removed. Use the `Hopac` module.">]
    val startIgnore: Job<_>    -> unit

    [<Obsolete "`Job.Global` module will be removed. Use the `Hopac` module.">]
    val queueAsTask: Job<'x> -> Task<'x>
    [<Obsolete "`Job.Global` module will be removed. Use the `Hopac` module.">]
    val startAsTask: Job<'x> -> Task<'x>

    /// Starts running the given job on the global scheduler, but does not wait
    /// for the job to finish.  Upon the failure or success of the job, one of
    /// the given actions is called once.
#if DOC
    ///
    /// Note that using this function in a job workflow is not optimal and you
    /// should instead use `Job.start` with the desired exception handling
    /// construct (e.g. `Job.tryIn` or `Job.catch`).
#endif
    [<Obsolete "`Job.Global` module will be removed. Use the `Hopac` module.">]
    val startWithActions: (exn -> unit) -> ('x -> unit) -> Job<'x> -> unit

    /// Starts running the given job on the global scheduler and then blocks the
    /// current thread waiting for the job to either return successfully or
    /// fail.
#if DOC
    ///
    /// WARNING: Use of `run` should be considered carefully, because calling
    /// `run` from an arbitrary thread can cause deadlock.
    ///
    /// A call of `run xJ` is safe when the call is not made from within a Hopac
    /// worker thread and the job `xJ` does not perform operations that might
    /// block or that might directly, or indirectly, need to communicate with
    /// the thread from which `run` is being called.
    ///
    /// Note that using this function from within a job workflow should never be
    /// needed, because within a workflow the result of a job can be obtained by
    /// binding.
#endif
    [<Obsolete "`Job.Global` module will be removed. Use the `Hopac` module.">]
    val run: Job<'x> -> 'x

  //# Spawning jobs

  /// Creates a job that schedules the given job to be run as a separate
  /// concurrent job.  Use `Promise.queue` if you need to be able to get the
  /// result.  See also: `Proc.queue`.
#if DOC
  ///
  /// The difference between `start` and `queue` is which job, the current job,
  /// or the new job, is immediately given control.  `start` queues the current
  /// job, while `queue` queues the new job.  `start` has slightly lower
  /// overhead than `queue` and is likely to be faster in cases where the new
  /// job blocks immediately.
  ///
  /// For best performance the choice of which operation to use, `start` or
  /// `queue`, depends on the critical path of your system.  It is generally
  /// preferable to keep control in the job that is on the critical path.
#endif
  val inline queue:       Job<unit> -> Job<unit>

  /// Creates a job that schedules the given job to be run as a separate
  /// concurrent job.  `queueIgnore xJ` is equivalent to `Job.Ignore xJ |>
  /// queue`.
  val queueIgnore: Job<_>    -> Job<unit>

  /// Creates a job that immediately starts running the given job as a separate
  /// concurrent job like `start`, but the given job is known never to return
  /// normally, so the job can be spawned in an even more lightweight manner.
  val server:      Job<Void> -> Job<unit>

  /// Creates a job that immediately starts running the given job as a separate
  /// concurrent job.  Use `Promise.start` if you need to be able to get the
  /// result.  Use `Job.server` if the job never returns normally.  See also:
  /// `Job.queue`, `Proc.start`.
  val inline start:       Job<unit> -> Job<unit>

  /// Creates a job that immediately starts running the given job as a separate
  /// concurrent job.  `startIgnore xJ` is equivalent to `Job.Ignore xJ |>
  /// start`.
  val startIgnore: Job<_>    -> Job<unit>

  //# Basic combinators

  /// Creates a job with the given result.  See also: `lift`, `thunk`, `unit`.
  val result: 'x   -> Job<'x>

  /// Returns a job that does nothing and returns `()`.  `unit ()` is an
  /// optimized version of `result ()`.
  val   inline unit: unit -> Job<unit>

  /// Creates a job that first runs the given job and then passes the result of
  /// that job to the given function to build another job which will then be
  /// run.  This is the same as `>>=` with the arguments flipped.
  val inline bind:      ('x   -> #Job<'y>) -> Job<'x> -> Job<'y>

  /// Creates a job that calls the given function with the given value to build
  /// a job that will then be run.  `delayWith x2yJ x` is equivalent to `result
  /// x >>= x2yJ`.
  val inline delayWith: ('x   -> #Job<'y>) ->     'x  -> Job<'y>

  /// Creates a job that runs the given job and maps the result of the job with
  /// the given function.  This is the same as `>>-` with the arguments flipped.
  val inline map:       ('x   ->      'y)  -> Job<'x> -> Job<'y>

  /// Creates a job that calls the given function with the given value to
  /// compute the result of the job.  `lift x2y x` is equivalent to `result x
  /// >>- x2y`.  Note that `x2y x |> result` is not the same.
  val inline lift:      ('x   ->      'y)  ->     'x  -> Job<'y>

  /// Creates a job that calls the given function to build a job that will then
  /// be run.  `delay u2xJ` is equivalent to `result () >>= u2xJ`.
#if DOC
  ///
  /// Use of `delay` is often essential for making sure that a job constructed
  /// with user-defined code properly captures side-effects performed in the
  /// user-defined code or that a job is not constructed too eagerly
  /// (e.g. traversing an entire data structure to build a very large job
  /// object).  However, it is also the case that there is no need to wrap every
  /// constructed job with `delay` and avoiding unnecessary `delay` operations
  /// can improve performance.
#endif
  val inline delay:     (unit -> #Job<'y>)            -> Job<'y>

  /// Creates a job that invokes the given thunk to compute the result of the
  /// job.  `thunk u2x` is equivalent to `result () >>- u2x`.
  val inline thunk:     (unit ->      'y)             -> Job<'y>

  /// `x2yJ |> apply xJ` is equivalent to `x2yJ >>= fun x2y -> xJ >>- x2y`.
  val apply: Job<'x> -> Job<'x -> 'y> -> Job<'y>

  /// `join xJJ` is equivalent to `bind id xJJ`.
  val inline join: Job<#Job<'x>> -> Job<'x>

  /// Creates a job that immediately terminates the current job.
#if DOC
  ///
  /// Note that when a job aborts, it is considered to be equivalent to having
  /// the job block indefinitely and the job will be garbage collected.  This
  /// also means that the job neither returns succesfully nor fails with an
  /// exception.  This can sometimes be just what you want.  However, in order
  /// to execute clean-up operations implemented with `using` or `tryFinallyFun`
  /// or `tryFinallyJob`, the job must either return normally or raise an
  /// exception.  In other words, do not use `abort` in such a case.
#endif
  val abort: unit -> Job<_>

  /// Creates a job like the given job except that the result of the job will be
  /// `()`.  `Ignore xJ` is equivalent to `xJ >>- ignore`.
  val Ignore: Job<_> -> Job<unit>

  //# Exception handling

  /// Creates a job that has the effect of raising the specified exception.
  /// `raises e` is equivalent to `Job.delayWith raise e`.
  val raises: exn -> Job<_>

  /// Implements the `try-in-unless` exception handling construct for jobs.
  /// Both of the continuation jobs `'x -> Job<'y>`, for success, and `exn ->
  /// Job<'y>`, for failure, are invoked from a tail position.  See also:
  /// `tryInDelay`.
#if DOC
  ///
  /// Note that the workflow notation of F# does not support this operation.  It
  /// only supports the `Job.tryWith` operation.  `Job.tryIn` makes it easier to
  /// write exception handling code that has the desired tail-call properties.
#endif
  val inline tryIn:                Job<'x>  -> ('x -> #Job<'y>) -> (exn -> #Job<'y>) -> Job<'y>

  /// Implements the `try-in-unless` exception handling construct for jobs.
  /// Both of the continuation jobs `'x -> Job<'y>`, for success, and `exn ->
  /// Job<'y>`, for failure, are invoked from a tail position.  `tryInDelay u2xJ
  /// x2yJ e2yJ` is equivalent to `tryIn (delay u2xJ) x2yJ e2yJ`.
  val inline tryInDelay: (unit -> #Job<'x>) -> ('x -> #Job<'y>) -> (exn -> #Job<'y>) -> Job<'y>

  /// Implements the try-with exception handling construct for jobs.
#if DOC
  ///
  /// Reference implementation:
  ///
  ///> let tryWith xJ e2xJ = tryIn xJ result e2xJ
#endif
  val inline tryWith:                Job<'x>  -> (exn -> #Job<'x>) -> Job<'x>

  /// Implements the try-with exception handling construct for jobs.
  val inline tryWithDelay: (unit -> #Job<'x>) -> (exn -> #Job<'x>) -> Job<'x>

  /// Implements a variation of the `try-finally` exception handling construct
  /// for jobs.  The given action, specified as a job, is executed after the job
  /// has been run, whether it fails or completes successfully.
#if DOC
  ///
  /// Note that the workflow notation of F# does not support this operation.  It
  /// only supports the weaker `tryFinallyFun` operation.
  ///
  /// Reference implementation:
  ///
  ///> let tryFinallyJob xJ uJ =
  ///>   tryIn xJ
  ///>    <| fun x -> uJ >>-. x
  ///>    <| fun e -> uJ >>-! e
#endif
  val tryFinallyJob:                Job<'x>  ->      Job<unit> -> Job<'x>

  /// Implements a variation of the `try-finally` exception handling construct
  /// for jobs.  The given action, specified as a job, is executed after the job
  /// has been run, whether it fails or completes successfully.
  val tryFinallyJobDelay: (unit -> #Job<'x>) ->      Job<unit> -> Job<'x>

  /// Implements a variation of the `try-finally` exception handling construct
  /// for jobs.  The given action, specified as a function, is executed after
  /// the job has been run, whether it fails or completes successfully.
#if DOC
  ///
  /// Reference implementation:
  ///
  ///> let tryFinallyFun xJ u2u =
  ///>   tryFinallyJob xJ
  ///>    <| thunk u2u
#endif
  val tryFinallyFun:                Job<'x>  -> (unit -> unit) -> Job<'x>

  /// Implements a variation of the `try-finally` exception handling construct
  /// for jobs.  The given action, specified as a function, is executed after
  /// the job has been run, whether it fails or completes successfully.
  val tryFinallyFunDelay: (unit -> #Job<'x>) -> (unit -> unit) -> Job<'x>

  /// Creates a job that runs the given job and results in either the ordinary
  /// result of the job or the exception raised by the job.
#if DOC
  ///
  /// Reference implementation:
  ///
  ///> let catch xJ =
  ///>   tryIn xJ
  ///>    <| lift Choice1Of2
  ///>    <| lift Choice2Of2
#endif
  val catch: Job<'x> -> Job<Choice<'x, exn>>

  //# Finalization

  /// `useIn x2yJ x` is equivalent to `using x x2yJ` and can be more convenient
  /// to use in pipelines (i.e. `x |> useIn x2yJ`).
  val inline useIn:      ('x -> #Job<'y>) -> 'x -> Job<'y> when 'x :> IDisposable

  /// Implements the `use` construct for jobs.  The `Dispose` method of the
  /// given disposable object is called after running the job constructed with
  /// the disposable object.  See also: `abort`, `usingAsync`.
#if DOC
  ///
  /// Reference implementation:
  ///
  ///> let using (x: 'x when 'x :> IDisposable) x2yJ =
  ///>   tryFinallyFun (delayWith x2yJ x) (x :> IDisposable).Dispose
  ///
  /// Note that the `Dispose` method is not called if the job aborts before
  /// returning from the scope of the `using` job.  This is not a serious
  /// problem, because scoped disposal of managed resources is usually an
  /// optimization and unmanaged resources should already be cleaned up by
  /// finalizers.  In cases where you need to ensure scoped disposal, make sure
  /// that the job does not abort before returning.
#endif
  val inline using:      'x -> ('x -> #Job<'y>) -> Job<'y> when 'x :> IDisposable

  /// Implements an experimental `use` like construct for asynchronously
  /// disposable resources.  The `DisposeAsync` method of the asynchronously
  /// disposable resource is called to construct a job that is later used to
  /// dispose the resource after the constructed job returns.  See also:
  /// `abort`, `using`.
#if DOC
  ///
  /// Reference implementation:
  ///
  ///> let usingAsync (x: 'x when 'x :> IAsyncDisposable) x2yJ =
  ///>   tryFinallyJob <| delayWith x2yJ x
  ///>    <| x.DisposeAsync ()
#endif
  val usingAsync: 'x -> ('x -> #Job<'y>) -> Job<'y> when 'x :> IAsyncDisposable

  //# Repeating an operation

  /// Creates a job that runs the given job sequentially the given number of
  /// times.
#if DOC
  ///
  /// Reference implementation:
  ///
  ///> let rec forN n uJ =
  ///>   if n > 0 then
  ///>     uJ >>= fun () -> forN (n - 1) uJ
  ///>   else
  ///>     Job.unit ()
#endif
  val inline forN:       int -> Job<unit> -> Job<unit>

  /// `forNIgnore n xJ` is equivalent to `Job.Ignore xJ |> forN n`.
  val forNIgnore: int -> Job<_>    -> Job<unit>

  //# Iterating over a range

  /// `forUpTo lo hi i2uJ` creates a job that sequentially iterates from `lo` to
  /// `hi` (inclusive) and calls the given function to construct jobs that will
  /// be executed.
#if DOC
  ///
  /// Reference implementation:
  ///
  ///> let rec forUpTo lo hi i2uJ =
  ///>   if lo <= hi then
  ///>     i2uJ lo >>= fun () -> forUpTo (lo + 1) hi i2uJ
  ///>   else
  ///>     Job.unit ()
  ///
  /// Rationale: The reason for iterating over an inclusive range is to make
  /// this construct work like a `for ... to ... do ...` loop of the base F#
  /// language.
#endif
  val inline forUpTo:         int -> int -> (int -> #Job<unit>) -> Job<unit>

  /// `forUpToIgnore lo hi i2xJ` is equivalent to `forUpTo lo hi (i2xJ >>
  /// Job.Ignore)`.
  val inline forUpToIgnore:   int -> int -> (int -> #Job<_>)    -> Job<unit>

  /// `forDownTo hi lo i2uJ` creates a job that sequentially iterates from `hi`
  /// to `lo` (inclusive) and calls the given function to construct jobs that
  /// will be executed.
#if DOC
  ///
  /// Reference implementation:
  ///
  ///> let rec forDownTo hi lo i2uJ =
  ///>   if hi >= lo then
  ///>     i2uJ hi >>= fun () -> forDownTo (hi - 1) lo i2uJ
  ///>   else
  ///>     Job.unit ()
  ///
  /// Rationale: The reason for iterating over an inclusive range is to make
  /// this construct work like a `for ... downto ... do ...` loop of the base F#
  /// language.
#endif
  val inline forDownTo:       int -> int -> (int -> #Job<unit>) -> Job<unit>

  /// `forDownToIgnore hi lo i2xJ` is equivalent to `forDownTo hi lo (i2xJ >>
  /// Job.Ignore)`.
  val inline forDownToIgnore: int -> int -> (int -> #Job<_>)    -> Job<unit>

  //# Iterating conditionally

  /// `whileDo u2b uJ` creates a job that sequentially executes the `uJ` job as
  /// long as `u2b ()` returns `true`.  See also: `whileDoDelay`.
#if DOC
  ///
  /// Reference implementation:
  ///
  ///> let whileDo u2b uJ = Job.delay <| fun () ->
  ///>   let rec loop () =
  ///>     if u2b () then
  ///>       uJ >>= loop
  ///>     else
  ///>       Job.unit ()
  ///>   loop ()
#endif
  val inline whileDo:       (unit -> bool) ->           Job<unit> -> Job<unit>

  /// `whileDoDelay u2b u2xJ` creates a job that sequentially constructs a job
  /// with `u2xJ` and executes it as long as `u2b ()` returns `true`.
  val inline whileDoDelay:  (unit -> bool) -> (unit -> #Job<_>)   -> Job<unit>

  /// `whileDoIgnore u2b xJ` creates a job that sequentially executes the `xJ`
  /// job as long as `u2b ()` returns `true`.  `whileDoIgnore u2b xJ` is
  /// equivalent to `Job.Ignore xJ |> whileDo u2b`.
  val inline whileDoIgnore: (unit -> bool) ->           Job<_>    -> Job<unit>

  //# Conditional

  /// `whenDo b uJ` is equivalent to `if b then uJ else Job.unit ()`.
  val inline whenDo: bool -> Job<unit> -> Job<unit>

  //# Server loops

  /// Creates a job that repeats the given job indefinitely.  See also:
  /// `foreverServer`, `iterate`.
#if DOC
  ///
  /// It is a common programming pattern to use server jobs that loop
  /// indefinitely and communicate with clients via channels.  When a job is
  /// blocked waiting for communication on one or more channels and the channels
  /// become garbage (no longer reachable by any other job) the job can be
  /// garbage collected as well.
  ///
  /// Reference implementation:
  ///
  ///> let rec forever uJ = uJ >>= fun () -> forever uJ
#endif
  val inline forever:       Job<unit> -> Job<_>

  /// `foreverIgnore xJ` is equivalent to `Job.Ignore xJ |> forever`.
  val foreverIgnore: Job<_>    -> Job<_>

  /// Creates a job that indefinitely iterates the given job constructor
  /// starting with the given value.  See also: `iterateServer`, `forever`.
#if DOC
  ///
  /// It is a common programming pattern to use server jobs that loop
  /// indefinitely and communicate with clients via channels.  When a job is
  /// blocked waiting for communication on one or more channels and the channels
  /// become garbage (no longer reachable by any other job) the job can be
  /// garbage collected as well.
  ///
  /// Reference implementation:
  ///
  ///> let rec iterate x x2xJ =
  ///>   x2xJ x >>= fun x -> iterate x x2xJ
#endif
  val inline iterate: 'x -> ('x -> #Job<'x>) -> Job<_>

  //# Spawning server loops

  /// Creates a job that starts a separate server job that repeats the given job
  /// indefinitely.  `foreverServer xJ` is equivalent to `forever xJ |> server`.
  val foreverServer: Job<unit> -> Job<unit>

  /// Creates a job that starts a separate server job that indefinitely iterates
  /// the given job constructor starting with the given value.  `iterateServer x
  /// x2xJ` is equivalent to `iterate x x2xJ |> server`.
  val inline iterateServer: 'x -> ('x -> #Job<'x>) -> Job<unit>

  //# Sequences of jobs

  /// Creates a job that runs all of the jobs in sequence and returns a list of
  /// the results.  See also: `seqIgnore`, `conCollect`, `Seq.mapJob`.
#if DOC
  ///
  /// Reference implementation:
  ///
  ///> let seqCollect (xJs: seq<Job<'x>>) = Job.delay <| fun () ->
  ///>   let xs = ResizeArray<_> ()
  ///>   Job.using <| xJs.GetEnumerator () <| fun xJs ->
  ///>   Job.whileDoDelay xJs.MoveNext <| fun () ->
  ///>         xJs.Current >>- xs.Add
  ///>   >>-. xs

#endif
  val seqCollect: seq<#Job<'x>> -> Job<ResizeArray<'x>>

  /// Creates a job that runs all of the jobs as separate concurrent jobs and
  /// returns a list of the results.  See also: `conIgnore`, `seqCollect`,
  /// `Seq.Con.mapJob`.
  ///
  /// Note that when multiple jobs raise exceptions, then the created job raises
  /// an `AggregateException`.
  ///
  /// Note that this is not optimal for fine-grained parallel execution.
  val conCollect: seq<#Job<'x>> -> Job<ResizeArray<'x>>

  /// Creates a job that runs all of the jobs in sequence.  The results of the
  /// jobs are ignored.  See also: `seqCollect`, `conIgnore`, `Seq.iterJob`.
#if DOC
  ///
  /// Reference implementation:
  ///
  ///> let seqIgnore (uJs: seq<#Job<unit>>) = Job.delay <| fun () ->
  ///>   Job.using <| uJs.GetEnumerator () <| fun uJs ->
  ///>   Job.whileDoDelay uJs.MoveNext <| fun () ->
  ///>     uJs.Current
#endif
  val seqIgnore: seq<#Job<_>> -> Job<unit>

  /// Creates a job that runs all of the jobs as separate concurrent jobs and
  /// then waits for all of the jobs to finish.  The results of the jobs are
  /// ignored.  See also: `conCollect`, `seqIgnore`, `Seq.Con.iterJob`.
  ///
  /// Note that when multiple jobs raise exceptions, then the created job raises
  /// an `AggregateException`.
  ///
  /// Note that this is not optimal for fine-grained parallel execution.
  val conIgnore: seq<#Job<_>> -> Job<unit>

  //# Interop

  /// Creates a job that performs the asynchronous operation defined by the
  /// given pair of `doBegin` and `doEnd` operations.  See also:
  /// `Alt.fromBeginEnd`.
#if DOC
  ///
  /// Reference implementation:
  ///
  ///> let fromBeginEnd doBegin doEnd =
  ///>   Job.Scheduler.bind <| fun sr ->
  ///>   let xI = IVar ()
  ///>   doBegin <| AsyncCallback (fun ar ->
  ///>     Scheduler.start sr (try xI *<= doEnd ar with e -> xI *<=! e))
  ///>   |> ignore
  ///>   xI
#endif
  val inline fromBeginEnd: doBegin: (AsyncCallback * obj -> IAsyncResult)
                 -> doEnd: (IAsyncResult -> 'x)
                 -> Job<'x>

  /// `fromEndBegin doEnd doBegin` is equivalent to `fromBeginEnd doBegin
  /// doEnd`.
  val inline fromEndBegin: doEnd: (IAsyncResult -> 'x)
                 -> doBegin: (AsyncCallback * obj -> IAsyncResult)
                 -> Job<'x>

  /// Creates a job that starts an asynchronous operation by calling the given
  /// function with success and failure continuations of which exactly one must
  /// be called once.
  val inline fromContinuations: (('x -> unit) -> (exn -> unit) -> unit) -> Job<'x>

  /// Creates a job that queues the given thunk to execute on the system
  /// `ThreadPool` and then waits for the result of the thunk.
#if DOC
  ///
  /// One use case for this is to run legacy, non-cooperative, non-async
  /// (blocking) IO bound operations.
#endif
  val inline onThreadPool: (unit -> 'x) -> Job<'x>

  /// Creates a job that starts the given async operation and waits for it to
  /// complete.  See also: `Alt.fromAsync`.
#if DOC
  ///
  /// Note that the async operation is started on whichever thread (and
  /// synchronization context) the job happens to be executed on.  Transfer the
  /// async operation explicitly, e.g. by using `Async.SwitchToContext`, to the
  /// desired context when necessary.
#endif
  val fromAsync: Async<'x> -> Job<'x>

  /// Creates an async operation that starts the given job and waits for it to
  /// complete.
  val   toAsync: Job<'x> -> Async<'x>

  /// `bindAsync x2yJ xA` is equivalent to `fromAsync xA >>= x2yJ`.
  val inline bindAsync:     ('x -> #Job<'y>) -> Async<'x> -> Job<'y>

  /// Creates a job that calls the given function to start a task and waits for
  /// it to complete.  See also: `Alt.fromTask`.
  val inline fromTask:      (unit -> Task<'x>) -> Job<'x>

  /// Creates a job that calls the given function to start a value task and waits for
  /// it to complete.  See also: `Alt.fromValueTask`.
  val inline fromValueTask: (unit -> ValueTask<'x>) -> Job<'x>

  /// Creates a job that calls the given function to start a task and waits for
  /// it to complete.  See also: `Alt.fromUnitTask`.
  val inline fromUnitTask:  (unit -> Task)     -> Job<unit>

  /// Creates a job that calls the given function to start a value task and waits for
  /// it to complete. TODO: Alt.fromUnitValueTask
  val inline fromUnitValueTask: (unit -> ValueTask)     -> Job<unit>

  /// `liftTask x2yT` is equivalent to `fun x -> fromTask <| fun () -> x2yT x`.
  val inline liftTask:      ('x -> Task<'y>) -> 'x -> Job<'y>

  // TODO: liftValueTask

  /// `liftUnitTask x2uT` is equivalent to `fun x -> fromUnitTask <| fun () ->
  /// x2uT x`.
  val inline liftUnitTask: ('x -> Task)     -> 'x -> Job<unit>

  // TODO: liftUnitValueTask

  /// Creates a job that waits for the given task to finish and then returns the
  /// result of the task.  Note that this does not start the task.  Make sure
  /// that the task is started correctly.  Exceptions thrown during task
  /// initialization may not be caught. Prefer `fromTask` or `liftTask`.
#if DOC
  ///
  /// Functions that return a task run synchronously until they block.  If an
  /// exception is raised during this synchronous phase, the exception cannot
  /// be caught by a later `Job.catch`.
  ///
  /// This form will not catch the expected value:
  ///
  ///> readFromBufferTask buffer |> Job.awaitTask |> Job.catch
  ///
  /// Use `fromTask` or `liftTask` instead:
  ///
  ///> Job.fromTask (fun () -> readFromBufferTask buffer) |> Job.catch
  ///> Job.liftTask readFromBufferTask buffer |> Job.catch
  ///
  /// This will catch an exception thrown by the wrapped task for handling in
  /// the `Job` context.
  ///
  /// Reference implementation:
  ///
  ///> let awaitTask (xT: Task<'x>) =
  ///>   Job.Scheduler.bind <| fun sr ->
  ///>   let xI = IVar ()
  ///>   xT.ContinueWith (Action<Threading.Tasks.Task>(fun _ ->
  ///>     Scheduler.start sr (try xI *<= xT.Result with e -> xI *<=! e)))
  ///>   |> ignore
  ///>   xI
#endif
  val inline awaitTask:     Task<'x> -> Job<'x>

  /// Creates a job that waits for the given task to finish and then returns the
  /// result of the task.  Note that this does not start the task.  Make sure
  /// that the task is started correctly.  Exceptions thrown during task
  /// initialization may not be caught. Prefer `fromTask` or `liftTask`.
#if DOC
  ///
  /// Functions that return a task run synchronously until they block.  If an
  /// exception is raised during this synchronous phase, the exception cannot
  /// be caught by a later `Job.catch`.
  ///
  /// This form will not catch the expected value:
  ///
  ///> readFromBufferTask buffer |> Job.awaitValueTask |> Job.catch
  ///
  /// Use `fromTask` or `liftTask` instead:
  ///
  ///> Job.fromTask (fun () -> readFromBufferTask buffer) |> Job.catch
  ///> Job.liftTask readFromBufferTask buffer |> Job.catch
  ///
  /// This will catch an exception thrown by the wrapped task for handling in
  /// the `Job` context.
  ///
  /// Reference implementation:
  ///
  ///> let awaitTask (xT: Task<'x>) =
  ///>   Job.Scheduler.bind <| fun sr ->
  ///>   let xI = IVar ()
  ///>   xT.ContinueWith (Action<Threading.Tasks.Task>(fun _ ->
  ///>     Scheduler.start sr (try xI *<= xT.Result with e -> xI *<=! e)))
  ///>   |> ignore
  ///>   xI
#endif
  val inline awaitValueTask: ValueTask<'x> -> Job<'x>

  /// Creates a job that waits until the given task finishes.  Note that this
  /// does not start the task.  Make sure that the task is started correctly.
  /// Exceptions thrown during task initialization may not be caught. Prefer
  /// `fromUnitTask` or `liftUnitTask`.
#if DOC
  ///
  /// Functions that return a task run synchronously until they block.  If an
  /// exception is raised during this synchronous phase, the exception cannot
  /// be caught by a later `Job.catch`.
  ///
  /// This form will not catch the expected value:
  ///
  ///> writeToBufferTask buffer value |> Job.awaitUnitTask |> Job.catch
  ///
  /// Use `fromUnitTask` or `liftUnitTask` instead:
  ///
  ///> Job.fromUnitTask (fun () -> writeToBufferTask buffer value) |> Job.catch
  ///> Job.liftUnitTask (writeToBufferTask buffer) value |> Job.catch
  ///
  /// This will catch an exception thrown by the wrapped task for handling in
  /// the `Job` context.
#endif
  val inline awaitUnitTask: Task     -> Job<unit>

  /// `bindTask x2yJ xT` is equivalent to `awaitTask xT >>= x2yJ`.
  /// Exceptions thrown during task initialization may not be caught. Prefer
  /// `fromTask` or `liftTask` to convert the task to a `Job` and use
  /// `Job.bind`.
  val inline bindTask:          ('x   -> #Job<'y>) -> Task<'x>       -> Job<'y>

  /// `bindValueTask x2yJ xVT` is equivalent to `awaitValueTask xVT >>= x2yJ`.
  /// Exceptions thrown during task initialization may not be caught. Prefer
  /// `fromValueTask` or `liftValueTask` to convert the task to a `Job` and use
  /// `Job.bind`.
  val inline bindValueTask:     ('x   -> #Job<'y>) -> ValueTask<'x>  -> Job<'y>

  /// `bindUnitTask u2xJ uT` is equivalent to `awaitUnitTask uT >>= u2xJ`.
  /// Exceptions thrown during task initialization may not be caught. Prefer
  /// `fromUnitTask` or `liftUnitTask` to convert the task to a `Job` and
  /// use `Job.bind`.
  val inline bindUnitTask:      (unit -> #Job<'y>) -> Task           -> Job<'y>
  
  /// `bindUnitValueTask u2xJ uVT` is equivalent to
  /// `awaitUnitValueeTask uVT >>= u2xJ`.
  /// Exceptions thrown during task initialization may not be caught. Prefer
  /// `fromUnitTask` or `liftUnitTask` to convert the task to a `Job` and
  /// use `Job.bind`.
  val inline bindUnitValueTask: (unit -> #Job<'y>) -> ValueTask      -> Job<'y>

  //# Debugging

  /// Given a job, creates a new job that behaves exactly like the given job,
  /// except that the new job obviously cannot be directly downcast to the
  /// underlying type of the given job.  This operation is provided for
  /// debugging purposes.  You can always break abstractions using reflection.
  /// See also: `Alt.paranoid`.
  val paranoid: Job<'x> -> Job<'x>

  /// Operations for dealing with the scheduler.
  module Scheduler =
    /// `bind s2xJ` creates a job that calls the given job constructor with the
    /// scheduler under which the job is being executed.  `bind` allows
    /// interfacing Hopac with existing asynchronous operations that do not fall
    /// into a pattern that is already supported explicitly.
#if DOC
    ///
    /// Hopac jobs are executed under a scheduler.  In almost all cases the
    /// scheduler is the global scheduler, but Hopac also allows local
    /// schedulers to be created for special purposes.  A job that is suspended
    /// for the duration of an external asynchronous operation should be
    /// explicitly resumed on the same scheduler.
    ///
    /// Suppose, for example, that some system provides an asynchronous
    /// operation with the following signature:
    ///
    ///> val opWithCallback: Input
    ///>                  -> onSuccess: (Output -> unit)
    ///>                  -> onFailure: (exn -> unit)
    ///>                  -> unit
    ///
    /// We would like to wrap the asynchronous operation as a job with following
    /// signature:
    ///
    ///> val opAsJob: Input -> Job<Output>
    ///
    /// This can be done by using a write once variable, which will be filled
    /// with the result of the operation, and using `bind` to capture the
    /// current scheduler:
    ///
    ///> let opAsJob input = Job.Scheduler.bind <| fun scheduler ->
    ///>   let resultIVar = IVar ()
    ///>   let handleWith fill result =
    ///>     fill resultIVar result |> Scheduler.start scheduler
    ///>   ioWithCallback input
    ///>    <| handleWith IVar.fill
    ///>    <| handleWith IVar.fillFailure
    ///>   resultIVar
    ///
    /// Note that the `Scheduler.start` operation is used to explicitly start
    /// the fill operation on the captured scheduler.
    ///
    /// There are other similar examples as reference implementations of various
    /// Hopac primitives.  See, for example, the reference implementations of
    /// `fromBeginEnd` and `Job.awaitTask`.

#endif
    val inline bind: (Scheduler -> #Job<'x>) -> Job<'x>

    /// Returns a job that returns the scheduler under which the job is being
    /// run.  `get ()` is equivalent to `bind result`.
    val inline get: unit -> Job<Scheduler>

    /// Returns a job that ensures that the immediately following operation will
    /// be executed on a Hopac worker thread.
    val inline switchToWorker: unit -> Job<unit>

    /// `isolate u2x` is like `thunk u2x`, but it is ensured that the blocking
    /// invocation of `u2x` does not prevent scheduling of other work.
    val inline isolate: (unit -> 'x) -> Job<'x>

  /// Operations on the built-in pseudo random number generator (PRNG) of Hopac.
#if DOC
  ///
  /// Note that every actual Hopac worker thread has its own PRNG state and is
  /// initialized with a distinct seed.  However, when you `Hopac.start` or
  /// `Hopac.run` jobs from some non worker thread, it is possible that
  /// successive executions generate the same sequence of numbers.  In the
  /// extremely rare case that could be a problem, use `Hopac.queue` or
  /// `switchToWorker`.
#endif
  module Random =
    /// `bind r2xJ` creates a job that calls the given job constructor with a
    /// pseudo random 64-bit unsigned integer.
    val inline bind: (uint64 -> #Job<'x>) -> Job<'x>

    /// `map r2x` is equivalent to `bind (r2x >> result)`.
    val  inline map: (uint64 ->      'x)  -> Job<'x>

    /// Returns a job that generates a pseudo random 64-bit unsigned integer.
    /// `get ()` is equivalent to `bind result`.
    val inline get: unit -> Job<uint64>

////////////////////////////////////////////////////////////////////////////////

#if DOC
/// Represents a first-class selective synchronous operation.
///
/// The inspiration for alternatives comes from the events of Concurrent ML.
/// The term ''alternative'' was chosen, because the term ''event'' is already
/// widely used in .Net.
///
/// Simpler forms of selective synchronization exists in various languages.  For
/// example, the occam language has an `alt` statement, the Go language has a
/// `select` statement and Clojure's core.async has an `alt` function.  In Hopac
/// and Concurrent ML, selective synchronous operations are not limited to
/// primitive message passing operations (see `Ch.give` and `Ch.take`), but are
/// instead first-class values (see `choose`) and can be extended with
/// user-defined code (see `afterJob` and `withNackJob`) allowing the
/// encapsulation of concurrent protocols as selective synchronous operations.
///
/// The idea of alternatives is to allow one to introduce new selective
/// synchronous operations to be used with non-determinic choice aka `choose`.
/// Obviously, when you have a concurrent server that responds to some protocol,
/// you don't have to perform the protocol as a selective synchronous operation.
/// However, if you do encapsulate the protocol as a selective synchronous
/// operation, you can then combine the operation with other selective
/// synchronous operations.  That is the essence of Hopac and CML.
///
/// If a selective synchronous operation is not committed to then it should have
/// essentially no effect.  In order to create such alternatives, one may take
/// advantage of idempotency, rendezvous and negative acknowledgments.  Here are
/// few rules of thumb:
///
/// - If you don't need to send arguments to the server, you can synchronize
/// using a `take` operation on the server's reply channel.  E.g. an operation
/// to take an element from a concurrent buffer.
///
/// - If you don't need a result from the server, aside from acknowledgment that
/// the operation has been performed, you can synchronize using a `give`
/// operation on the server's request channel.  E.g. an operation to remove a
/// specified element from a concurrent bag.
///
/// - If you have an idempotent operation, you can use `prepareJob` to send the
/// arguments and a write once variable to the server and then synchronize using
/// a `read` operation on the write once variable for the reply.  E.g. request
/// to receive a timeout event.
///
/// - If you have a non-idempotent operation, you can use `withNackJob` to send
/// the arguments, negative acknowledgment token and a channel to the server and
/// then synchronize using a `take` operation on the channel for the reply.  See
/// `withNackJob` for an illustrative toy example.
type Alt<'x> =
  /// `Alt<'x>` is a subtype of `Job<'x>`.  You can use an alternative in any
  /// context that requires a job.
  inherit Job<'x>
#endif

/// Operations on first-class synchronous operations or alternatives.
module Alt =
  //# Basic alternatives

  /// Creates an alternative that is always available and results in the given
  /// value.
  ///
  /// Note that when there are alternatives immediately available in a choice,
  /// the first such alternative will be committed to.
  val inline always: 'x   -> Alt<'x>

  /// Returns an alternative that is always available and results in the unit
  /// value.  `unit ()` is an optimized version of `always ()`.
  val   inline unit: unit -> Alt<unit>

  /// Returns an alternative that can be committed to once and that produces the
  /// given value.
#if DOC
  ///
  /// `once` is basically an optimized version of
  ///
  ///> let once x =
  ///>   let xCh = Ch ()
  ///>   xCh *<+ x |> run
  ///>   paranoid xCh
#endif
  val inline once: 'x -> Alt<'x>

  /// Creates an alternative that is never available.
  ///
  /// Note that synchronizing on `never ()`, without other alternatives, is
  /// equivalent to performing `abort ()`.
  val inline never: unit -> Alt<'x>

  /// Returns an alternative that is never available.  `zero ()` is an optimized
  /// version of `never ()`.
  val  inline zero: unit -> Alt<unit>

  /// `Ignore xA` is equivalent to `xA ^-> always ()`.
  val Ignore: Alt<_> -> Alt<unit>

  //# Before actions

  /// Creates an alternative that is computed at instantiation time with the
  /// given job.  See also: `*<-=>-`, `prepareFun`, `withNackJob`.
#if DOC
  ///
  /// `prepareJob` allows client-server protocols that do not require the server
  /// to be notified when the client aborts the transaction to be encapsulated
  /// as selective operations.  For example, the given job may create and send a
  /// request to a server and then return an alternative that waits for the
  /// server's reply.
  ///
  /// Reference implementation:
  ///
  ///> let prepareJob u2xAJ = withNackJob (ignore >> u2xAJ)
  ///
  /// Note that, like with `withNackJob`, it is essential to avoid blocking
  /// inside `prepareJob`.
#endif
  val prepareJob: (unit -> #Job<#Alt<'x>>) -> Alt<'x>

  /// Creates an alternative that is computed at instantiation time with the
  /// given job.  `prepare xAJ` is equivalent to `prepareJob <| fun () -> xAJ`.
  val prepare:              Job<#Alt<'x>>  -> Alt<'x>

  /// Creates an alternative that is computed at instantiation time with the
  /// given thunk.  See also: `*<-=>-`, `prepareJob`.
#if DOC
  ///
  /// `prepareFun` is an optimized weaker form of `prepareJob` that can be used
  /// when no concurrent operations beyond the returned alternative are required
  /// by the encapsulated request protocol.
  ///
  /// Reference implementation:
  ///
  ///> let prepareFun u2xA = prepareJob (u2xA >> result)
#endif
  val inline prepareFun: (unit ->      #Alt<'x>)  -> Alt<'x>

  //# Negative acknowledgments

  /// Creates an alternative that is computed at instantiation time with the
  /// given job constructed with a negative acknowledgment alternative.  See
  /// also: `*<+->-`, `withNackFun`, `prepareJob`.
#if DOC
  ///
  /// `withNackJob` allows client-server protocols that do require the server to
  /// be notified when the client aborts the transaction to be encapsulated as
  /// selective operations.  The negative acknowledgment alternative will be
  /// available in case some other instantiated alternative involved in the
  /// choice is committed to instead.
  ///
  /// Like `prepare`, `withNackJob` is typically used to encapsulate the client
  /// side operation of a concurrent protocol.  The client side operation
  /// typically constructs a request, containing the negative acknowledgment
  /// alternative, sends it to a server and then returns an alternative that
  /// waits for a rendezvous with the server.  In case the client later commits
  /// to some other alternative, the negative acknowledgment token becomes
  /// available and the server can also abort the operation.
  ///
  /// Here is a simple example of an operation encapsulated using `withNackJob`.
  /// The idea is that we have a server that maintains a counter.  Clients can
  /// request the server to increment the counter by a specific amount and
  /// return the incremented counter value.  We further want to make it so that
  /// in case the client does not commit to the operation, the counter in the
  /// server is not updated.
  ///
  /// Here is the server communication channel and the server loop:
  ///
  ///> let counterServer : Ch<int * Promise<unit> * Ch<int>> =
  ///>   let reqCh = Ch ()
  ///>   server << Job.iterate 0 <| fun oldCounter ->
  ///>     reqCh >>= fun (n, nack, replyCh) ->
  ///>     let newCounter = oldCounter + n
  ///>     replyCh *<- newCounter ^->. newCounter <|>
  ///>     nack                   ^->. oldCounter
  ///>   reqCh
  ///
  /// Note how the server tries to synchronize on either giving the new counter
  /// value to the client or the negative acknowledgment.
  ///
  /// Here is the encapsulated client side operation:
  ///
  ///> let incrementBy n : Alt<int> = Alt.withNackJob <| fun nack ->
  ///>   let replyCh = Ch ()
  ///>   counterServer *<+ (n, nack, replyCh) >>-.
  ///>   replyCh
  ///
  /// Note that the above can be expressed more concisely using `*<+->-`.
  ///
  /// The client side operation just sends the negative acknowledgment to the
  /// server as a part of the request.  It is essential that a synchronous
  /// rendezvous via a channel, rather than e.g. a write once variable, is used
  /// for the reply.  It is also essential to avoid blocking inside
  /// `withNackJob`, which is why an asynchronous send is used inside the client
  /// side operation.
  ///
  /// Note that if an alternative created with `withNackJob` is not
  /// instantiated, then no negative acknowledgment is created.  For example,
  /// given an alternative of the form `always () <|> withNackJob (...)` the
  /// `withNackJob` alternative is never instantiated.
#endif
  val inline withNackJob: (Promise<unit> -> #Job<#Alt<'x>>) -> Alt<'x>

  /// `withNackFun n2xA` is equivalent to `withNackJob (Job.lift n2xA)`.
  val inline withNackFun: (Promise<unit> ->      #Alt<'x>)  -> Alt<'x>

  /// Returns a new alternative that that makes it so that the given job will be
  /// started as a separate concurrent job if the given alternative isn't the
  /// one being committed to.  See also: `wrapAbortFun`, `withNackJob`.
#if DOC
  ///
  /// `wrapAbortJob` and `withNackJob` have roughly equivalent expressive power
  /// and `wrapAbortJob` can be expressed in terms of `withNackJob`.  Sometimes
  /// `wrapAbortJob` more directly fits the desired usage than `withNackJob` and
  /// should be preferred in those cases.  In particular, consider using
  /// `wrapAbortJob`, when you have an alternative whose implementation is
  /// similar to the following reference implementation:
  ///
  ///> let wrapAbortJob (abortAct: Job<unit>) (evt: Alt<'x>) : Alt<'x> =
  ///>   Alt.withNackJob <| fun nack ->
  ///>   nack >>=. abortAct |> Job.start >>-.
  ///>   evt
  ///
  /// Historical note: Originally Concurrent ML only provided a corresponding
  /// combinator named `wrapAbort`.  Later Concurrent ML changed to provide only
  /// `withNack` as a primitive, because it is a better fit for most use cases,
  /// and `wrapAbort` could be expressed in terms of it.  Racket only provides
  /// `withNack` and, under Racket's model, `withNack` cannot be expressed in
  /// terms of `wrapAbort`.
#endif
  val wrapAbortJob:      Job<unit> -> Alt<'x> -> Alt<'x>

  /// `wrapAbortFun u2u xA` is equivalent to `wrapAbortJob (Job.thunk u2u) xA`.
  val wrapAbortFun: (unit -> unit) -> Alt<'x> -> Alt<'x>

  //# Choices

  /// Creates an alternative that is available when any one of the given
  /// alternatives is.  See also: `choosy`, `<|>`.
  ///
  /// Note that `choose []` is equivalent to `never ()`.
#if DOC
  ///
  /// Reference implementation:
  ///
  ///> let choose xAs = Alt.prepareFun <| fun () ->
  ///>   Seq.foldBack (<|>) xAs <| never ()
  ///
  /// Above, `Seq.foldBack` has the obvious meaning.  Alternatively we could
  /// define `xA1 <|> xA2` to be equivalent to `choose [xA1; xA2]` and consider
  /// `choose` as primitive.
#endif
  val choose:   seq<#Alt<'x>> -> Alt<'x>

  /// `choosy xAs` (read: choose array) is an optimized version of `choose xAs`
  /// when `xAs` is an array.  Do not write `choosy (Seq.toArray xAs)` instead
  /// of `choose xAs` unless the resulting alternative is reused many times.
#if DOC
  ///
  /// One dominating cost in .Net is memory allocations.  To choose between
  /// various forms of non-determistic choice, the following low level
  /// implementation details may be of interest.
  ///
  ///> choosy [| ... |]
  ///
  /// Creation: 1 array + 1 object.  Use: 1 object.  Total cost: 3 allocations.
  ///
  ///> xA1 <|> xA2
  ///
  /// Creation: 1 object.  Use: 1 object.  Total cost: 2 allocations.
  ///
  ///> xA1 <|> xA2 <|> xA3
  ///
  /// Creation: 2 objects.  Use: 2 objects.  Total cost: 4 allocations.
  ///
  /// If you are choosy, then when choosing between 2 or 3 alternatives, `<|>`
  /// is likely to be fastest.  When choosing between 4 or more alternatives,
  /// `choosy` is likely to be fastest.
#endif
  val choosy: array<#Alt<'x>> -> Alt<'x>

  //# Randomization

  /// Creates an alternative that is computed at instantiation time with the
  /// the given function, which will be called with a pseudo random 64-bit
  /// unsigned integer.  See also: `Random.bind`.
  val inline random: (uint64 -> #Alt<'x>) -> Alt<'x>

  /// `chooser xAs` is like `choose xAs` except that the order in which the
  /// alternatives from the sequence are instantiated will be determined at
  /// random each time the alternative is used.  See also: `<~>`.
#if DOC
  ///
  /// Note that randomization only applies to the instantiation order.  It makes
  /// no difference after instantiation.
#endif
  val chooser:  seq<#Alt<'x>> -> Alt<'x>

  //# After actions

  /// Creates an alternative whose result is passed to the given job constructor
  /// and processed with the resulting job after the given alternative has been
  /// committed to.  This is the same as `^=>` with the arguments flipped.
#if DOC
  ///
  /// Note that although this operator has a type somewhat similar to a monadic
  /// bind operation, alternatives do not form a monad (with the `always`
  /// alternative constructor).  So called Transactional Events do form a monad,
  /// but require a more complex synchronization protocol.
#endif
  val inline afterJob: ('x -> #Job<'y>) -> Alt<'x> -> Alt<'y>

  /// Creates an alternative which is committed to when the given alternative
  /// is committed to. Once committed, the given alternative's result is mapped
  /// using the given function, providing the final result.
  ///
  /// `xA |> afterFun x2y` is equivalent to `xA |> afterJob (x2y >> result)`.
  /// This is the same as `^->` with the arguments flipped.
  val inline afterFun: ('x ->      'y)  -> Alt<'x> -> Alt<'y>

  //# Exception handling

  /// Creates an alternative that has the effect of raising the specified
  /// exception.  `raises e` is equivalent to `prepareFun <| fun () -> raise e`.
  val raises: exn -> Alt<_>

  /// Implements the `try-in-unless` exception handling construct for
  /// alternatives.  Both of the continuation jobs `'x -> Job<'y>`, for success,
  /// and `exn -> Job<'y>`, for failure, are invoked from a tail position.
  ///
  /// Exceptions from both before and after the commit point can be handled.  An
  /// exception that occurs before a commit point, from the user code in a
  /// `prepareJob`, or `withNackJob`, results in treating that exception as the
  /// commit point.
  ///
  /// Note you can also use function or job level exception handling before the
  /// commit point within the user code in a `prepareJob` or `withNackJob`.
  val tryIn: Alt<'x> -> ('x -> #Job<'y>) -> (exn -> #Job<'y>) -> Alt<'y>

  /// Implements a variation of the `try-finally` exception handling construct
  /// for alternatives.  The given action, specified as a job, is executed after
  /// the alternative has been committed to, whether the alternative fails or
  /// completes successfully.  Note that the action is not executed in case the
  /// alternative is not committed to.  Use `withNackJob` to attach the action
  /// to the non-committed case.
#if DOC
  ///
  /// Reference implementation:
  ///
  ///> let tryFinallyJob xA uJ =
  ///>   tryIn xA
  ///>    <| fun x -> uJ >>-. x
  ///>    <| fun e -> uJ >>-! e
#endif
  val tryFinallyJob: Alt<'x> ->      Job<unit> -> Alt<'x>

  /// Implements a variation of the `try-finally` exception handling construct
  /// for alternatives.  The given action, specified as a function, is executed
  /// after the alternative has been committed to, whether the alternative fails
  /// or completes successfully.  Note that the action is not executed in case
  /// the alternative is not committed to.  Use `withNackJob` to attach the
  /// action to the non-committed case.
#if DOC
  ///
  /// Reference implementation:
  ///
  ///> let tryFinallyFun xA u2u =
  ///>   tryFinallyJob xA
  ///>    <| Job.thunk u2u
#endif
  val tryFinallyFun: Alt<'x> -> (unit -> unit) -> Alt<'x>

  //# Interop

  /// Creates an alternative that, when instantiated, starts the cancellable
  /// asynchronous operation defined by the given `doBegin`, `doEnd` and
  /// `doCancel` operations and waits for it to complete, after which the
  /// alternative becomes available.  If some other alternative is committed to
  /// a in a choice before the operation completes, then the operation is
  /// cancelled. See also: `Job.fromBeginEnd`.
  val inline fromBeginEnd: doBegin: (AsyncCallback * obj -> IAsyncResult)
                 -> doEnd: (IAsyncResult -> 'x)
                 -> doCancel: (IAsyncResult -> unit)
                 -> Alt<'x>

  /// Creates an alternative that, when instantiated, starts the given
  /// cancellable async operation and waits for it to complete, after which the
  /// alternative becomes available.  If some other alternative is committed to
  /// in a choice before the operation completes, then the operation is
  /// cancelled.  See also: `Job.fromAsync`.
#if DOC
  ///
  /// Note that the async operation is started on whichever thread (and
  /// synchronization context) the job happens to be executed on.  Transfer the
  /// async operation explicitly, e.g. by using `Async.SwitchToContext`, to the
  /// desired context when necessary.
#endif
  val inline fromAsync: Async<'x> -> Alt<'x>

  /// Creates an async operation that starts the given alternative and waits for
  /// it to be committed to.  If the async operation is cancelled before the
  /// alternative is committed to, an attempt is made to also cancel the
  /// alternative by making a cancellation alternative available.  Note that
  /// cancellation is not transactional and `Alt.toAsync >> Alt.fromAsync` is
  /// not the identity function.  See also: `Job.toAsync`.
  val   toAsync: Alt<'x> -> Async<'x>

  /// Creates an alternative that, when instantiated, calls the given function
  /// with a cancellation token to start a cancellable task and waits for it to
  /// complete, after which the alternative becomes available.  If some other
  /// alternative is committed to in a choice before the task completes, then
  /// the token will be cancelled.  See also: `Job.fromTask`.
  val inline fromTask:     (CancellationToken -> Task<'x>) -> Alt<'x>

  /// Creates an alternative that, when instantiated, calls the given function
  /// with a cancellation token to start a cancellable task and waits for it to
  /// complete, after which the alternative becomes available.  If some other
  /// alternative is committed to in a choice before the task completes, then
  /// the token will be cancelled.  See also: `Job.fromUnitTask`.
  val inline fromUnitTask: (CancellationToken -> Task)     -> Alt<unit>

  //# Debugging

  /// Given an alternative, creates a new alternative that behaves exactly like
  /// the given alternative, except that the new alternative obviously cannot be
  /// directly downcast to the underlying type of the given alternative.  This
  /// operation is provided for debugging purposes.  You can always break
  /// abstractions using reflection.  See also: `Job.paranoid`.
  val paranoid: Alt<'x> -> Alt<'x>

////////////////////////////////////////////////////////////////////////////////

/// Operations on a wall-clock timer.
[<Obsolete "The `Timer` module will be removed.  Use the `Hopac` module.">]
module internal Timer =

  /// Operations on the global wall-clock timer.  The global timer is implicitly
  /// associated with the global scheduler.
  [<Obsolete "The `Timer` module will be removed.  Use the `Hopac` module.">]
  module Global =
    /// Creates an alternative that, after instantiation, becomes available
    /// after the specified time span.
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
    /// Timeouts, like other alternatives, can also directly be used as job
    /// level operations.  For example, using the above definition of `after1s`
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
    /// that always waits for a timeout is held live by the timeout.  Such
    /// servers need to support an explicit kill protocol.
    ///
    /// When a timeout is used as a part of a non-deterministic choice, e.g.
    /// `timeOut span <|> somethingElse`, and some other alternative is
    /// committed to before the timeout expires, the memory held by the timeout
    /// can be released by the timer mechanism.  However, when a timeout is not
    /// part of a non-deterministic choice, e.g.
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
    ///
    /// A timeout of zero is optimized to an alternative that is immediately
    /// available, while a negative timeout results in an `Alt` that is never
    /// available.  See `idle` for an alternative that yields the thread of
    /// execution to any ready work before becoming available.
#endif
    [<Obsolete "The `Timer` module will be removed.  Use the `Hopac` module.">]
    val timeOut:       TimeSpan -> Alt<unit>

    /// `timeOutMillis n` is equivalent to `timeOut << TimeSpan.FromMilliseconds
    /// <| float n`.
    [<Obsolete "The `Timer` module will be removed.  Use the `Hopac` module.">]
    val timeOutMillis: int      -> Alt<unit>

    /// Creates an alternative that yields the thread of execution to any ready
    /// work and then becomes available.
#if DOC
    ///
    /// This is similar to `timeOutMillis 0` except that it yields the current
    /// thread of execution to any other ready work before attempting to
    /// become available.
    ///
    /// When executing multiple concurrent jobs that do not have natural points
    /// where they yield execution, `idle` allows progress to be made by other
    /// ready work.
#endif
    [<Obsolete "The `Timer` module will be removed.  Use the `Hopac` module.">]
    val idle:                      Alt<unit>

#if DOC
/// Represents a promise to produce a result at some point in the future.
///
/// Promises are used when a parallel job is started for the purpose of
/// computing a result.  When multiple parallel jobs need to be started to
/// compute results in parallel in regular patterns, combinators such as `<*>`,
/// `Job.conCollect` and `Seq.Con.mapJob` may be easier to use and provide
/// improved performance.
type Promise<'x> =
  /// `Promise<'x>` is a subtype of `Alt<'x>` and `xPr :> Alt<'x>` is equivalent
  /// to `Promise.read xPr`.
  inherit Alt<'x>

  /// Creates a promise that will never be fulfilled.
  new: unit    -> Promise<'x>

  /// Creates a promise whose value is computed lazily with the given job when
  /// an attempt is made to read the promise.  Although the job is not started
  /// immediately, the effect is that the delayed job will be run as a separate
  /// job, which means it is possible to communicate with it as long the delayed
  /// job is started before trying to communicate with it.  See also: `memo`.
  new: Job<'x> -> Promise<'x>

  /// Creates a promise with the given value.
  new:     'x  -> Promise<'x>

  /// Creates a promise with the given failure exception.
  new: exn     -> Promise<'x>
#endif

/// Operations on promises.
module Promise =
  /// Creates a job that creates a promise, whose value is computed with the
  /// given job, which is scheduled to be run as a separate concurrent job.  See
  /// also: `start`, `Job.queue`.
  val queue: Job<'x> -> Job<Promise<'x>>

  /// Creates a job that creates a promise, whose value is computed with the
  /// given job, which is immediately started to run as a separate concurrent
  /// job.  See also: `queue`, `Job.queue`.
  val start: Job<'x> -> Job<Promise<'x>>

  /// Creates an alternative for reading the promise.  If the promise was
  /// delayed, it is started as a separate job.
  val inline read: Promise<'x> -> Alt<'x>

  /// Immediate or non-workflow operations on promises.
  module Now =
    /// Creates a promise whose value is computed lazily with the given job when
    /// an attempt is made to read the promise.  Although the job is not started
    /// immediately, the effect is that the delayed job will be run as a
    /// separate job, which means it is possible to communicate with it as long
    /// the delayed job is started before trying to communicate with it.
    val inline delay: Job<'x> -> Promise<'x>

    /// Creates a promise with the given value.
    val inline withValue:   'x  -> Promise<'x>

    /// Creates a promise with the given failure exception.
    val inline withFailure: exn -> Promise<'x>

    /// Creates a promise that will never be fulfilled.
    val never: unit -> Promise<'x>

    /// Returns true iff the given promise has already been fulfilled (either
    /// with a value or with a failure).
    ///
    /// This operation is mainly provided for advanced uses of promises such as
    /// when creating more complex data structures that make internal use of
    /// promises.  Using this to poll promises is not generally a good idea.
    val isFulfilled: Promise<'x> -> bool

    /// Returns the value or raises the failure exception that the promise has
    /// been fulfilled with.  It is considered an error if the promise has not
    /// yet been fulfilled.
    ///
    /// This operation is mainly provided for advanced uses of promises such as
    /// when creating more complex data structures that make internal use of
    /// promises.  Using this to poll promises is not generally a good idea.
    val get: Promise<'x> -> 'x

////////////////////////////////////////////////////////////////////////////////

#if DOC
/// Represents a synchronous channel.
///
/// Channels provide a simple rendezvous mechanism for concurrent jobs and are
/// designed to be used as the building blocks of selective synchronous
/// abstractions.
///
/// Channels are lightweight objects and it is common to allocate fresh channels
/// for short-term, possibly even one-shot, communications.  When simple
/// rendezvous is not needed in a one-shot communication, a write once variable,
/// `IVar`, may offer slightly better performance.
///
/// Channels are optimized for synchronous message passing, which can often be
/// done without buffering.  Channels also provide an asynchronous `Ch.send`
/// operation, but in situations where buffering is needed, some other message
/// passing mechanism such as a bounded mailbox, `BoundedMb<_>`, or unbounded
/// mailbox, `Mailbox<_>`, may be preferable.
type Ch<'x> =
  /// `Ch<'x>` is a subtype of `Alt<'x>` and `xCh :> Alt<'x>` is equivalent to
  /// `Ch.take xCh`.
  inherit Alt<'x>

  /// Creates a new channel.
  new: unit -> Ch<'x>
#endif

/// Operations on synchronous channels.
module Ch =
  /// Creates an alternative that, at instantiation time, offers to give the
  /// given value on the given channel, and becomes available when another job
  /// offers to take the value.  See also: `*<-`.
  val inline give: Ch<'x> -> 'x -> Alt<unit>

  /// Creates a job that sends a value to another job on the given channel.  A
  /// send operation is asynchronous.  In other words, a send operation does not
  /// wait for another job to give the value to.
#if DOC
  ///
  /// Note that channels have been optimized for synchronous operations; an
  /// occasional send can be efficient, but when sends are queued, performance
  /// maybe be significantly worse than with a `Mailbox` optimized for
  /// buffering.  See also: `*<+`.
#endif
  val inline send: Ch<'x> -> 'x -> Job<unit>

  /// Creates an alternative that, at instantiation time, offers to take a value
  /// from another job on the given channel, and becomes available when another
  /// job offers to give a value.
  val inline take: Ch<'x> -> Alt<'x>

  /// Polling, or non-blocking, operations on synchronous channels.
#if DOC
  ///
  /// Note that polling operations only make sense when the other side of the
  /// communication is blocked waiting on the channel.  If both a giver and a
  /// taker use polling operations on a channel, it is not guaranteed that
  /// communication will ever happen.
  ///
  /// Also note that a job that performs arbitrarily many polling operations
  /// without blocking should not be used in a cooperative system, like Hopac,
  /// because such a job completely uses up a single core and prevents other
  /// ready jobs from being executed.  Jobs that perform polling should be
  /// designed so that after a finitely many poll operations they will block
  /// waiting for communication.
#endif
  module Try =
    /// Creates a job that attempts to give a value to another job waiting on
    /// the given channel.  The result indicates whether a value was given or
    /// not.  Note that the other side of the communication must be blocked on
    /// the channel for communication to happen.
    val inline give: Ch<'x> -> 'x -> Job<bool>

    /// Creates a job that attempts to take a value from another job waiting on
    /// the given channel.  Note that the other side of the communication must
    /// be blocked on the channel for communication to happen.
    val inline take: Ch<'x> -> Job<option<'x>>

  /// Immediate or non-workflow operations on synchronous channels.
  module Now =
    /// Sends the given value to the specified channel.  `Ch.Now.send xCh x` is
    /// equivalent to `Ch.send xCh x |> Hopac.start`.
    ///
    /// Note that using this function in a job workflow is not optimal and you
    /// should use `Ch.send` instead.
    val send: Ch<'x> -> 'x -> unit

////////////////////////////////////////////////////////////////////////////////

#if DOC
/// Represents a write once variable.
///
/// Write once variables are designed for and most commonly used for getting
/// replies from concurrent servers and asynchronous operations, but can also be
/// useful for other purposes such as for one-shot events and for implementing
/// incremental, but immutable, concurrent data structures.
///
/// Because it is common to need to be able to communicate either an expected
/// successful result or an exceptional failure in typical use cases of write
/// once variables, direct mechanisms are provided for both.  The implementation
/// is optimized in such a way that the ability to report an exceptional failure
/// does not add overhead to the expected successful usage scenarios.
///
/// Write once variables are lightweight objects and it is typical to always
/// just create a new write once variable when one is needed.  In most cases, a
/// write once variable will be slightly more lightweight than a channel.  This
/// is possible because write once variables do not support simple rendezvous
/// like channels do.  When simple rendezvous is necessary, a channel should be
/// used instead.
type IVar<'x> =
  /// `IVar<'x>` is a subtype of `Promise<'x>` and `IVar.read xI` is equivalent
  /// to `xI :> Alt<'x>`.
  inherit Promise<'x>

  /// Creates a new write once variable.
  new: unit -> IVar<'x>

  /// Creates a new write once variable with the given value.
  new: 'x   -> IVar<'x>

  /// Creates a new write once variable with the given failure exception.
  new: exn  -> IVar<'x>
#endif

/// Operations on write once variables.
module IVar =
  /// Creates a job that writes the given value to the given write once
  /// variable.  It is an error to write to a single write once variable more
  /// than once.
#if DOC
  ///
  /// In most use cases of write once variables the write once assumption
  /// naturally follows from the property that there is only one concurrent job
  /// that may ever write to a particular write once variable.  If that is not
  /// the case, then you should likely use some other communication primitive.
  /// See also: `*<=`, `tryFill`, `fillFailure`.
#endif
  val    inline fill: IVar<'x> -> 'x -> Job<unit>

  /// Creates a job that tries to write the given value to the given write once
  /// variable.  No operation takes place and no error is reported in case the
  /// write once variable has already been written to.
#if DOC
  ///
  /// In most use cases of write once variables it should be clear that a
  /// particular variable is written to at most once, because there is only one
  /// specific concurrent job that may write to the variable, and `tryFill`
  /// should not be used as a substitute for not understanding how the program
  /// behaves.  However, in some case it can be convenient to use a write once
  /// variable as a single shot event and there may be several concurrent jobs
  /// that initially trigger the event.  In such cases, you may use `tryFill`.
#endif
  val inline tryFill: IVar<'x> -> 'x -> Job<unit>

  /// Creates a job that writes the given exception to the given write once
  /// variable.  It is an error to write to a single `IVar` more than once.  See
  /// also: `*<=!`, `fill`.
  val    inline fillFailure: IVar<'x> -> exn -> Job<unit>

  /// Creates a job that tries to write the given exception to the given write
  /// once variable.  No operation takes place and no error is reported in case
  /// the write once variable has already been written to.
  val inline tryFillFailure: IVar<'x> -> exn -> Job<unit>

  /// Creates an alternative that becomes available after the write once
  /// variable has been written to.
  val inline read: IVar<'x> -> Alt<'x>

  /// Immediate or non-workflow operations on write once variables.
  module Now =
    /// Returns true iff the given write once variable has already been filled
    /// (either with a value or with a failure).
    ///
    /// This operation is mainly provided for advanced uses of write once
    /// variables such as when creating more complex data structures that make
    /// internal use of write once variables.  Using this to poll write once
    /// variables is not generally a good idea.
    val isFull: IVar<'x> -> bool

    /// Returns the value or raises the failure exception written to the write
    /// once variable.  It is considered an error if the write once variable has
    /// not yet been written to.
    ///
    /// This operation is mainly provided for advanced uses of write once
    /// variables such as when creating more complex data structures that make
    /// internal use of write once variables.  Using this to poll write once
    /// variables is not generally a good idea.
    val get: IVar<'x> -> 'x

////////////////////////////////////////////////////////////////////////////////

#if DOC
/// Represents a dynamic latch.  Latch is similar to the .Net `CountdownEvent`.
///
/// Latches are used for determining when a finite set of parallel jobs is done.
/// If the size of the set is known a priori, then the latch can be initialized
/// with the size as initial count and then each job just decrements the latch.
///
/// If the size is unknown (dynamic), then a latch is initialized with a count
/// of one, the a priori known jobs are queued to the latch and then the latch
/// is decremented.  A queue operation increments the count immediately and
/// decrements the count after the job is finished.
///
/// Both a first-order interface, with `create`, `increment` and `decrement`
/// operations, and a higher-order interface, with `within`, `holding`, `queue`
/// and `queueAsPromise` operations, are provided for programming with latches.
type Latch =
  /// `Latch` is a subtype of `Alt<unit>` and `Latch.await l` is equivalent to
  /// `l :> Alt<unit>`.
  inherit Alt<unit>

  /// Creates a new latch with the specified initial count.
  new: int -> Latch
#endif

/// Operations on latches.
module Latch =
  //# Await

  /// Returns an alternative that becomes available once the latch opens.
  val inline await: Latch -> Alt<unit>

  //# Higher-order interface

  /// Creates a job that creates a new latch, passes it to the given function to
  /// create a new job to run and then awaits for the latch to open.
  val within: (Latch -> #Job<'x>) -> Job<'x>

  /// Creates a job that runs the given job holding the specified latch.  Note
  /// that the latch is only held while the given job is being run.  See also
  /// `Latch.queue`.
  val holding: Latch -> Job<'x> -> Job<'x>

  /// Creates a job that queues the given job to run as a separate concurrent
  /// job and holds the latch until the queued job either returns or fails with
  /// an exception.  See also `Latch.queueAsPromise`.
  val queue: Latch -> Job<unit> -> Job<unit>

  /// Creates a job that queues the given job to run as a separate concurrent
  /// job and holds the latch until the queued job either returns or fails with
  /// an exception.  A promise is returned for observing the result or failure
  /// of the queued job.
  val queueAsPromise: Latch -> Job<'x> -> Job<Promise<'x>>

  //# First-order interface

  /// Returns a job that explicitly decrements the counter of the latch.  When
  /// the counter reaches `0`, the latch becomes open and operations awaiting
  /// the latch are resumed.
  val inline decrement: Latch -> Job<unit>

  /// Immediate operations on latches.
  module Now =
    /// Increments the counter of the latch.
    val inline increment: Latch -> unit

////////////////////////////////////////////////////////////////////////////////

#if DOC
/// Represents a serialized variable.
///
/// A serialized variable can be either empty or full.  When a job makes an
/// attempt to take the value of an empty variable, the job is suspended until
/// some other job fills the variable with a value.  At any one time there
/// should only be at most one job that holds the state to be written to a
/// serialized variable.  If this cannot be guaranteed, in other words, there
/// might be two or more jobs trying to fill a serialized variable, then you
/// should not be using serialized variables.  Indeed, the idea is that access
/// to the state is serialized meaning that one and only one concurrent job has
/// access to the state at any one time.
///
/// For example, one could declare a variable holding a shared map:
///
///> let sharedMap = MVar Map.empty
///
/// The map can then be accessed from multiple concurrent jobs, one job at a
/// time, using the `MVar` operations:
///
///> MVar.mutateFun (Map.add key value) sharedMap
///> MVar.read sharedMap >>- Map.tryFind key
///
/// Another way to put the idea of serialized variables is that the variable
/// acts as a mechanism for passing a permission token, the value contained by
/// the variable, from one concurrent job to another.  Only the concurrent job
/// that holds the token is allowed to fill the variable.  When used in this
/// way, operations on the variable appear as atomic and access to the state
/// will be serialized.
///
/// For example, here is an implementation of a synchronization object similar
/// to the .Net `AutoResetEvent` using serialized variables:
///
///> type AutoResetEvent (init: bool) =
///>   let set = if init then MVar (()) else MVar ()
///>   let unset = if init then MVar () else MVar (())
///>   member this.Set = unset <|> set >>= MVar.fill set
///>   member this.Wait = set ^=> MVar.fill unset
///
/// The idea is to use two serialized variables to represent the state of the
/// synchronization object.  At most one of the variables, representing the
/// state of the synchronization object, is full at any time.
///
/// In general, aside from a possible initial `fill` operation, an access to a
/// serialized variable should be of the form `take >>= ... fill` or of the form
/// `read`.  Note that this follows naturally if you initialize a serialized
/// variable with a value and then use the read, modify and mutate operations.
/// On the other hand, accesses of the form `fill` and `read >>= ... fill` are
/// unsafe.  A `take` operation effectively grants permission to the job to
/// access the shared state.  The `fill` operation then gives that permission to
/// the next job that wants to access the shared state.
///
/// WARNING: Unfortunately, `MVar`s are easy to use unsafely.  Do not use an
/// `MVar` to pass information from a client to a server, for example.  Use a
/// `Ch` or `Mailbox` for that.  Note that if you are familiar with the `MVar`
/// abstraction provided by Concurrent Haskell, then it is important to realize
/// that the semantics and intended usage of Hopac's and Concurrent ML's `MVar`
/// are quite different.  The `MVar` of Concurrent Haskell is a bit like a
/// simplified `Ch` with a buffer of one element and some additional operations.
/// The `MVar` of Hopac and Concurrent ML does not allow usage as a kind of
/// buffered channel.
type MVar<'x> =
  /// `MVar<'x>` is a subtype of `Alt<'x>` and `xM :> Alt<'x>` is equivalent to
  /// `MVar.take xM`.
  inherit Alt<'x>

  /// Creates a new serialized variable that is initially empty.
  new: unit -> MVar<'x>

  /// Creates a new serialized variable that initially contains the given value.
  new: 'x   -> MVar<'x>
#endif

/// Operations on serialized variables.
module MVar =
  //# Primitives

  /// Creates a job that writes the given value to the serialized variable.  It
  /// is an error to write to a `MVar` that is full.  See also: `*<<=`.
  val inline fill: MVar<'x> -> 'x -> Job<unit>

  /// Creates an alternative that becomes available when the variable contains a
  /// value and, if committed to, takes the value from the variable.
  val inline take: MVar<'x> -> Alt<'x>

  //# Read

  /// Creates an alternative that becomes available when the variable contains a
  /// value and, if committed to, read the value from the variable.
#if DOC
  ///
  /// Reference implementation:
  ///
  ///> let read xM = take xM ^=> fun x -> fill xM x >>-. x
#endif
  val inline read: MVar<'x> -> Alt<'x>

  //# Mutate

  /// Creates an alternative that takes the value of the serialized variable and
  /// then fills the variable with the result of performing the given job.
  val    inline mutateJob: ('x -> #Job<'x>) -> MVar<'x> -> Alt<unit>

  /// Creates an alternative that takes the value of the serialized variable and
  /// then fills the variable with the result of performing the given job.  If
  /// the job raises an exception, the serialized variable is filled with its
  /// original value before propagating the exception.
  val inline tryMutateJob: ('x -> #Job<'x>) -> MVar<'x> -> Alt<unit>

  /// Creates an alternative that takes the value of the serialized variable and
  /// then fills the variable with the result of performing the given function.
  val    inline mutateFun: ('x ->      'x)  -> MVar<'x> -> Alt<unit>

  /// Creates an alternative that takes the value of the serialized variable and
  /// then fills the variable with the result of performing the given function.
  /// If the function raises an exception, the serialized variable is filled
  /// with its original value before propagating the exception.
  val inline tryMutateFun: ('x ->      'x)  -> MVar<'x> -> Alt<unit>

  //# Modify

  /// Creates an alternative that takes the value of the serialized variable and
  /// then fills the variable with the result of performing the given job.
#if DOC
  ///
  /// Reference implementation:
  ///
  ///> let modifyJob (x2xyJ: 'x -> Job<'x * 'y>) (xM: MVar<'x>) =
  ///>   xM ^=> (x2xyJ >=> fun (x, y) -> fill xM x >>-. y)
#endif
  val    inline modifyJob: ('x -> #Job<'x * 'y>) -> MVar<'x> -> Alt<'y>

  /// Creates an alternative that takes the value of the serialized variable and
  /// then fills the variable with the result of performing the given job.  If
  /// the job raises an exception, the serialized variable is filled with its
  /// original value before propagating the exception.
  val inline tryModifyJob: ('x -> #Job<'x * 'y>) -> MVar<'x> -> Alt<'y>

  /// Creates an alternative that takes the value of the serialized variable and
  /// then fills the variable with the result of performing the given function.
#if DOC
  ///
  /// Reference implementation:
  ///
  ///> let modifyFun (x2xy: 'x -> 'x * 'y) (xM: MVar<'x>) =
  ///>   xM ^=> (x2xy >> fun (x, y) -> fill xM x >>-. y)
#endif
  val    inline modifyFun: ('x ->      'x * 'y)  -> MVar<'x> -> Alt<'y>

  /// Creates an alternative that takes the value of the serialized variable and
  /// then fills the variable with the result of performing the given function.
  /// If the function raises an exception, the serialized variable is filled
  /// with its original value before propagating the exception.
  val inline tryModifyFun: ('x ->      'x * 'y)  -> MVar<'x> -> Alt<'y>

////////////////////////////////////////////////////////////////////////////////

/// Represents a bounded synchronous mailbox for many to many communication.
#if DOC
///
/// Bounded synchronous mailboxes are a useful tool for coordinating work among
/// co-operating jobs.  They provide slack in the form of buffering between
/// producers and consumers allowing them to proceed in parallel.  They also
/// provide back-pressure in the form of blocking producers when consumers
/// cannot keep up.
///
/// In cases where buffering is not necessary, the basic channel primitive,
/// `Ch<_>`, should be preferred.  In cases where unbounded buffering is not a
/// problem, the basic mailbox primitive, `Mailbox<_>`, should be preferred.
///
/// At the time of writing, `BoundedMb<_>` is not implemented as a primitive,
/// but is implemented using other primitives of Hopac, and it is likely that
/// performance can be improved significantly.  If you run into a case where the
/// performance of `BoundedMb<_>` becomes problematic, please submit an issue.
#endif
type BoundedMb<'x> =
  /// Creates a new bounded mailbox.
  new: int -> BoundedMb<'x>

  [<Obsolete "This is an internal implementation detail.  Do not depend on.">]
  member Put: 'x -> Alt<unit>
  [<Obsolete "This is an internal implementation detail.  Do not depend on.">]
  member Take: Alt<'x>

/// Operations on bounded synchronous mailboxes.
module BoundedMb =
  /// Selective synchronous operation to put a message to a bounded mailbox.
  /// `put` operations are processed in FIFO order and become enabled as soon as
  /// there is room in the bounded buffer.  If the buffer capacity is `0`, `put`
  /// behaves exactly like `Ch.give`.
  val inline put: BoundedMb<'x> -> 'x -> Alt<unit>

  /// Selective synchronous operation to take a message from a bounded mailbox.
  /// `take` operations are processed in FIFO order and become enabled as soon
  /// as there are messages in the bounded buffer.  If the buffer capacity is
  /// `0`, `take` behaves exactly like `Ch.take`.
  val inline take: BoundedMb<'x> -> Alt<'x>

////////////////////////////////////////////////////////////////////////////////

#if DOC
/// Represents an asynchronous, unbounded buffered mailbox.
///
/// Compared to channels, mailboxes take more memory when empty, but offer space
/// efficient buffering of messages.  In situations where buffering must be
/// bounded, a bounded mailbox, `BoundedMb<_>`, should be preferred.  In
/// situations where buffering is not needed, a channel, `Ch<_>`, should be
/// preferred.
type Mailbox<'x> =
  /// `Mailbox<'x>` is a subtype of `Alt<'x>` and `xMb :> Alt<'x>` is equivalent
  /// to `Mailbox.take xMb`.
  inherit Alt<'x>

  /// Creates a new mailbox.
  new: unit -> Mailbox<'x>
#endif

/// Operations on buffered mailboxes.
module Mailbox =
  /// Creates a job that sends the given value to the specified mailbox.  This
  /// operation never blocks.  See also: `*<<+`.
  val inline send: Mailbox<'x> -> 'x -> Job<unit>

  /// Creates an alternative that becomes available when the mailbox contains at
  /// least one value and, if committed to, takes a value from the mailbox.
  val inline take: Mailbox<'x> -> Alt<'x>

  /// Immediate or non-workflow operations on buffered mailboxes.
  module Now =
    /// Sends the given value to the specified mailbox.  `Mailbox.Now.send xMb
    /// x` is equivalent to `Mailbox.send xMb x |> Hopac.start`.
    ///
    /// Note that using this function in a job workflow is not optimal and you
    /// should use `Mailbox.send` instead.
    val send: Mailbox<'x> -> 'x -> unit

////////////////////////////////////////////////////////////////////////////////

#if DOC
/// A non-recursive mutual exclusion lock for jobs.
///
/// In most cases you should use higher-level message passing primitives such as
/// `Ch`, `Mailbox`, `MVar` or `IVar`, but in some cases a simple lock might be
/// more natural to use.
///
/// Note that this lock is for synchronizing at the level of jobs.  A job may
/// even block while holding the lock.  For short non-blocking critical
/// sections, native locks (e.g. `Monitor` and `SpinLock`), concurrent data
/// structures or interlocked operations should be faster.  On the other hand,
/// suspending and resuming a job is several orders of magnitude faster than
/// suspending and resuming a native thread.
type Lock =
  /// Creates a new lock.
  new: unit -> Lock
#endif

/// Operations on mutual exclusion locks.
module Lock =
  /// Creates a job that runs the given job so that the lock is held during the
  /// execution of the given job.
  val inline duringJob: Hopac.Lock ->      Job<'x> -> Job<'x>

  /// Creates a job that calls the given function so that the lock is held
  /// during the execution of the function.
  val inline duringFun: Hopac.Lock -> (unit -> 'x) -> Job<'x>

////////////////////////////////////////////////////////////////////////////////

/// Extensions to various system modules and types for programming with jobs.
/// You can open this module to use the extensions much like as if they were
/// part of the existing modules and types.
module Extensions =
  /// Operations for processing arrays with jobs.
  module Array =
    /// Sequentially maps the given job constructor to the elements of the array
    /// and returns an array of the results.  `Array.mapJob x2yJ xs` is an
    /// optimized version of `Seq.mapJob x2yJ xs >>- fun ys -> ys.ToArray ()`.
    val mapJob: ('x -> #Job<'y>) -> array<'x> -> Job<array<'y>>

    /// Sequentially iterates the given job constructor over the given array.
    /// `Array.iterJob x2uJ xs` is an optimized version of `Seq.iterJob x2uJ
    /// xs`.
    val inline iterJob:       ('x -> #Job<unit>) -> array<'x> -> Job<unit>

    /// `Array.iterJobIgnore x2yJ xs` is equivalent to `Array.iterJob (x2yJ >>
    /// Job.Ignore) xs`.
    val iterJobIgnore: ('x -> #Job<_>)    -> array<'x> -> Job<unit>

  /// Operations for processing sequences with jobs.
  module Seq =
    /// Sequentially folds the job constructor over the given sequence and
    /// returns the result of the fold.
#if DOC
    ///
    /// Reference implementation:
    ///
    ///> let foldJob xy2xJ x (ys: seq<'y>) = Job.delay <| fun () ->
    ///>   Job.using <| ys.GetEnumerator () <| fun ys ->
    ///>   let rec loop x =
    ///>     if ys.MoveNext () then
    ///>       xy2xJ x ys.Current >>= loop
    ///>     else
    ///>       Job.result x
    ///>   loop x
#endif
    val foldJob:     ('x -> 'y -> #Job<'x>) -> 'x -> seq<'y> -> Job<'x>

    /// `foldFromJob x x2y2xJ ys` is equivalent to `foldJob x2y2xJ x ys` and is
    /// often syntactically more convenient.
    val inline foldFromJob: 'x -> ('x -> 'y -> #Job<'x>) -> seq<'y> -> Job<'x>

    /// Sequentially iterates the given job constructor over the given sequence.
    /// See also: `Seq.iterJobIgnore`, `Seq.Con.iterJob`, `seqIgnore`,
    /// `Array.mapJob`.
#if DOC
    ///
    /// Reference implementation:
    ///
    ///> let iterJob x2uJ (xs: seq<'x>) = Job.delay <| fun () ->
    ///>   Job.using <| xs.GetEnumerator () <| fun xs ->
    ///>   Job.whileDoDelay xs.MoveNext <| fun () ->
    ///>     x2uJ xs.Current
#endif
    val inline iterJob:       ('x -> #Job<unit>) -> seq<'x> -> Job<unit>

    /// `Seq.iterJobIgnore x2yJ xs` is equivalent to `Seq.iterJob (x2yJ >>
    /// Job.Ignore) xs`.
    val iterJobIgnore: ('x -> #Job<_>)    -> seq<'x> -> Job<unit>

    /// Sequentially maps the given job constructor to the elements of the
    /// sequence and returns a list of the results.  See also: `seqCollect`,
    /// `Seq.Con.mapJob`, `Array.mapJob`.
#if DOC
    ///
    /// Reference implementation:
    ///
    ///> let mapJob x2yJ (xs: seq<'x>) = Job.delay <| fun () ->
    ///>   let ys = ResizeArray<_>()
    ///>   Job.using <| xs.GetEnumerator () <| fun xs ->
    ///>   Job.whileDoDelay xs.MoveNext <| fun () ->
    ///>         x2yJ xs.Current >>- ys.Add
    ///>   >>-. ys
#endif
    val mapJob: ('x -> #Job<'y>) -> seq<'x> -> Job<ResizeArray<'y>>

    /// Operations for processing sequences using concurrent jobs.
    module Con =
      /// Iterates the given job constructor over the given sequence, runs the
      /// constructed jobs as separate concurrent jobs and waits until all of
      /// the jobs have finished.  See also: `Con.iterJobIgnore`, `conIgnore`.
      ///
      /// Note that this is not optimal for fine-grained parallel execution.
      val inline iterJob:       ('x -> #Job<unit>) -> seq<'x> -> Job<unit>

      /// `Con.iterJobIgnore x2yJ xs` is equivalent to `Con.iterJob (x2yJ >>
      /// Job.Ignore) xs`.
      ///
      /// Note that this is not optimal for fine-grained parallel execution.
      val iterJobIgnore: ('x -> #Job<_>)    -> seq<'x> -> Job<unit>

      /// Iterates the given job constructor over the given sequence, runs the
      /// constructed jobs as separate concurrent jobs and waits until all of
      /// the jobs have finished collecting the results into a list.  See also:
      /// `Seq.mapJob`, `conCollect`.
      ///
      /// Note that this is not optimal for fine-grained parallel execution.
      val mapJob: ('x -> #Job<'y>) -> seq<'x> -> Job<ResizeArray<'y>>

  /// Operations for interfacing F# async operations with jobs.
#if DOC
  ///
  /// Note that these operations are provided for interfacing with existing APIs
  /// that work with async operations.  Running async operations within jobs and
  /// vice versa incurs potentially significant overheads.
  ///
  /// Note that there is almost a one-to-one mapping between async operations
  /// and jobs.  The main semantic difference between async operations and Hopac
  /// jobs is the threads and schedulers they are being executed on.
#endif
  module Async =
    /// Creates a job that starts the given async operation and then waits until
    /// the operation finishes.  See also: `toJobOn`, `toAlt`, `toAltOn`.
#if DOC
    ///
    /// The async operation will be started on a Hopac worker thread, which
    /// means that the async operation will continue on the thread pool.
    /// Consider whether you need to call `Async.SwitchToContext` or some other
    /// thread or synchronization context switching async operation in your
    /// async operation.
#endif
    [<Obsolete "`Async.toJob` will be removed as interop primitives are being revised. Use `Job.fromAsync` and switch synchronization context explicitly if necessary.">]
    val toJob: Async<'x> -> Job<'x>

    /// Creates a job that posts the given async operation to the specified
    /// synchronization context for execution and then waits until the operation
    /// finishes.  As a special case, `toJobOn null xA` is equivalent to `toJob
    /// xA`.  See also: `toAlt`, `toAltOn`.
    [<Obsolete "`Async.toJobOn` will be removed as interop primitives are being revised. Use `Job.fromAsync` and switch synchronization context explicitly if necessary.">]
    val toJobOn: SynchronizationContext -> Async<'x> -> Job<'x>

    /// Creates an alternative that, when instantiated, starts the given async
    /// operation and then becomes enabled once the operation finishes.
    /// Furthermore, in case the alternative is not committed to, the async
    /// operation is cancelled.  See also: `toJob`, `toJobOn`, `toAltOn`.
#if DOC
    ///
    /// The async operation will be started on a Hopac worker thread, which
    /// means that the async operation will continue on the thread pool.
    /// Consider whether you need to call `Async.SwitchToContext` or some other
    /// thread or synchronization context switching async operation in your
    /// async operation.
#endif
    [<Obsolete "`Async.toAlt` will be removed as interop primitives are being revised. Use `Alt.fromAsync` and switch synchronization context explicitly if necessary.">]
    val toAlt: Async<'x> -> Alt<'x>

    /// Creates an alternative that, when instantiated, posts the given async
    /// operation to the specified synchronization context for execution and
    /// then becomes enabled once the operation finishes.  Furthermore, in case
    /// the alternative is not committed to, the async operation is cancelled.
    /// As a special case, `toAltOn null xA` is equivalent to `toAlt xA`.  See
    /// also: `toJob`, `toJobOn`.
    [<Obsolete "`Async.toAltOn` will be removed as interop primitives are being revised. Use `Alt.fromAsync` and switch synchronization context explicitly if necessary.">]
    val toAltOn: SynchronizationContext -> Async<'x> -> Alt<'x>

    /// Creates an async operation that starts the given job on the specified
    /// scheduler and then waits until the started job finishes.  See also:
    /// `Job.Scheduler`, `Async.Global.ofJob`.
    [<Obsolete "`Async.ofJobOn` will be removed as interop primitives and scheduling are being revised. Use `Job.toAsync`.">]
    val ofJobOn: Scheduler -> Job<'x> -> Async<'x>

    /// Builder for async workflows.  The methods in this builder delegate to
    /// the default `async` builder.
    type [<AbstractClass>] OnWithSchedulerBuilder =
      ///
      new: unit -> OnWithSchedulerBuilder
      ///
      abstract Scheduler: Scheduler
      ///
      abstract Context: SynchronizationContext
      ///
      member inline Bind:  Task<'x> * ('x -> Async<'y>) -> Async<'y>
      ///
      member inline Bind:   Job<'x> * ('x -> Async<'y>) -> Async<'y>
      ///
      member inline Bind: Async<'x> * ('x -> Async<'y>) -> Async<'y>
      ///
      member inline Combine: Async<unit> * Async<'x> -> Async<'x>
      ///
      member inline Delay: (unit -> Async<'x>) -> Async<'x>
      ///
      member inline For: seq<'x> * ('x -> Async<unit>) -> Async<unit>
      ///
      member inline Return: 'x -> Async<'x>
      ///
      member inline ReturnFrom:  Task<'x> -> Async<'x>
      ///
      member inline ReturnFrom:   Job<'x> -> Async<'x>
      ///
      member inline ReturnFrom: Async<'x> -> Async<'x>
      ///
      member inline TryFinally: Async<'x> * (unit -> unit) -> Async<'x>
      ///
      member inline TryWith: Async<'x> * (exn -> Async<'x>) -> Async<'x>
      ///
      member inline Using: 'x * ('x -> Async<'y>) -> Async<'y> when 'x :> IDisposable
      ///
      member inline While: (unit -> bool) * Async<unit> -> Async<unit>
      ///
      member inline Zero: unit -> Async<unit>
      ///
      member inline Run: Async<'x> -> Job<'x>

    /// Operations on the global scheduler.
    module Global =
      /// Creates an async operation that starts the given job on the global
      /// scheduler and then waits until the started job finishes.  See also:
      /// `Async.ofJobOn`.
      [<Obsolete "`Async.Global.ofJob` will be removed as interop primitives and scheduling are being revised. Use `Job.toAsync`.">]
      val ofJob: Job<'x> -> Async<'x>

      /// Creates a builder for running an async workflow on the main
      /// synchronization context and interoperating with the Hopac global
      /// scheduler.  The application must call `Hopac.Extensions.Async.setMain`
      /// to configure Hopac with the main synchronization context.
      val onMain: unit -> OnWithSchedulerBuilder

    /// Sets the main synchronization context.  This must be called by
    /// application code in order to use operations such as `onceAltOnMain` and
    /// `Hopac.onMain`
    val setMain: SynchronizationContext -> unit

    /// Gets the main synchronization context.  The main synchronization context
    /// must be set by application code using `setMain` before calling this
    /// function.
    val getMain: unit -> SynchronizationContext

  /// Builder for an async operation started on the given synchronization
  /// context with jobs on the specified scheduler wrapped as a job.
  val inline asyncOn: SynchronizationContext -> Scheduler -> Async.OnWithSchedulerBuilder

  /// Operations for interfacing tasks with jobs.
#if DOC
  ///
  /// Note that these operations are provided for interfacing with existing
  /// APIs that work with tasks.  Starting a job as a task and then awaiting
  /// for its result has much higher overhead than simply starting the job as a
  /// `Promise`, for example.
  ///
  /// Note that starting tasks correctly can be tricky.  Hopac jobs are designed
  /// to be executed by Hopac worker threads, which have the default `null`
  /// synchronization context like the .Net thread pool, but Hopac jobs can also
  /// be started on other threads, which may live in non-default synchronization
  /// contexts.  Tasks that have been written using the C# async-await mechanism
  /// may capture the current synchronization context.  This means that when you
  /// call a function to start a task within a Hopac job, you may need to
  /// explicitly post that function call to a specific synchronization context.
  ///
  /// Note that tasks and jobs are quite different in nature as tasks are
  /// comonadic while jobs are monadic.
#endif
  type Task with
    /// Creates a job that starts the given job as a separate concurrent job,
    /// whose result can be obtained from the returned task.
    static member startJob: Job<'x> -> Job<Task<'x>>

//  /// Operations for interfacing the system `ThreadPool` with jobs.
//  type ThreadPool with
//    /// Creates a job that queues the given thunk to execute on the system
//    /// `ThreadPool` and then waits for the result of the thunk.
//    static member queueAsJob: (unit -> 'x) -> Job<'x>

//  /// Operations for interfacing with `WaitHandle`s.
//  type WaitHandle with
//    /// Creates a job that awaits for the given wait handle with the specified
//    /// timeout using the `RegisterWaitForSingleObject` API of the system
//    /// `ThreadPool`.
//    member awaitAsJob: TimeSpan -> Job<bool>

//    /// Creates a job that awaits for the given wait handle using the
//    /// `RegisterWaitForSingleObject` API of the system `ThreadPool`.
//    member awaitAsJob: Job<unit>

  /// Raised by `onceAltOn` when the associated observable signals the
  /// `OnCompleted` event.
  exception OnCompleted

  /// Operations for interfacing Hopac with observables.
  type IObservable<'x> with
    /// Creates an alternative that, when instantiated, subscribes to the
    /// observable on the specified synchronization context for at most one
    /// event.  Passing `null` as the synchronization context means that the
    /// subscribe and unsubscribe actions are performed on an unspecified
    /// thread.
#if DOC
    ///
    /// After an `OnNext` event, the alternative returns the value given by the
    /// observable.  After an `OnError` event, the alternative raises the
    /// exception given by the observable.  After an `OnCompleted` event, the
    /// alternative raises the `OnCompleted` exception.
    ///
    /// The alternative becomes available as soon as the observable signals any
    /// event after which the alternative unsubscribes from the observable.  If
    /// some other alternative is committed to before the observable signals any
    /// event, the alternative unsubscribes from the observable.  Note, however,
    /// that if the current job explicitly aborts while instantiating some other
    /// alternative involved in the same synchronous operation, there is no
    /// guarantee that the observable would be unsubscribed from.
    ///
    /// Note that, as usual, the alternative can be used many times and even
    /// concurrently.
#endif
    member onceAltOn: SynchronizationContext -> Alt<'x>

    /// This is equivalent to calling `onceAltOn` with the main synchronization
    /// context.  The application must call `Hopac.Extensions.Async.setMain` to
    /// configure Hopac with the main synchronization context.
    member onceAltOnMain: Alt<'x>

    /// `xO.onceAlt` is equivalent to `xO.oneAltOn null`.  Note that it is often
    /// necessary to specify the synchronization context to subscribe on.  See
    /// also: `Observable.SubscribeOn`.
    member onceAlt: Alt<'x>

////////////////////////////////////////////////////////////////////////////////

/// Infix operators for concise expression of key Hopac idioms.  You can open
/// this module to bring all of the infix operators into scope.
#if DOC
///
/// The operator symbols have been designed to allow the most common expressions
/// to be written without parentheses:
///
/// - Message passing operators start with `*` and have the highest precedence.
///
/// - After action operators start with `^` and have the second highest
/// precedence.
///
/// - Choice and pairing operators start with `<` and have the lowest
/// precedence.
///
/// - Sequencing operators start with `>` and also have the lowest precendence.
///
/// Fortunately, sequencing usually follows after choice or pairing, or is
/// inside a function expression, so parentheses are not required in that case.
///
/// As an example of operator usage, the `swap` operation of a swap channel
/// combines message passing, after actions, sequencing and choice:
///
///> let swap swapCh outMsg =
///>       swapCh ^=> fun (inMsg, outIv) -> outIv *<= outMsg >>-. inMsg
///>   <|> swapCh *<-=>- fun inIv -> (outMsg, inIv)
///>    :> Alt<_>
///
/// The type ascription above is unnecessary and is there only as an example of
/// how it can be placed.
#endif
module Infixes =

  //# Query-Reply

  /// Creates an alternative that, using the given job constructor, constructs a
  /// query with a reply channel and a nack, sends it to the query channel and
  /// commits on taking the reply from the reply channel.  See also: `*<+->-`.
#if DOC
  ///
  /// Reference implementation:
  ///
  ///> let ( *<+->= ) qCh rCh2n2qJ = Alt.withNackJob <| fun nack ->
  ///>   let rCh = Ch<_> ()
  ///>   rCh2n2qJ rCh nack >>= fun q ->
  ///>   qCh *<+ q >>-.
  ///>   rCh
#endif
  val inline ( *<+->= ): Ch<'q> ->   (Ch<'r> -> Promise<unit> -> #Job<'q>) -> Alt<'r>

  /// Creates an alternative that, using the given function, constructs a query
  /// with a reply channel and a nack, sends it to the query channel and commits
  /// on taking the reply from the reply channel.  `*<+->-` captures the most
  /// common use case of `Alt.withNackJob` and is a slightly less expressive
  /// form of `*<+->=`.  See also: `*<-=>-`.
#if DOC
  ///
  /// Here is the `incrementBy` function from the example in `Alt.withNackJob`
  /// expressed using `*<+->-`:
  ///
  ///> let incrementBy n =
  ///>   counterServer *<+->- fun replyCh nack -> (n, nack, replyCh)
  ///
  /// Reference implementation:
  ///
  ///> let ( *<+->- ) qCh rCh2n2q =
  ///>   qCh *<+->= fun rCh n -> rCh2n2q rCh n |> result
#endif
  val inline ( *<+->- ): Ch<'q> ->   (Ch<'r> -> Promise<unit> ->      'q)  -> Alt<'r>

  /// Creates an alternative that, using the given job constructor, constructs a
  /// query with a reply variable, commits on giving the query and reads the
  /// reply variable.  See also: `*<-=>-`.
#if DOC
  ///
  /// Reference implementation:
  ///
  ///> let ( *<-=>= ) qCh rI2qJ = Alt.prepareJob <| fun () ->
  ///>   let rI = IVar<_> ()
  ///>   rI2qJ rI >>- fun q ->
  ///>   qCh *<- q ^=>.
  ///>   rI
#endif
  val inline ( *<-=>= ): Ch<'q> -> (IVar<'r>                  -> #Job<'q>) -> Alt<'r>

  /// Creates an alternative that, using the given function, constructs a query
  /// with a reply variable, commits on giving the query and reads the reply
  /// variable.  `*<-=>-` captures the most common use case of `Alt.prepareFun`
  /// and is a slighly less expressive form of `*<-=>=`.  See also: `*<+->-`.
#if DOC
  ///
  /// Reference implementation:
  ///
  ///> let ( *<-=>- ) qCh rI2q = qCh *<-=>= (rI2q >> result)
#endif
  val inline ( *<-=>- ): Ch<'q> -> (IVar<'r>                  ->      'q)  -> Alt<'r>

  /// Creates an alternative that, using the given job constructor, constructs a
  /// query with a reply variable, sends the query and reads the reply.  In
  /// order for the alternative to make sense, the operation must not require
  /// exclusive choice.  If this not the case, then the resulting value should
  /// only be used as a job.
#if DOC
  ///
  /// Reference implementation:
  ///
  ///> let ( *<+=>= ) qCh rI2qJ = Alt.prepareJob <| fun () ->
  ///>   let rI = IVar ()
  ///>   rI2qJ rI >>= fun q ->
  ///>   qCh *<+ q >>-.
  ///>   rI
#endif
  val inline ( *<+=>= ): Ch<'q> -> (IVar<'r>                  -> #Job<'q>) -> Alt<'r>

  /// Creates an alternative that, using the given function, constructs a query
  /// with a reply variable, sends the query and reads the reply.  In order for
  /// the alternative to make sense, the operation must not require exclusive
  /// choice.  If this is not the case, then the resulting value should only be
  /// used as a job.
#if DOC
  ///
  /// Reference implementation:
  ///
  ///> let ( *<+=>- ) qCh rI2q = qCh *<+=>= (rI2q >> result)
#endif
  val inline ( *<+=>- ): Ch<'q> -> (IVar<'r>                  ->      'q)  -> Alt<'r>

  //# Message passing

  /// Creates an alternative that, at instantiation time, offers to give the
  /// given value on the given channel, and becomes available when another job
  /// offers to take the value.  `xCh *<- x` is equivalent to `Ch.give xCh x`.
  val inline ( *<-  ):      Ch<'x> -> 'x  -> Alt<unit>

  /// Creates a job that sends a value to another job on the given channel.  A
  /// send operation is asynchronous.  In other words, a send operation does not
  /// wait for another job to give the value to.  `xCh *<+ x` is equivalent to
  /// `Ch.send xCh x`.
  ///
  /// Note that channels have been optimized for synchronous operations; an
  /// occasional send can be efficient, but when sends are queued, performance
  /// maybe be significantly worse than with a `Mailbox` optimized for
  /// buffering.
  val inline ( *<+  ):      Ch<'x> -> 'x  -> Job<unit>

  /// Creates a job that writes to the given write once variable.  It is an
  /// error to write to a single `IVar` more than once.  `xI *<= x` is
  /// equivalent to `IVar.fill xI x`.
  val inline ( *<=  ):    IVar<'x> -> 'x  -> Job<unit>

  /// Creates a job that writes the given exception to the given write once
  /// variable.  It is an error to write to a single `IVar` more than once.  `xI
  /// *<=! e` is equivalent to `IVar.fillFailure xI e`.
  val inline ( *<=! ):    IVar<'x> -> exn -> Job<unit>

  /// Creates a job that writes the given value to the serialized variable.  It
  /// is an error to write to a `MVar` that is full.  `xM *<<= x` is equivalent
  /// to `MVar.fill xM x`.
  val inline ( *<<= ):    MVar<'x> -> 'x  -> Job<unit>

  /// Creates a job that sends the given value to the specified mailbox.  This
  /// operation never blocks.  `xMb *<<+ x` is equivalent to `Mailbox.send xMb
  /// x`.
  val inline ( *<<+ ): Mailbox<'x> -> 'x  -> Job<unit>

  //# After actions

  /// Creates an alternative whose result is passed to the given job constructor
  /// and processed with the resulting job after the given alternative has been
  /// committed to.  This is the same as `afterJob` with the arguments flipped.
  val inline ( ^=>  ): Alt<'x> -> ('x -> #Job<'y>) -> Alt<'y>

  /// Creates an alternative which is committed to when the given alternative
  /// is committed to. Once committed, the given alternative's result is mapped
  /// using the given function, providing the final result.
  ///
  /// `xA ^-> x2y` is equivalent to `xA ^=> (x2y >> result)`.  This is the same
  /// as `afterFun` with the arguments flipped.
  val inline ( ^->  ): Alt<'x> -> ('x ->      'y)  -> Alt<'y>

  /// Creates an alternative which is committed to when the given alternative
  /// is committed to. Once committed, the job argument is executed and
  /// generates the result.
  ///
  /// `xA ^=>. yJ` is equivalent to `xA ^=> always yJ`.
  val ( ^=>. ): Alt<_>  ->         Job<'y>  -> Alt<'y>

  /// Creates an alternative which is committed to when the given alternative
  /// is committed to. Once committed, the given value is used as the result.
  ///
  /// `xA ^->. y` is equivalent to `xA ^-> always y`.
  val ( ^->. ): Alt<_>  ->             'y   -> Alt<'y>

  /// Creates an alternative which is committed to when the alternative
  /// argument is committed to. Once committed, the given exception is raised.
  ///
  /// `xA ^->! e` is equivalent to `xA ^-> fun _ -> raise e`.
  val ( ^->! ): Alt<_>  ->             exn  -> Alt<_>

  //# Choices

  /// Creates an alternative that is available when either of the given
  /// alternatives is available.  `xA1 <|> xA2` is an optimized version of
  /// `choose [xA1; xA2]`.  See also: `choosy`.
#if DOC
  ///
  /// The given alternatives are processed in a left-to-right order with
  /// short-cut evaluation.  In other words, given an alternative of the form
  /// `first <|> second`, the `first` alternative is first instantiated and, if
  /// it is available, is committed to and the `second` alternative will not be
  /// instantiated at all.
#endif
  val ( <|>  ): Alt<'x> -> Alt<'x> ->     Alt<'x>

  /// A memoizing version of `<|>`.
  val inline ( <|>* ): Alt<'x> -> Alt<'x> -> Promise<'x>

  /// `xA1 <~> xA2` is like `xA1 <|> xA2` except that the order in which `xA1`
  /// and `xA2` are instantiated is determined at random every time the
  /// alternative is used.  See also: `chooser`.
#if DOC
  ///
  /// Note that randomization only applies to the instantiation order.  It makes
  /// no difference after instantiation.
  ///
  /// WARNING: Chained uses of `<~>` do not lead to uniform distributions.
  /// Consider the expression `xA1 <~> xA2 <~> xA3`.  It parenhesizes as `(xA1
  /// <~> xA2) <~> xA3`.  This means that `xA3` has a 50% and both `xA1` and
  /// `xA2` have 25% probability of being considered first.
#endif
  val ( <~>  ): Alt<'x> -> Alt<'x> ->     Alt<'x>

  /// A memoizing version of `<~>`.
  val inline ( <~>* ): Alt<'x> -> Alt<'x> -> Promise<'x>

  //# Sequencing

  /// Creates a job that first runs the given job and then passes the result of
  /// that job to the given function to build another job which will then be
  /// run.  This is the same as `bind` with the arguments flipped.
  val inline ( >>=   ): Job<'x> -> ('x -> #Job<'y>) ->     Job<'y>

  /// A memoizing version of `>>=`.
  val inline ( >>=*  ): Job<'x> -> ('x -> #Job<'y>) -> Promise<'y>

  /// Creates a job that runs the given job and maps the result of the job with
  /// the given function.  `xJ >>- x2y` is an optimized version of `xJ >>= (x2y
  /// >> result)`.  This is the same as `map` with the arguments flipped.
  val inline ( >>-   ): Job<'x> -> ('x ->      'y)  ->     Job<'y>

  /// A memoizing version of `>>-`.
  val inline ( >>-*  ): Job<'x> -> ('x ->      'y)  -> Promise<'y>

  /// Creates a job that runs the given two jobs and returns the result of the
  /// second job.  `xJ >>=. yJ` is equivalent to `xJ >>= always yJ`.
  val ( >>=.  ): Job<_>  ->         Job<'y>  ->     Job<'y>

  /// A memoizing version of `>>=.`.
  val inline ( >>=*. ): Job<_>  ->         Job<'y>  -> Promise<'y>

  /// Creates a job that runs the given job and then returns the given value.
  /// `xJ >>-. y` is an optimized version of `xJ >>= always (result y)`.
  val ( >>-.  ): Job<_>  ->             'y   ->     Job<'y>

  /// A memoizing version of `>>-.`.
  val inline ( >>-*. ): Job<_>  ->             'y   -> Promise<'y>

  /// Creates a job that runs the given job and then raises the given exception.
  /// `xJ >>-! e` is equivalent to `xJ >>= fun _ -> raise e`.
  val ( >>-!  ): Job<_>  ->             exn  ->     Job<_>

  /// A memoizing version of `>>-!`.
  val inline ( >>-*! ): Job<_>  ->             exn  -> Promise<_>

  //# Composition

  /// Creates a job that is the composition of the given two job constructors.
  /// `(x2yJ >=> y2zJ) x` is equivalent to `x2yJ x >>= y2zJ` and is much like
  /// the `>>` operator on ordinary functions.
  val inline ( >=>   ): ('x -> #Job<'y>) -> ('y -> #Job<'z>) -> 'x ->     Job<'z>

  /// A memoizing version of `>=>`.
  val inline ( >=>*  ): ('x -> #Job<'y>) -> ('y -> #Job<'z>) -> 'x -> Promise<'z>

  /// Creates a job that is the composition of the given job constructor and
  /// function.  `(x2yJ >-> y2z) x` is equivalent to `x2yJ x >>- y2z` and is
  /// much like the `>>` operator on ordinary functions.
  val inline ( >->   ): ('x -> #Job<'y>) -> ('y ->      'z)  -> 'x ->     Job<'z>

  /// A memoizing version of `>->`.
  val inline ( >->*  ): ('x -> #Job<'y>) -> ('y ->      'z)  -> 'x -> Promise<'z>

  /// `(x2yJ >=>. zJ) x` is equivalent to `x2yJ x >>=. zJ`.
  val inline ( >=>.  ): ('x -> #Job<_>)  ->         Job<'z>  -> 'x ->     Job<'z>
  /// A memoizing version of `>=>.`.
  val inline ( >=>*. ): ('x -> #Job<_>)  ->         Job<'z>  -> 'x -> Promise<'z>
  /// `(x2yJ >->. z) x` is equivalent to `x2yJ x >>-. z`.
  val inline ( >->.  ): ('x -> #Job<_>)  ->             'z   -> 'x ->     Job<'z>
  /// A memoizing version of `>->.`.
  val inline ( >->*. ): ('x -> #Job<_>)  ->             'z   -> 'x -> Promise<'z>
  /// `(x2yJ >->! e) x` is equivalent to `x2yJ x >>-! e`.
  val inline ( >->!  ): ('x -> #Job<_>)  ->             exn  -> 'x ->     Job<_>
  /// A memoizing version of `>->!`.
  val inline ( >->*! ): ('x -> #Job<_>)  ->             exn  -> 'x -> Promise<_>

  //# Pairing

  /// Creates a job that runs the given two jobs and then returns a pair of
  /// their results.  `xJ <&> yJ` is equivalent to `xJ >>= fun x -> yJ >>= fun y
  /// -> result (x, y)`.
  val ( <&> ): Job<'x> -> Job<'y> -> Job<'x * 'y>

  /// Creates a job that either runs the given jobs sequentially, like `<&>`, or
  /// as two separate parallel jobs and returns a pair of their results.
  ///
  /// Note that when the jobs are run in parallel and both of them raise an
  /// exception then the created job raises an `AggregateException`.
#if DOC
  ///
  /// Note that, because it is not guaranteed that the jobs would always be run
  /// as separate parallel jobs, a job such as
  ///
  ///> let mayDeadlock = delay <| fun () ->
  ///>   let c = Ch ()
  ///>   Ch.give c () <*> Ch.take c
  ///
  /// may deadlock.  If two jobs need to communicate with each other they need
  /// to be started as two separate jobs.
#endif
  val ( <*> ): Job<'x> -> Job<'y> -> Job<'x * 'y>

  /// An alternative that is equivalent to first committing to either one of the
  /// given alternatives and then committing to the other alternative.  Note
  /// that this is not the same as committing to both of the alternatives in a
  /// single transaction.  Such an operation would require a more complex
  /// synchronization protocol like with the so called Transactional Events.
  val ( <+> ): Alt<'x> -> Alt<'y> -> Alt<'x * 'y>

////////////////////////////////////////////////////////////////////////////////

#if DOC
/// Represents a handle to a (started, running or terminated) job.
///
/// A handle makes it possible to determine when a job is known to have been
/// terminated.  An example use for handles would be a system where critical
/// resources are managed by a server job and those critical resources need to
/// be released even in case a client job suffers from a fault and is terminated
/// before properly releasing resources.
///
/// For performance reasons, Hopac creates handles lazily for simple jobs,
/// because for many uses of lightweight threads such a capability is simply not
/// necessary.  However, when handles are known to be needed, it is better to
/// allocate them eagerly by directly starting jobs using `Proc.start` or
/// `Proc.queue`.
type Proc =
  /// `Proc` is a subtype of `Alt<unit>` and `p :> Alt<unit>` is equivalent to
  /// `Proc.join p`.
  inherit Alt<unit>
#endif

/// Operations on handles.
module Proc =
  //# Spawning jobs with handles

  /// Creates a job that queues a new job with a handle.  See also: `start`,
  /// `Job.queue`.
  val inline queue:       Job<unit> -> Job<Proc>

  /// Creates a job that queues a new job with a handle.  `queueIgnore xJ` is
  /// equivalent to `Job.Ignore xJ |> queue`.
  val queueIgnore: Job<_>    -> Job<Proc>

  /// Creates a job that starts a new job with a handle.  See also: `queue`,
  /// `Job.start`.
  val inline start:       Job<unit> -> Job<Proc>

  /// Creates a job that starts a new job with a handle.  `startIgnore xJ` is
  /// equivalent to `Job.Ignore xJ |> start`.
  val startIgnore: Job<_>    -> Job<Proc>

  //# Access to handle

  /// Creates a job that calls the given job contructor with the handle
  /// of the current job.
#if DOC
  ///
  /// Note that this is an `O(n)` operation where `n` is the number of
  /// continuation or stack frames of the current job.  In most cases this
  /// should not be an issue, but if you need to repeatedly access the proc
  /// handle of the current job it may be advantageous to cache it in a local
  /// variable.
#endif
  val inline bind: (Proc -> #Job<'x>) -> Job<'x>

  /// `map p2x` is equivalent to `bind (p2x >> result)`.
  val  inline map: (Proc ->      'x)  -> Job<'x>

  /// Returns a job that returns the handle of the current job.  `self ()` is
  /// equivalent to `bind result`.
  val inline self: unit -> Job<Proc>

  //# Joining

  /// Returns an alternative that becomes available once the job corresponding
  /// to the handle is known to have been terminated for any reason.
  val inline join: Proc -> Alt<unit>

////////////////////////////////////////////////////////////////////////////////

/// Expression builder type for jobs.
#if DOC
///
/// The following expression constructs are supported:
///
///> ... ; ...
///> do ...
///> do! ... | async | task | obs
///> for ... = ... to ... do ...
///> for ... in ... do ...
///> if ... then ...
///> if ... then ... else ...
///> let ... = ... in ...
///> let! ... = ... | async | task | obs in ...
///> match ... with ...
///> return ...
///> return! ... | async | task
///> try ... finally ...
///> try ... with ...
///> use ... = ... in ...
///> use! ... = ... in ...
///> while ... do ...
///
/// In the above, an ellipsis denotes either a job, an ordinary expression or a
/// pattern.  A job workflow can also directly bind and return from async
/// operations, which will be started on a Hopac worker thread (see
/// `Job.fromAsync`), tasks (see `Job.awaitTask`) and observables (see
/// `IObservable<'x>.onceAlt`).
///
/// Note that the `Job` module provides more combinators for constructing jobs.
/// For example, the F# workflow notation does not support `Job.tryFinallyJob`
/// and `Job.tryIn` is easier to use correctly than `try ... with ...`
/// expressions.  Operators such as `>>-` and `>>-.` and operations such as
/// `Job.iterate` and `Job.forever` are frequently useful and may improve
/// performance.
#endif
type JobBuilder =
  ///
  new: unit -> JobBuilder
  ///
  member inline Bind: IObservable<'x> * ('x -> Job<'y>) -> Job<'y>
  ///
  member inline Bind:       Async<'x> * ('x -> Job<'y>) -> Job<'y>
  ///
  member inline Bind:        Task<'x> * ('x -> Job<'y>) -> Job<'y>
  ///
  member inline Bind:         Job<'x> * ('x -> Job<'y>) -> Job<'y>
  ///
  member inline Combine: Job<unit> * (unit -> Job<'x>) -> Job<'x>
  ///
  member inline Delay: (unit -> Job<'x>) -> (unit -> Job<'x>)
  ///
  member inline For: seq<'x> * ('x -> Job<unit>) -> Job<unit>
  ///
  member inline Return: 'x -> Job<'x>
  ///
  member inline ReturnFrom: IObservable<'x> -> Job<'x>
  ///
  member inline ReturnFrom:       Async<'x> -> Job<'x>
  ///
  member inline ReturnFrom:        Task<'x> -> Job<'x>
  ///
  member inline ReturnFrom:         Job<'x> -> Job<'x>
  ///
  member inline Run: (unit -> Job<'x>) -> Job<'x>
  ///
  member inline TryFinally: (unit -> Job<'x>) * (unit -> unit) -> Job<'x>
  ///
  member inline TryWith: (unit -> Job<'x>) * (exn -> Job<'x>) -> Job<'x>
  ///
  member inline Using: 'x * ('x -> Job<'y>) -> Job<'y> when 'x :> IDisposable
  ///
  member inline While: (unit -> bool) * (unit -> Job<unit>) -> Job<unit>
  ///
  member inline Zero: unit -> Job<unit>

////////////////////////////////////////////////////////////////////////////////

/// Represents a job to be embedded within a computation built upon jobs.
#if DOC
///
/// Embedded jobs can be useful when defining computations built upon jobs.
/// Having to encode lightweight threads using the job monad is somewhat
/// unfortunate, because it is such a fundamental abstraction.  One sometimes,
/// perhaps even often, wants to define more interesting computations upon jobs,
/// but the traditional way of doing that requires adding yet another costly
/// layer of abstraction on top of jobs.  Another possibility is to expose the
/// `Job<'x>` type constructor as shown in the following example:
///
///> type Monad<'x>
///
///> type MonadBuilder =
///>   member Delay: unit -> Job<Monad<'x>>
///>   member Return: 'x -> Job<Monad<'x>>
///>   member Bind:   Job<Monad<'x>> * ('x -> Job<Monad<'y>>) -> Job<Monad<'y>>
///>   member Bind: EmbeddedJob<'x>  * ('x -> Job<Monad<'y>>) -> Job<Monad<'y>>
///
/// The `Monad<'x>` type constructor and the `MonadBuilder` defines the new
/// computation mechanism on top of jobs.  The `Bind` operation taking an
/// `EmbeddedJob<'x>` allows one to conveniently embed arbitrary jobs within the
/// computations without introducing nasty overload resolution problems.
///
/// Consider what would happen if one would instead define `MonadBuilder` as
/// follows:
///
///> type MonadBuilder =
///>   member Delay: unit -> Job<Monad<'x>>
///>   member Return: 'x -> Job<Monad<'x>>
///>   member Bind: Job<Monad<'x>> * ('x -> Job<Monad<'y>>) -> Job<Monad<'y>>
///>   member Bind: Job<      'x > * ('x -> Job<Monad<'y>>) -> Job<Monad<'y>>
///
/// A `Bind` operation is now almost always ambiguous and one would have to
/// annotate bind expressions to resolve the ambiguity.
///
/// The types of the operations in the `MonadBuilder` may, at first glance, seem
/// complicated.  Essentially the covariant positions in the signature are
/// wrapped with the `Job<_>` type constructor to make it possible to use
/// lightweight threads.  In a language with built-in lightweight threads this
/// would be unnecessary.  Reading the signature by mentally replacing every
/// `Job<'x>` with just `'x`, the signature should become clear.
#endif
type EmbeddedJob<'x> = struct
    val Job: Job<'x>
    new: Job<'x> -> EmbeddedJob<'x>
  end

/// A builder for embedded jobs.
type EmbeddedJobBuilder =
  inherit JobBuilder
  new: unit -> EmbeddedJobBuilder
  member Run: (unit -> Job<'x>) -> EmbeddedJob<'x>

////////////////////////////////////////////////////////////////////////////////

#if DOC
/// Represents a scheduler that manages a number of worker threads.
type Scheduler
#endif

/// Operations on schedulers.  Use of this module requires more intimate
/// knowledge of Hopac, but may allow adapting Hopac to special application
/// requirements.
module Scheduler =

  /// A record of scheduler configuration options.
  type Create =
    {
      /// Specifies whether worker threads are run as background threads or as
      /// foreground threads.  The default is to run workers as background
      /// threads.  If you want to run worker threads as foreground threads,
      /// then you will have to explicitly kill the worker threads.  Using
      /// foreground threads is probably preferable if your application
      /// dynamically creates and kills local schedulers to make sure the
      /// worker threads are properly killed.
      Foreground: option<bool>

      /// Specifies the idle handler for workers.  The worker idle handler is
      /// run whenever an individual worker runs out of work.  The idle handler
      /// must return an integer value that specifies how many milliseconds the
      /// worker is allowed to sleep.  `Timeout.Infinite` puts the worker into
      /// sleep until the scheduler explicitly wakes it up.  `0` means that the
      /// idle handler found some new work and the worker should immediately
      /// look for it.
      IdleHandler: option<Job<int>>

      /// Specifies the maximum stack size for worker threads.  The default
      /// is to use the default maximum stack size of the `Thread` class.
      MaxStackSize: option<int>

      /// Number of worker threads.  Using more than
      /// `Environment.ProcessorCount` is not optimal and may, in some cases,
      /// significantly reduce performance.  The default is
      /// `Environment.ProcessorCount`.
      NumWorkers: option<int>

//      /// Specifies the priority of the worker threads.  The default is to use
//      /// `Normal` priority.
//      Priority: option<Threading.ThreadPriority>

      /// Specifies the top level exception handler job constructor of the
      /// scheduler.  When a job fails with an otherwise unhandled exception,
      /// the job is killed and a new job is constructed with the top level
      /// handler constructor and then started.  To avoid infinite loops, in
      /// case the top level handler job raises exceptions, it is simply killed
      /// after printing a message to the console.  The default top level
      /// handler simply prints out a message to the console.
      TopLevelHandler: option<exn -> Job<unit>>
    }

    /// Default options.
    static member Def: Create

  /// Operations on the global scheduler.
  module Global =
    /// Sets options for creating the global scheduler.  This must be called
    /// before invoking any Hopac functionality that implicitly creates the
    /// global scheduler.
    val setCreate: Create -> unit

  /// Creates a new local scheduler.
  ///
  /// Note that a local scheduler does not automatically implement services such
  /// as the global wall-clock timer.
  val create: Create -> Scheduler

  /// Queues the given job for execution on the scheduler.
  ///
  /// Note that using this function in a job workflow is not optimal and you
  /// should use `Job.queue` instead.
  val inline queue:       Scheduler -> Job<unit> -> unit

  /// `queueIgnore xJ` is equivalent to `Job.Ignore xJ |> queue`.
  val queueIgnore: Scheduler -> Job<_>    -> unit

  /// Like `Scheduler.start`, but the given job is known never to return
  /// normally, so the job can be spawned in an even more lightweight manner.
  val server:      Scheduler -> Job<Void> -> unit

  /// Starts running the given job, but does not wait for the job to finish.
  ///
  /// Note that using this function in a job workflow is not optimal and you
  /// should use `Job.start` instead.
  val inline start:       Scheduler -> Job<unit> -> unit

  /// `startIgnore xJ` is equivalent to `Job.Ignore xJ |> start`.
  val startIgnore: Scheduler -> Job<_>    -> unit

  /// Starts running the given job, but does not wait for the job to finish.
  /// Upon the failure or success of the job, one of the given actions is called
  /// once.  See also: `abort`.
  ///
  /// Note that using this function in a job workflow is not optimal and you
  /// should instead use `Job.start` with desired Job exception handling
  /// construct (e.g. `Job.tryIn` or `Job.catch`).
  val startWithActions: Scheduler -> (exn -> unit) -> ('x -> unit) -> Job<'x> -> unit

  /// Starts running the given job on the specified scheduler and then blocks
  /// the current thread waiting for the job to either return successfully or
  /// fail.
#if DOC
  ///
  /// WARNING: Use of `run` should be considered carefully, because calling
  /// `run` from an arbitrary thread can cause deadlock.
  ///
  /// A call of `run xJ` is safe when the call is not made from within a Hopac
  /// worker thread and the job `xJ` does not perform operations that might
  /// block or that might directly, or indirectly, need to communicate with
  /// the thread from which `run` is being called.
#endif
  val run: Scheduler -> Job<'x> -> 'x

  /// Waits until the scheduler becomes completely idle.
  ///
  /// Note that for this to make sense, the scheduler should be a local
  /// scheduler that your program manages explicitly.
  val wait: Scheduler -> unit

  /// Kills the worker threads of the scheduler one-by-one.  This should only be
  /// used with a local scheduler that is known to be idle.
  val kill: Scheduler -> unit
