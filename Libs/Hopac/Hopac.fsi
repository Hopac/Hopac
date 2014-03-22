// Copyright (C) by Housemarque, Inc.

/// Hopac is a library for F# with the aim of making it easier to write
/// efficient parallel, asynchronous and concurrent programs.  The design of
/// Hopac draws inspiration from Concurrent ML.  Similar to Concurrent ML, Hopac
/// provides message passing primitives and supports the construction of
/// first-class synchronous abstractions.  Parallel jobs (light-weight threads)
/// in Hopac are created using techniques similar to the F# Async framework.
/// Hopac runs parallel jobs using a work distributing scheduler in a
/// non-preemptive fashion.
namespace Hopac

open System
open System.Collections.Generic

///////////////////////////////////////////////////////////////////////////////

/// A type that has no public constructors to indicate that a job or function
/// does not return normally.
type Void

///////////////////////////////////////////////////////////////////////////////

/// Expression builder type for jobs.  Note that the Job module provides more
/// combinators for building jobs.
type JobBuilder =
  new : unit -> JobBuilder
  member inline Bind: Job<'x> * ('x -> Job<'y>) -> Job<'y>
  member inline Combine: Job<unit> * Job<'x> -> Job<'x>
  member inline Delay: (unit -> Job<'x>) -> Job<'x>
  member inline For: seq<'x> * ('x -> Job<unit>) -> Job<unit>
  member inline For: array<'x> * ('x -> Job<unit>) -> Job<unit>
  member inline Return: 'x -> Job<'x>
  member inline ReturnFrom: Job<'x> -> Job<'x>
  member inline TryFinally: Job<'x> * (unit -> unit) -> Job<'x>
  member inline TryWith: Job<'x> * (exn -> Job<'x>) -> Job<'x>
  member inline Using: 'x * ('x -> Job<'y>) -> Job<'y> when 'x :> IDisposable
  member inline While: (unit -> bool) * Job<unit> -> Job<unit>
  member inline Zero: unit -> Job<unit>

///////////////////////////////////////////////////////////////////////////////

/// Convenience bindings for programming with Hopac.
[<AutoOpen>]
module TopLevel =
  /// Default expression builder for jobs.
  val job: JobBuilder

  /// Starts running the given job on the global scheduler, but does not wait
  /// for the job to finish.  The result, if any, of the job is ignored.  Note
  /// that using this function in a job workflow is not optimal and you should
  /// use "Job.start" instead.  This is the same function as "Job.Global.run".
  val inline run: Job<'x> -> 'x

  /// Use object as alternative.  This function is a NOP and is provided as a
  /// kind syntactic alternative to using a type cast.
  val inline asAlt: Alt<'x> -> Alt<'x>

  /// Use object as job.  This function is a NOP and is provided as a kind
  /// syntactic alternative to using a type cast.
  val inline asJob: Job<'x> -> Job<'x>

  /// Creates a new channel.  This is the same function as "Ch.Now.create".
  val inline ch: unit -> Ch<'x>

  /// Creates a new mailbox.  This is the same function as
  /// "Mailbox.Now.create".
  val inline mb: unit -> Mailbox<'x>

  /// Creates a new write once variable.  This is the same function as
  /// "IVar.Now.create".
  val inline ivar: unit -> IVar<'x>

  /// Creates a new synchronous variable that is initially empty.  This is the
  /// same function as "MVar.Now.create".
  val inline mvar: unit -> MVar<'x>

///////////////////////////////////////////////////////////////////////////////

/// Operations on jobs.
module Job =

  /// Operations on the global scheduler.  Note that in a typical program there
  /// should only be a few points (maybe just one) where jobs are started or run
  /// outside of job workflows.
  module Global =
    /// Starts running the given job on the global scheduler, but does not wait
    /// for the job to finish.  Upon the failure or success of the job, one of
    /// the given actions is called once.  Note that using this function in a
    /// job workflow is not optimal and you should instead use "Job.start" with
    /// desired Job exception handling construct (e.g. "Job.tryIn" or
    /// "Job.catch").
    val startWithActions: (exn -> unit) -> ('x -> unit) -> Job<'x> -> unit

    /// Starts running the given job on the global scheduler, but does not wait
    /// for the job to finish.  The result, if any, of the job is ignored.  Note
    /// that using this function in a job workflow is not optimal and you should
    /// use Job.start instead.
    val start: Job<_> -> unit

    /// Like Job.start, but the given job is known never to return normally, so
    /// the job can be spawned in a sligthly lighter-weight manner.
    val server: Job<Void> -> unit

    /// Starts running the job on the global scheduler and then waits for the
    /// job to either return successfully or fail.  Note that using this
    /// function in a job workflow is not optimal and should never be needed.
    val run: Job<'x> -> 'x

  /////////////////////////////////////////////////////////////////////////////

  /// Creates a job that schedules the given job to be executed as a separate
  /// concurrent job.  The result, if any, of the concurrent job is ignored.
  /// Use "Promise.start" if you need to be able to get the result.  Use
  /// "Job.server" if the job never returns normally.
  val start: Job<_> -> Job<unit>

  /// Like "Job.start", but the given job is known never to return normally, so
  /// the job can be spawned in a sligthly lighter-weight manner.
  val server: Job<Void> -> Job<unit>

  /////////////////////////////////////////////////////////////////////////////

  /// Creates a job that calls the given function to build a job that will then
  /// be run.
  val delay: (unit -> Job<'x>) -> Job<'x>

  /// Creates a job that calls the given function with the given value to build
  /// a job that will then be run.
  val delayWith: ('x -> Job<'y>) -> 'x -> Job<'y>

  /// Creates a job that calls the given function with the given value to
  /// compute the result of the job.
  val lift: ('x -> 'y) -> 'x -> Job<'y>

  /// Creates a job that invokes the given thunk to compute the result of the
  /// job.
  val thunk: (unit -> 'x) -> Job<'x>

  /////////////////////////////////////////////////////////////////////////////

  /// Returns a job that does nothing and returns ().  "unit ()" is equivalent
  /// to "result ()".
  val inline unit: unit -> Job<unit>

  /// Creates a job with the given result.
  val result: 'x -> Job<'x>

  /// Creates a job that immediately terminates the current job.  Note that in
  /// order to execute clean-up operations implemented with "using" or
  /// "tryFinallyFun" or "tryFinallyJob" the job must either return normally or
  /// raise an exception.
  val abort: unit -> Job<_>

  /// Creates a job that has the effect of raising the specified exception.
  /// "raises e" is equivalent to "delay <| fun () -> raise e".
  val raises: exn -> Job<_>

  /// Infix operators on jobs.  You can open this module to bring all of the
  /// infix operators into scope.
  module Infixes =
    /// Creates a job that first runs the given job and then passes the result
    /// of that job to the given function to build another job which will then
    /// be run.
    val (>>=): Job<'x> -> ('x -> Job<'y>) -> Job<'y>

    /// Creates a job that runs the given two jobs and returns the result of the
    /// second job.  "xJ >>. yJ" is equivalent to "xJ >>= fun _ -> yJ".
    val (>>.): Job<_> -> Job<'y> -> Job<'y>

    /// Creates a job that runs the given two jobs and returns the result of the
    /// first job.  "xJ .>> yJ" is equivalent to "xJ >>= fun x -> yJ >>% x".
    val (.>>): Job<'x> -> Job<_> -> Job<'x>

    /// Creates a job that runs the given job and maps the result of the job
    /// with the given function.  "xJ |>> x2y" is equivalent to "xJ >>= (x2y >>
    /// result)".
    val (|>>): Job<'x> -> ('x -> 'y) -> Job<'y>

    /// Creates a job that runs the given job and then returns the given value.
    /// "xJ >>% y" is equivalent to "xJ >>= fun _ -> result y".
    val (>>%): Job<'x> -> 'y -> Job<'y>

    /// Creates a job that runs the given job and then raises the given
    /// exception.  "xJ >>! e" is equivalent to "xJ >>= fun _ -> raise e".
    val (>>!): Job<'x> -> exn -> Job<_>

    /// Creates a job that runs the given two jobs and then returns a pair of
    /// their results.  "xJ <&> yJ" is equivalent to "xJ >>= fun x -> yK >>= fun
    /// y -> result (x, y)".
    val (<&>): Job<'x> -> Job<'y> -> Job<'x * 'y>

    /// Creates a job that either runs the given jobs sequentially, like "<&>",
    /// or as two separate parallel jobs and returns a pair of their results.
    /// Note that when the jobs are run in parallel and both of them raise an
    /// exception then the created job raises an AggregateException.  Note that
    /// it is not guaranteed that the jobs would be run as separate jobs.  This
    /// means that a job such as "let c = Ch.Now.create () in Ch.give c () <*>
    /// Ch.take c" may deadlock.
    val (<*>): Job<'x> -> Job<'y> -> Job<'x * 'y>

  /////////////////////////////////////////////////////////////////////////////

  /// Implements the try-in-unless exception handling construct for jobs.  Both
  /// of the continuation jobs "'x -> Job<'y>", for success, and "exn ->
  /// Job<'y>", for failure, are invoked from a tail position.
  val tryIn: Job<'x> -> ('x -> Job<'y>) -> (exn -> Job<'y>) -> Job<'y>

  /// Implements the try-with exception handling construct for jobs.
  val tryWith: Job<'x> -> (exn -> Job<'x>) -> Job<'x>

  /// Implements a variation of the try-finally exception handling construct for
  /// jobs.  The given action, specified as a function, is executed after the
  /// job has been run, whether it fails or completes successfully.
  val tryFinallyFun: Job<'x> -> (unit -> unit) -> Job<'x>

  /// Implements a variation of the try-finally exception handling construct for
  /// jobs.  The given action, specified as a job, is executed after the the job
  /// has been run, whether it fails or completes successfully.
  val tryFinallyJob: Job<'x> -> Job<unit> -> Job<'x>

  /// Implements the use construct for jobs.  The Dispose method of the given
  /// disposable object is called after running the job constructed with the
  /// disposable object.
  val using: 'x -> ('x -> Job<'y>) -> Job<'y> when 'x :> IDisposable

  /// Creates a job that runs the given job and results in either the ordinary
  /// result of the job or the exception raised by the job.  "catch j" is
  /// equivalent to "tryIn j (lift Choice1Of2) (lift Choice2Of2)".
  val catch: Job<'x> -> Job<Choice<'x, exn>>

  /////////////////////////////////////////////////////////////////////////////

  /// Creates a job that runs the given job sequentially the given number of
  /// times.  The results from the jobs are ignored.
  val forN: int -> Job<_> -> Job<unit>

  /// "forUpTo lo hi i2xJ" creates a job that sequentially iterates from lo to
  /// hi (inclusive) and calls the given function to construct jobs that will be
  /// executed.  The results from the jobs are ignored.
  val forUpTo: int -> int -> (int -> Job<_>) -> Job<unit>

  /// "forDownTo hi lo i2xJ" creates a job that sequentially iterates from hi to
  /// lo (inclusive) and calls the given function to construct jobs that will be
  /// executed.  The results from the jobs are ignored.
  val forDownTo: int -> int -> (int -> Job<_>) -> Job<unit>

  /// "whileDo cond body" creates a job that sequentially executes the body job
  /// as long as cond returns true.  The results from the jobs are ignored.
  val whileDo: (unit -> bool) -> Job<_> -> Job<unit>

  /// "whenDo b uJ" is equivalent to "if b then uJ else Job.unit ()".
  val inline whenDo: bool -> Job<unit> -> Job<unit>

  /////////////////////////////////////////////////////////////////////////////

  /// Creates a job that repeats the given job indefinitely.  Note that the
  /// results, if any, from the given job are ignored.  It is a common
  /// programming pattern to use server jobs that loop indefinitely and
  /// communicate with clients via channels.  When a job is blocked waiting for
  /// communication on one or more channels and the channels become garbage (no
  /// longer reachable by any other job) the job can be garbage collected as
  /// well.
  val forever: Job<_> -> Job<_>

  /// Creates a job that indefinitely iterates the given job constructor
  /// starting with the given value.  More precisely, "iter x x2xJ" is
  /// equivalent to "let rec lp x = x2xJ x >>= lp in lp x".  It is a common
  /// programming pattern to use server jobs that loop indefinitely and
  /// communicate with clients via channels.  When a job is blocked waiting for
  /// communication on one or more channels and the channels become garbage (no
  /// longer reachable by any other job) the job can be garbage collected as
  /// well.
  val iterate: 'x -> ('x -> Job<'x>) -> Job<_>

  /////////////////////////////////////////////////////////////////////////////

  /// Creates a job that starts a separate server job that repeats the given job
  /// indefinitely.  "foreverServer xJ" is equivalent to "forever xJ |> server".
  val foreverServer: Job<_> -> Job<unit>

  /// Creates a job that starts a separate server job that indefinitely iterates
  /// the given job constructor starting with the given value.  "iterateServer x
  /// x2xJ" is equivalent to "iterate x x2xJ |> server".
  val iterateServer: 'x -> ('x -> Job<'x>) -> Job<unit>

  /////////////////////////////////////////////////////////////////////////////

  /// Creates a job that runs all of the jobs in sequence and returns a list of
  /// the results.
  val seqCollect: seq<Job<'x>> -> Job<ResizeArray<'x>>

  /// Creates a job that runs all of the jobs in sequence.  The results of the
  /// jobs are ignored.
  val seqIgnore: seq<Job<_>> -> Job<unit>

  /// Creates a job that runs all of the jobs as separate concurrent jobs and
  /// returns a list of the results.  Note that when multiple jobs raise
  /// exceptions, then the created job raises an AggregateException.
  val conCollect: seq<Job<'x>> -> Job<ResizeArray<'x>>

  /// Creates a job that runs all of the jobs as separate concurrent jobs and
  /// then waits for all of the jobs to finish.  The results of the jobs are
  /// ignored.  Note that when multiple jobs raise exceptions, then the created
  /// job raises an AggregateException.
  val conIgnore: seq<Job<_>> -> Job<unit>

  /////////////////////////////////////////////////////////////////////////////

  /// Creates a job that performs the asynchronous operation defined by the
  /// given pair of begin and end operations.
  val fromBeginEnd: (AsyncCallback * obj -> IAsyncResult)
                 -> (IAsyncResult -> 'x)
                 -> Job<'x>

///////////////////////////////////////////////////////////////////////////////

/// Operations on first-class synchronous operations or alternatives.
module Alt =
  /// Creates an alternative that is always available for picking and results in
  /// the given value.  Note that when there are alternatives immediately
  /// available for picking in a disjunction, the first such alternative will be
  /// committed to.
  val inline always: 'x -> Alt<'x>

  /// Returns an alternative that is always available for picking and results in
  /// the unit value.  "unit ()" is equivalent to "always ()".
  val inline unit: unit -> Alt<unit>

  /// Creates an alternative that is never available for picking.
  val never: unit -> Alt<'x>

  /// Return an alternative that is never available for picking.  "zero ()" is
  /// equivalent to "never ()".
  val inline zero: unit -> Alt<unit>

  /// Creates an alternative that is computed at instantiation time with the
  /// given job.
  val guard: Job<Alt<'x>> -> Alt<'x>

  /// Creates an alternative that is computed at instantiation time with the
  /// given thunk.
  val delay: (unit -> Alt<'x>) -> Alt<'x>

  /// Creates an alternative that is computed at instantiation time with the
  /// given job constructed with a negative acknowledgment alternative.  The
  /// negative acknowledgment alternative will be available for picking in case
  /// some other instantiated alternative involved in the picking is committed
  /// to instead.  Note that if an alternative created with withNack is not
  /// instantiated, then no negative acknowledgment is created.  For example,
  /// given an alternative of the form "always () <|> withNack (...)"  the
  /// withNack alternative is never instantiated.
  val withNack: (Alt<unit> -> Job<Alt<'x>>) -> Alt<'x>

  /// Creates an alternative that is available for picking when any one of the
  /// given alternatives is.  More precisely, "choose alts" is equivalent to
  /// "delay (fun () -> Seq.foldBack (<|>) alts never)", given "Seq.foldBack"
  /// with the obvious meaning.
  val choose: seq<Alt<'x>> -> Alt<'x>

  /////////////////////////////////////////////////////////////////////////////

  /// Infix operators on alternatives.  You can open this module to bring all
  /// of the infix operators into scope.
  module Infixes =
    /// Creates an alternative that is available for picking when either of the
    /// given alternatives is available.  The given alternatives are processed
    /// in a left-to-right order with short-cut evaluation.  In other words,
    /// given an alternative of the form "first <|> second", the "first"
    /// alternative is first instantiated and, if it is pickable, is committed
    /// to and the "second" alternative will not be instantiated at all.
    val (<|>): Alt<'x> -> Alt<'x> -> Alt<'x>

    /// Creates an alternative whose result is passed to the given job
    /// constructor and processed with the resulting job after the given
    /// alternative has been committed to.  Although this operator has a type
    /// similar to a monadic bind operation, alternatives do not form a monad
    /// (with the always alternative constructor).  So called Transactional
    /// Events do form a monad, but require a more complex synchronization
    /// protocol.
    val (>>=?): Alt<'x> -> ('x -> Job<'y>) -> Alt<'y>

    /// "xA >>.? yJ" is equivalent to "xA >>=? fun _ -> yJ".
    val (>>.?): Alt<_> -> Job<'y> -> Alt<'y>

    /// "xA .>>? yJ" is equivalent to "xA >>=? fun x -> yJ >>% x".
    val (.>>?): Alt<'x> -> Job<_> -> Alt<'x>

    /// "xA |>>? x2y" is equivalent to "xA >>=? (x2y >> result)".
    val (|>>?): Alt<'x> -> ('x -> 'y) -> Alt<'y>

    /// "xA >>%? y" is equivalent to "xA >>=? fun _ -> result y".
    val (>>%?): Alt<'x> -> 'y -> Alt<'y>

    /// "xA >>!? e" is equivalent to "xA >>=? fun _ -> raise e".
    val (>>!?): Alt<'x> -> exn -> Alt<_>

  /////////////////////////////////////////////////////////////////////////////

  /// Implements the try-in-unless exception handling construct for
  /// alternatives.  Both of the continuation jobs "'x -> Job<'y>", for success,
  /// and "exn -> Job<'y>", for failure, are invoked from a tail position.
  /// Exceptions from both before and after the commit point can be handled.  An
  /// exception that occurs before a commit point, from the user code in a
  /// guard, delay, or withNack, results in treating that exception as the
  /// commit point.  Note you can also use function or job level exception
  /// handling before the commit point within the user code in a guard, delay,
  /// or withNack.
  val tryIn: Alt<'x> -> ('x -> Job<'y>) -> (exn -> Job<'y>) -> Alt<'y>

  /////////////////////////////////////////////////////////////////////////////

  /// Creates a job that instantiates the alternative, waits until it becomes
  /// available for picking and then commits to the alternative and results in
  /// its value.  This function is a NOP and is provided as a kind of syntactic
  /// alternative to using a type ascription or a type cast.
  val inline pick: Alt<'x> -> Job<'x>

  /// Creates a job that instantiates the given sequence of alternatives
  /// one-by-one, waits until at least one of them becomes available for picking
  /// and then commits to the alternative resulting in its value.  "select xAs"
  /// is equivalent to "pick (choose xJs)".
  val inline select: seq<Alt<'x>> -> Job<'x>

///////////////////////////////////////////////////////////////////////////////

/// Operations on a wall-clock timer.
module Timer =

  /// Operations on the global wall-clock timer.  The global timer is implicitly
  /// associated with the global scheduler.
  module Global =
    /// Creates an alternative that, after instantiation, becomes pickable after
    /// the specified time span.  Note that this is simply not intended for high
    /// precision timing and the resolution of the underlying timing mechanism
    /// is very coarse (Windows system ticks).
    val timeOut: TimeSpan -> Alt<unit>

    /// Creates a job that sleeps for (about) the specified time.  Note that
    /// this is simply not intended for high precision timing and the resolution
    /// of the underlying timing mechanism is coarse (Windows system ticks).
    val sleep: TimeSpan -> Job<unit>

///////////////////////////////////////////////////////////////////////////////

/// Operations on synchronous channels.
module Ch =
  /// Immediate or non-workflow operations on synchronous channels.
  module Now =
    /// Creates a new channel.
    val inline create: unit -> Ch<'x>

  /// Operations bound to the global scheduler.
  module Global =
    /// Sends the given value to the specified channel.  Note that using this
    /// function in a job workflow is not generally optimal and you should use
    /// "Ch.send" instead.
    val send: Ch<'x> -> 'x -> unit

  /// Creates a job that creates a new channel.
  val create: unit -> Job<Ch<'x>>

  /// Creates a job that offers to give the given value to another job on the
  /// given channel.  A give operation is synchronous.  In other words, a give
  /// operation waits until another job takes the value.
  val inline give: Ch<'x> -> 'x -> Job<unit>

  /// Creates a job that sends a value to another job on the given channel.  A
  /// send operation is asynchronous.  In other words, a send operation does not
  /// wait for another job to give the value to.  Note that channels have been
  /// optimized for synchronous operations; an occasional send can be efficient,
  /// but when sends are queued, performance maybe be significantly worse than
  /// with a Mailbox optimized for buffering.
  val inline send: Ch<'x> -> 'x -> Job<unit>

  /// Creates a job that offers to take a value from another job on the given
  /// channel.  In other words, a take operation waits until another job gives
  /// (or sends) a value.
  val inline take: Ch<'x> -> Job<'x>

  /// Selective operations on synchronous channels.
  module Alt =
    /// Creates an alternative that, at instantiation time, offers to give the
    /// given value on the given channel, and becomes available for picking when
    /// another job offers to take the value.
    val inline give: Ch<'x> -> 'x -> Alt<unit>

    /// Creates an alternative that, at instantiation time, offers to take a
    /// value from another job on the given channel, and becomes available for
    /// picking when another job offers to give a value.
    val inline take: Ch<'x> -> Alt<'x>

///////////////////////////////////////////////////////////////////////////////

/// Operations on write once variables.
module IVar =
  /// Immediate or non-workflow operations on write once variables.
  module Now =
    /// Creates a new write once variable.
    val inline create: unit -> IVar<'x>

  /// Creates a job that creates a new write once variable.
  val create: unit -> Job<IVar<'x>>

  /// Creates a job that writes to the given write once variable.  It is an
  /// error to write to a single "IVar" more than once.  This assumption may be
  /// used to optimize the implementation and incorrect usage leads to undefined
  /// behavior.
  val inline fill: IVar<'x> -> 'x -> Job<unit>

  /// Creates a job that, if necessary, waits until the given write once
  /// variable is written and then returns the written value.
  val inline read: IVar<'x> -> Job<'x>

  /// Selective operations on write once variables.
  module Alt =
    /// Creates an alternative that becomes available for picking after the
    /// write once variable has been written to.
    val inline read: IVar<'x> -> Alt<'x>

///////////////////////////////////////////////////////////////////////////////

/// Operations on write many variables.
module MVar =
  /// Immediate or non-workflow operations on write many variables.
  module Now =
    /// Creates a new synchronous variable that is initially empty.
    val inline create: unit -> MVar<'x>

    /// Creates a new synchronous variable that initially contains the given
    /// value.
    val inline createFull: 'x -> MVar<'x>

  /// Creates a job that creates a new synchronous variable that is initially
  /// empty.
  val create: unit -> Job<MVar<'x>>

  /// Creates a job that creates a new synchronous variable that initially
  /// contains the given value.
  val createFull: 'x -> Job<MVar<'x>>

  /// Creates a job that writes the given value to the synchronous variable.  It
  /// is an error to write to a MVar that is full.  This assumption may be used
  /// to optimize the implementation and incorrect usage leads to undefined
  /// behavior.
  val inline fill: MVar<'x> -> 'x -> Job<unit>

  /// Creates a job that waits until the synchronous variable contains a value
  /// and then takes the value contained by the synchronous variable leaving the
  /// variable empty.
  val inline take: MVar<'x> -> Job<'x>

  /// Creates a job that takes the value of the variable and then fills the
  /// variable with the result of performing the given function.  Note that this
  /// operation is not atomic.  However, it is a common programming pattern to
  /// make it so that only the job that has emptied an "MVar" by taking a value
  /// from it is allowed to fill the "MVar".  Such an access pattern makes
  /// operations on the "MVar" appear as atomic.
  val inline modifyFun: ('x -> 'x * 'y) -> MVar<'x> -> Job<'y>

  /// Creates a job that takes the value of the variable and then fills the
  /// variable with the result of performing the given job.  Note that this
  /// operation is not atomic.  However, it is a common programming pattern to
  /// make it so that only the job that has emptied an MVar by taking a value
  /// from it is allowed to fill the "MVar".  Such an access pattern makes
  /// operations on the "MVar" appear as atomic.
  val inline modifyJob: ('x -> Job<'x * 'y>) -> MVar<'x> -> Job<'y>

  /// Selective operations on write many variables.
  module Alt =
    /// Creates an alternative that becomes available for picking when the
    /// variable contains a value and, if committed to, takes the value from the
    /// variable.
    val inline take: MVar<'x> -> Alt<'x>

///////////////////////////////////////////////////////////////////////////////

/// Operations on buffered mailboxes.
module Mailbox =
  /// Immediate or non-workflow operations on buffered mailboxes.
  module Now =
    /// Creates a new mailbox.
    val inline create: unit -> Mailbox<'x>

  /// Operations bound to the global scheduler.
  module Global =
    /// Sends the given value to the specified mailbox.  Note that using this
    /// function in a job workflow is not generally optimal and you should use
    /// Mailbox.send instead.
    val send: Mailbox<'x> -> 'x -> unit

  /// Creates a job that creates a new mailbox.
  val create: unit -> Job<Mailbox<'x>>

  /// Creates a job that sends the given value to the specified mailbox.  This
  /// operation never blocks.
  val inline send: Mailbox<'x> -> 'x -> Job<unit>

  /// Creates a job that waits until the specified mailbox contains at least one
  /// value and then takes a value from the mailbox and returns it.  Values are
  /// taken in FIFO order.
  val inline take: Mailbox<'x> -> Job<'x>

  /// Selective operations on buffered mailboxes.
  module Alt =
    /// Creates and alternative that becomes available for picking when the
    /// mailbox contains at least one value and, if committed to, takes a value
    /// from the mailbox.
    val inline take: Mailbox<'x> -> Alt<'x>

///////////////////////////////////////////////////////////////////////////////

/// Operations on promises.
module Promise =
  /// Immediate or non-workflow operations on promises.
  module Now =
    /// Creates a promise whose value is computed lazily with the given job when
    /// an attempt is made to read the promise.  Although the job is not started
    /// immediately, the effect is that the delayed job will be run as a
    /// separate job, which means it is possible to communicate with it as long
    /// the delayed job is started before trying to communicate with it.
    val inline delay: Job<'x> -> Promise<'x>

    /// Creates a promise with the given value.
    val inline withValue: 'x -> Promise<'x>

    /// Creates a promise with the given failure exception.
    val inline withFailure: exn -> Promise<'x>

  /// Creates a job that creates a promise, whose value is computed eagerly with
  /// the given job, which is started to run as a separate concurrent job.
  val start: Job<'x> -> Job<Promise<'x>>

  /// Creates a job that creates a promise, whose value is computed with the
  /// given job, when an attempt is made to read the promise.  Although the job
  /// is not started immediately, the effect is that the delayed job will be run
  /// as a separate job, which means it is possible to communicate with it as
  /// long the delayed job is started before trying to communicate with it.
  val delay: Job<'x> -> Job<Promise<'x>>

  /// Creates a job that waits for the promise to be computed and then returns
  /// its value (or fails with exception).  If the job of promise was delayed,
  /// it is first started as a separate job.
  val inline read: Promise<'x> -> Job<'x>

  /// Selective operations on promises.
  module Alt =
    /// Creates an alternative for reading the promise.  If the job of the
    /// promise was delayed, it is started as a separate job.
    val inline read: Promise<'x> -> Alt<'x>

///////////////////////////////////////////////////////////////////////////////

/// Operations on mutual exclusion locks.
module Lock =
  /// Creates a job that creates a new mutual exclusion lock.
  val create: unit -> Job<Lock>

  /// Creates a job that calls the given function so that the lock is held
  /// during the execution of the function.  This locking method is optimized
  /// for short duration locking so that other jobs contending for the lock will
  /// keep spinning during the execution of the function.
  val inline duringFun: Lock -> (unit -> 'x) -> Job<'x>

  /// Creates a job that runs the given job so that the lock is held during the
  /// execution of the given job.  This locking method is optimized for longer
  /// duration locking so that other jobs contending for the lock will be
  /// blocked to a queue during the execution of the function.  Note that
  /// blocking and resuming a job is much faster than blocking and resuming a
  /// native thread.
  val inline duringJob: Lock -> Job<'x> -> Job<'x>

  /// Immediate or non-workflow operations on locks.
  module Now =
    /// Creates a new lock.
    val inline create: unit -> Lock

///////////////////////////////////////////////////////////////////////////////

/// Extensions to various system modules and types for programming with jobs.
/// You can open this module to use the extensions much like as if they were
/// part of the existing modules and types.
module Extensions =
  /// Operations for processing arrays with jobs.
  module Array =
    /// Sequentially maps the given job constructor to the elements of the array
    /// and returns an array of the results.
    val mapJob: ('x -> Job<'y>) -> array<'x> -> Job<array<'y>>

    /// Sequentially iterates the given job constructor over the given array.
    /// The results, if any, of the jobs are ignored.
    val iterJob: ('x -> Job<_>) -> array<'x> -> Job<unit>

  /// Operations for processing sequences with jobs.
  module Seq =
    /// Sequentially iterates the given job constructor over the given sequence.
    /// The results, if any, of the jobs are ignored.
    val iterJob: ('x -> Job<_>) -> seq<'x> -> Job<unit>

    /// Sequentially maps the given job constructor to the elements of the
    /// sequence and returns a list of the results.
    val mapJob: ('x -> Job<'y>) -> seq<'x> -> Job<ResizeArray<'y>>

    /// Sequentially folds the job constructor over the given sequence and
    /// returns the result of the fold.
    val foldJob: ('x -> 'y -> Job<'x>) -> 'x -> seq<'y> -> Job<'x>

    /// Operations for processing sequences using concurrent Hopac jobs.
    module Con =
      /// Iterates the given job constructor over the given sequence, runs the
      /// constructed jobs as separate concurrent jobs and waits until all of
      /// the jobs have finished.  The results of the created jobs are ignored.
      val iterJob: ('x -> Job<_>) -> seq<'x> -> Job<unit>

      /// Iterates the given job constructor over the given sequence, runs the
      /// constructed jobs as separate concurrent jobs and waits until all of
      /// the jobs have finished collecting the results into a list.
      val mapJob: ('x -> Job<'y>) -> seq<'x> -> Job<ResizeArray<'y>>

  /// Operations for interfacing tasks with jobs.
  type [<Sealed>] Task =
    /// Creates a job that waits for the given task to finish and then returns
    /// the result of the task.  Note that this does not start the job.
    static member inline awaitJob: Threading.Tasks.Task<'x> -> Job<'x>

    /// Creates a job that waits until the given task finishes.  Note that this
    /// does not start the job.
    static member inline awaitJob: Threading.Tasks.Task -> Job<unit>

///////////////////////////////////////////////////////////////////////////////

/// Operations on schedulers.  Use of this module requires more intimate
/// knowledge of Hopac, but may allow adapting Hopac to special application
/// requirements.
module Scheduler =

  /// Operations on the global scheduler.
  module Global =  
    /// Sets the top level exception handler job constructor of the global
    /// scheduler.  When a job fails with an otherwise unhandled exception, the
    /// job is killed and a new job is constructed with the top level handler
    /// constructor and then started.  To avoid infinite loops, in case the top
    /// level handler job raises exceptions, it is simply killed after printing
    /// a message to the console.  The default top level handler, or None,
    /// simply prints out a message to the console.
    val setTopLevelHandler: option<exn -> Job<unit>> -> unit

  /// A record of scheduler configuration options.
  type Create =
    {
      /// Number of worker threads.  Using more than
      /// "Environment.ProcessorCount" is not optimal and may, in some cases,
      /// significantly reduce performance.  The default is
      /// "Environment.ProcessorCount".
      NumWorkers: option<int>

      /// Specifies the top level exception handler job constructor of the
      /// scheduler.  When a job fails with an otherwise unhandled exception,
      /// the job is killed and a new job is constructed with the top level
      /// handler constructor and then started.  To avoid infinite loops, in
      /// case the top level handler job raises exceptions, it is simply killed
      /// after printing a message to the console.  The default top level
      /// handler simply prints out a message to the console.
      TopLevelHandler: option<exn -> Job<unit>>
     
      /// Specifies the idle handler for workers.  The worker idle handler is
      /// run whenever an individual worker runs out of work.  The idle handler
      /// must return an integer value that specifies how many milliseconds the
      /// worker is allowed to sleep.  "Timeout.Infinite" puts the worker into
      /// sleep until the scheduler explicitly wakes it up.  "0" means that the
      /// idle handler found some new work and the worker should immediately
      /// look for it.
      IdleHandler: option<Job<int>>
    }
    
    /// Default options.
    static member Def: Create

  /// Creates a new local scheduler.  Note that a local scheduler does not
  /// automatically implement services such as the global wall-clock timer.
  val create: Create -> Scheduler

  /// Starts running the given job, but does not wait for the job to finish.
  /// Upon the failure or success of the job, one of the given actions is called
  /// once.  Note that using this function in a job workflow is not optimal and
  /// you should instead use "Job.start" with desired Job exception handling
  /// construct (e.g. "Job.tryIn" or "Job.catch").
  val startWithActions: Scheduler
                     -> (exn -> unit)
                     -> ('x -> unit)
                     -> Job<'x> -> unit

  /// Starts running the given job, but does not wait for the job to finish.
  /// The result, if any, of the job is ignored.  Note that using this function
  /// in a job workflow is not optimal and you should use "Job.start" instead.
  val start: Scheduler -> Job<_> -> unit

  /// Like "Scheduler.start", but the given job is known never to return
  /// normally, so the job can be spawned in a sligthly lighter-weight manner.
  val server: Scheduler -> Job<Void> -> unit

  /// Waits until the scheduler becomes completely idle.
  val wait: Scheduler -> unit

  /// Kills the worker threads of the scheduler one-by-one.  This should only be
  /// used with a local scheduler that is known to be idle.
  val kill: Scheduler -> unit

///////////////////////////////////////////////////////////////////////////////

/// Additional infix operators.  You can open this module to bring all of the
/// infix operators into scope.
module Infixes =
  /// Creates an alternative that, at instantiation time, offers to give the
  /// given value on the given channel, and becomes available for picking when
  /// another job offers to take the value.  "xCh <-? x" is equivalent to
  /// "Ch.Alt.give xCh x".
  val inline (<-?): Ch<'x> -> 'x -> Alt<unit>

  /// Creates a job that offers to give the given value to another job on the
  /// given channel.  A give operation is synchronous.  In other words, a give
  /// operation waits until another job takes the value.  "xCh <-- x" is
  /// equivalent to "Ch.give xCh x".
  val inline (<--): Ch<'x> -> 'x -> Job<unit>

  /// Creates a job that sends a value to another job on the given channel.  A
  /// send operation is asynchronous.  In other words, a send operation does not
  /// wait for another job to give the value to.  Note that channels have been
  /// optimized for synchronous operations; an occasional send can be efficient,
  /// but when sends are queued, performance maybe be significantly worse than
  /// with a Mailbox optimized for buffering.  "xCh <-+ x" is equivalent to
  /// "Ch.send xCh x".
  val inline (<-+): Ch<'x> -> 'x -> Job<unit>

  /// Creates a job that writes to the given write once variable.  It is an
  /// error to write to a single "IVar" more than once.  This assumption may be
  /// used to optimize the implementation and incorrect usage leads to undefined
  /// behavior.  "xI <-= x" is equivalent to "IVar.fill xI x".
  val inline (<-=): IVar<'x> -> 'x -> Job<unit>

  /// Creates a job that writes the given value to the synchronous variable.  It
  /// is an error to write to a MVar that is full.  This assumption may be used
  /// to optimize the implementation and incorrect usage leads to undefined
  /// behavior.  "xM <<-= x" is equivalent to "MVar.fill xM x".
  val inline (<<-=): MVar<'x> -> 'x -> Job<unit>

  /// Creates a job that sends the given value to the specified mailbox.  This
  /// operation never blocks.  "xMb <<-+ x" is equivalent to "Mailbox.send xMb
  /// x".
  val inline (<<-+): Mailbox<'x> -> 'x -> Job<unit>
