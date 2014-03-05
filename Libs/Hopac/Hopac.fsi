// Copyright (C) by Housemarque, Inc.

namespace Hopac

open System
open System.Collections.Generic

/////////////////////////////////////////////////////////////////////////

/// Expression builder type for jobs.  Note that the Job module provides
/// more combinators for building jobs.
type JobBuilder =
  new : unit -> JobBuilder
  member inline Bind: Job<'a> * ('a -> Job<'b>) -> Job<'b>
  member inline Combine: Job<unit> * Job<'a> -> Job<'a>
  member inline Delay: (unit -> Job<'a>) -> Job<'a>
  member inline For: seq<'a> * ('a -> Job<unit>) -> Job<unit>
  member inline For: array<'a> * ('a -> Job<unit>) -> Job<unit>
  member inline Return: 'a -> Job<'a>
  member inline ReturnFrom: Job<'a> -> Job<'a>
  member inline TryFinally: Job<'a> * (unit -> unit) -> Job<'a>
  member inline TryWith: Job<'a> * (exn -> Job<'a>) -> Job<'a>
  member inline Using: 'a * ('a -> Job<'b>) -> Job<'b> when 'a :> IDisposable
  member inline While: (unit -> bool) * Job<unit> -> Job<unit>
  member inline Zero: unit -> Job<unit>

/////////////////////////////////////////////////////////////////////////

[<AutoOpen>]
module TopLevel =
  /// Default expression builder for jobs.
  val job: JobBuilder

  /// Convenience binding for running Hopac jobs.  This is the same function
  /// as Job.Now.run.
  val inline run: Job<'a> -> 'a

/////////////////////////////////////////////////////////////////////////

/// Operations on parallel jobs.
module Job =

  /// Immediate or non-workflow operations on parallel jobs.
  module Now =
    /// Starts running the given job, but does not wait for the job to finish.
    /// Upon the failure or success of the job, one of the given actions is
    /// called once.  Note that using this function in a job workflow is not
    /// optimal and you should instead use Job.start with desired Job exception
    /// handling construct (e.g. Job.tryIn or Job.catch).
    val startWithActions: (exn -> unit) -> ('a -> unit) -> Job<'a> -> unit

    /// Starts running the given job, but does not wait for the job to finish.
    /// Note that using this function in a job workflow is not optimal and you
    /// should use Job.start instead.
    val start: Job<'a> -> unit

    /// Starts running the job and then waits for the job to either return
    /// successfully or fail.  Note that using this function in a job workflow
    /// is not optimal and should never be needed.
    val run: Job<'a> -> 'a

  ///////////////////////////////////////////////////////////////////////

  /// Creates a job that schedules the given job to be executed as a separate
  /// parallel job.  The result, if any, of the separate job is ignored.  Use
  /// Promise.start if you need to be able to get the result.  Note that it is
  /// guaranteed that the job is executed as a separate job.  This means that a
  /// job such as "let c = Ch.Now.create () in Job.start (Ch.give c ()) >>.
  /// Ch.take c" will not deadlock.
  val start: Job<'a> -> Job<unit>

  ///////////////////////////////////////////////////////////////////////

  /// Creates a job that calls the given function to build a job that will
  /// then be run.
  val delay: (unit -> Job<'a>) -> Job<'a>

  /// Creates a job that calls the given function with the given value to build
  /// a job that will then be run.
  val delayWith: ('a -> Job<'b>) -> 'a -> Job<'b>

  /// Creates a job that calls the given function with the given value to
  /// compute the result of the job.
  val lift: ('a -> 'b) -> 'a -> Job<'b>

  /// Creates a job that invokes the given thunk to compute the result of the
  /// job.
  val thunk: (unit -> 'a) -> Job<'a>

  ///////////////////////////////////////////////////////////////////////

  /// "unit" is equivalent to "result ()".
  val unit: Job<unit>

  /// Creates a job with the given result.
  val result: 'a -> Job<'a>

  /// Infix operators on jobs.  You can open this module to bring all of the
  /// infix operators into scope.
  module Infixes =
    /// Creates a job that first runs the given job and then passes the
    /// result of that job to the given function to build another job which
    /// will then be run.
    val (>>=): Job<'a> -> ('a -> Job<'b>) -> Job<'b>

    /// "xJ >>. yJ" is equivalent to "xJ >>= fun _ -> yJ".
    val (>>.): Job<'a> -> Job<'b> -> Job<'b>

    /// "xJ .>> yJ" is equivalent to "xJ >>= fun x -> yJ >>% x".
    val (.>>): Job<'a> -> Job<'b> -> Job<'a>

    /// "xJ |>> x2y" is equivalent to "xJ >>= (x2y >> result)".
    val (|>>): Job<'a> -> ('a -> 'b) -> Job<'b>

    /// "xJ >>% y" is equivalent to "xJ >>= fun _ -> result y".
    val (>>%): Job<'a> -> 'b -> Job<'b>

    /// "xJ >>! e" is equivalent to "xJ >>= fun _ -> raise e".
    val (>>!): Job<'a> -> exn -> Job<'b>

    /// "xJ <&> yJ" is equivalent to "xJ >>= fun x -> yK >>= fun y -> result (x, y)".
    val (<&>): Job<'a> -> Job<'b> -> Job<'a * 'b>

    /// Creates a job that either runs the given jobs sequentially, like <&>,
    /// or as two separate parallel jobs and returns a pair of their results.
    /// Note that it is not guaranteed that the jobs would be run as separate
    /// jobs.  This means that a job such as "let c = Ch.Now.create () in
    /// Ch.give c () <*> Ch.take c" may deadlock.
    val (<*>): Job<'a> -> Job<'b> -> Job<'a * 'b>

  ///////////////////////////////////////////////////////////////////////

  /// Implements the try-in-unless exception handling construct for jobs.
  /// Both of the continuation jobs "'a -> Job<'b>", for success, and "exn
  /// -> Job<'b>", for failure, are invoked from a tail position.
  val tryIn: Job<'a> -> ('a -> Job<'b>) -> (exn -> Job<'b>) -> Job<'b>

  /// Implements the try-with exception handling construct for jobs.
  val tryWith: Job<'a> -> (exn -> Job<'a>) -> Job<'a>

  /// Implements the try-finally exception handling construct for jobs.  The
  /// given action is executed after the job has been run, whether it fails or
  /// completes successfully.
  val tryFinally: Job<'a> -> (unit -> unit) -> Job<'a>

  /// Implements the use construct for jobs.  The Dispose method of the
  /// given disposable object is called after running the job constructed
  /// with the disposable object.
  val using: 'a -> ('a -> Job<'b>) -> Job<'b> when 'a :> IDisposable

  /// "catch j" is equivalent to "tryIn j (lift Choice1Of2) (lift Choice2Of2)".
  val catch: Job<'a> -> Job<Choice<'a, exn>>

  ///////////////////////////////////////////////////////////////////////

  /// Creates a job that runs the given job sequentially the given number of
  /// times.  The results from the jobs are ignored.
  val forN: int -> Job<'a> -> Job<unit>

  /// "forUpTo lo hi i2xJ" creates a job that sequentially iterates from lo to
  /// hi (inclusive) and calls the given function to construct jobs that will
  /// be executed.  The results from the jobs are ignored.
  val forUpTo: int -> int -> (int -> Job<_>) -> Job<unit>

  /// "forDownTo hi lo i2xJ" creates a job that sequentially iterates from hi
  /// to lo (inclusive) and calls the given function to construct jobs that
  /// will be executed.  The results from the jobs are ignored.
  val forDownTo: int -> int -> (int -> Job<_>) -> Job<unit>

  /// Creates a job that runs the given job sequentially for an indefinite
  /// number of times.  It is a common programming pattern to use server jobs
  /// that loop indefinitely and communicate with clients via channels.  When a
  /// job is blocked waiting for communication on one or more channels and the
  /// channels become garbage (no longer reachable by any other job) the job
  /// can be garbage collected as well.
  val forever: Job<'a> -> Job<_>

  /// Creates a job that indefinitely iterates the given job constructor
  /// starting with the given value.  More precisely, "iter x x2xJ" is
  /// equivalent to "let rec lp x = x2xJ x >>= lp in lp".  It is a common
  /// programming pattern to use server jobs that loop indefinitely and
  /// communicate with clients via channels.  When a job is blocked waiting for
  /// communication on one or more channels and the channels become garbage (no
  /// longer reachable by any other job) the job can be garbage collected as
  /// well.
  val iterate: 'a -> ('a -> Job<'a>) -> Job<_>

  /// "whileDo cond body" creates a job that sequentially executes the body job
  /// as long as cond returns true.  The results from the jobs are ignored.
  val whileDo: (unit -> bool) -> Job<_> -> Job<unit>

  /// "whenDo b uJ" is equivalent to "if b then uJ else Job.unit".
  val inline whenDo: bool -> Job<unit> -> Job<unit>

  ///////////////////////////////////////////////////////////////////////

  /// Creates a job that runs all of the jobs in sequence and returns a
  /// sequence of the results.
  val seqCollect: seq<Job<'a>> -> Job<IList<'a>>

  /// Creates a job that runs all of the jobs in sequence and returns a
  /// sequence of the results.
  val seqIgnore: seq<Job<_>> -> Job<unit>

  /// Creates a job that runs all of the jobs potentially in parallel and
  /// returns a sequence of the results.  It is not guaranteed that the jobs
  /// would be run as separate parallel jobs.
  val parCollect: seq<Job<'a>> -> Job<IList<'a>>

  /// Creates a job that runs all of the jobs potentially in parallel and then
  /// waits for all of the jobs to finish.  The results of the jobs are
  /// ignored.  It is not guaranteed that the jobs would be run as separate
  /// parallel jobs.
  val parIgnore: seq<Job<_>> -> Job<unit>

  ///////////////////////////////////////////////////////////////////////

  /// Creates a job that performs the asynchronous operation defined by the
  /// given pair of begin and end operations.
  val fromBeginEnd: (AsyncCallback * obj -> IAsyncResult)
                 -> (IAsyncResult -> 'x)
                 -> Job<'x>

  ///////////////////////////////////////////////////////////////////////

  /// Creates a job that sleeps for (about) the specified time.  Note that this
  /// is simply not intended for high precision timing and the resolution of
  /// the underlying timing mechanism is coarse (Windows system ticks).
  val sleep: TimeSpan -> Job<unit>

/////////////////////////////////////////////////////////////////////////

/// Operations on first-class synchronous operations or alternatives.
module Alt =
  /// Creates an alternative that is always available for picking and results in
  /// the given value.  Note that when there are alternatives immediately
  /// available for picking in a disjunction, the first such alternative will be
  /// committed to.
  val inline always: 'a -> Alt<'a>

  /// Creates an an alternative that is never available for picking.
  val never: unit -> Alt<'a>

  /// Creates an alternative that is computed at instantiation time with the
  /// given job.
  val guard: Job<Alt<'a>> -> Alt<'a>

  /// Creates an alternative that is computed at instantiation time with the
  /// given thunk.
  val delay: (unit -> Alt<'a>) -> Alt<'a>

  /// Creates an alternative that is computed at instantiation time with the
  /// given job constructed with a negative acknowledgement alternative.  The
  /// negative acknowledgement alternative will be available for picking in
  /// case some other instantiated alternative involved in the picking is
  /// committed to instead.  Note that if an alternative created with withNack
  /// is not instantiated, then no negative acknowledgement is created.  For
  /// example, given an alternative of the form "always () <|> withNack (...)"
  /// the withNack alternative is never instantiated.
  val withNack: (Alt<unit> -> Job<Alt<'a>>) -> Alt<'a>

  /// Creates an alternative that is available for picking when any one of the
  /// given alternatives is.  More precisely, "choose alts" is equivalent to
  /// "delay (fun () -> Seq.foldBack (<|>) alts never)", given Seq.foldBack
  /// with the obvious meaning.
  val choose: seq<Alt<'a>> -> Alt<'a>

  /// "select xAs" is equivalent to "pick (choose xJs)".
  val select: seq<Alt<'a>> -> Job<'a>

  ///////////////////////////////////////////////////////////////////////

  /// Infix operators on alternatives.  You can open this module to bring all
  /// of the infix operators into scope.
  module Infixes =
    /// Creates an alternative that is available for picking when either of the
    /// given alternatives is available.  The given alternatives are processed
    /// in a left-to-right order with short-cut evaluation.  In other words,
    /// given an alternative of the form "first <|> second", the "first"
    /// alternative is first instantiated and, if it is pickable, is committed
    /// to and the "second" alternative will not be instantiated at all.
    val (<|>): Alt<'a> -> Alt<'a> -> Alt<'a>

    /// Creates an alternative whose result is processed with the given
    /// function after the given alternative has been committed to.
    val (>->): Alt<'a> -> ('a -> 'b) -> Alt<'b>

    /// Creates an alternative whose result is passed to the given job
    /// constructor and processed with the resulting job after the given
    /// alternative has been committed to.  Although this operator has a type
    /// similar to a monadic bind operation, alternatives do not form a monad
    /// (with the always alternative constructor).  So called Transactional
    /// Events do form a monad, but require a more complex synchronization
    /// protocol.
    val (>=>): Alt<'a> -> ('a -> Job<'b>) -> Alt<'b>

  ///////////////////////////////////////////////////////////////////////

  /// WIP: The semantics of this primitive are not yet fully cast in stone.
  ///
  /// The semantics of the current implementation differ from the semantics
  /// of exception handling construct (wrapHandler) in John Reppy's Concurrent
  /// ML.
  val tryIn: Alt<'a>
          -> ('a -> Job<'b>)
          -> (exn -> Job<'b>)
          -> Alt<'b>

  ///////////////////////////////////////////////////////////////////////

  /// Creates a job that instantiates the alternative, waits until it becomes
  /// available for picking and then commits to the alternative and results in
  /// its value.
  val inline pick: Alt<'a> -> Job<'a>

  ///////////////////////////////////////////////////////////////////////

  /// Creates an alternative that, after instantiation, becomes pickable after
  /// the specified time span.  Note that this is simply not intended for high
  /// precision timing and the resolution of the underlying timing mechanism is
  /// very coarse (Windows system ticks).
  val timeOut: TimeSpan -> Alt<unit>

/////////////////////////////////////////////////////////////////////////

/// Operations on synchronous channels.
module Ch =
  /// Immediate or non-workflow operations on synchronous channels.
  module Now =
    /// Creates a new channel.
    val inline create: unit -> Ch<'a>

  /// Creates a job that creates a new channel.
  val create: unit -> Job<Ch<'a>>

  /// Creates a job that offers to give the given value to another job on
  /// the given channel.  A give operation is synchronous.  In other words, a
  /// give operation waits until another job takes the value.
  val inline give: Ch<'a> -> 'a -> Job<unit>

  /// Creates a job that sends a value to another job on the given channel.
  /// A send operation is asynchronous.  In other words, a send operation does
  /// not wait for another job to give the value to.  Note that channels have
  /// been optimized for synchronous operations; an occasional send can be
  /// efficient, but when sends are queued, performance maybe be significantly
  /// worse than with a Mailbox optimized for buffering.
  val inline send: Ch<'a> -> 'a -> Job<unit>

  /// Creates a job that offers to take a value from another job on the
  /// given channel.  In other words, a take operation waits until another job
  /// gives (or sends) a value.
  val inline take: Ch<'a> -> Job<'a>

  /// Selective operations on synchronous channels.
  module Alt =
    /// Creates an alternative that, at instantiation time, offers to give
    /// the given value on the given channel, and becomes available for
    /// picking when another job offers to take the value.
    val inline give: Ch<'a> -> 'a -> Alt<unit>

    /// Creates an alternative that, at instantiation time, offers to take
    /// a value from another job on the given channel, and becomes available
    /// for picking when another job offers to give a value.
    val inline take: Ch<'a> -> Alt<'a>

/////////////////////////////////////////////////////////////////////////

/// Operations on write once variables.
module IVar =
  /// Immediate or non-workflow operations on write once variables.
  module Now =
    /// Creates a new write once variable.
    val inline create: unit -> IVar<'a>

  /// Creates a job that creates a new write once variable.
  val create: unit -> Job<IVar<'a>>

  /// Creates a job that writes to the given write once variable.  It is an
  /// error to write to a single IVar more than once.  This assumption may be
  /// used to optimize the implementation and incorrect usage leads to
  /// undefined behaviour.
  val inline fill: IVar<'a> -> 'a -> Job<unit>

  /// Creates a job that, if necessary, waits until the given write once
  /// variable is written and then returns the written value.
  val inline read: IVar<'a> -> Job<'a>

  /// Selective operations on write once variables.
  module Alt =
    /// Creates an alternative that becomes available for picking after
    /// the write once variable has been written to.
    val inline read: IVar<'a> -> Alt<'a>

/////////////////////////////////////////////////////////////////////////

/// Operations on write many variables.
module MVar =
  /// Immediate or non-workflow operations on write many variables.
  module Now =
    /// Creates a new synchronous variable that is initially empty.
    val inline create: unit -> MVar<'a>

    /// Creates a new synchronous variable that initially contains the given
    /// value.
    val inline createFull: 'a -> MVar<'a>

  /// Creates a job that creates a new synchronous variable that is
  /// initially empty.
  val create: unit -> Job<MVar<'a>>

  /// Creates a job that creates a new synchronous variable that initially
  /// contains the given value.
  val createFull: 'a -> Job<MVar<'a>>

  /// Creates a job that writes the given value to the synchronous variable.
  /// It is an error to write to a MVar that is full.  This assumption may be
  /// used to optimize the implementation and incorrect usage leads to
  /// undefined behaviour.
  val inline fill: MVar<'a> -> 'a -> Job<unit>

  /// Creates a job that waits until the synchronous variable contains a
  /// value and then takes the value contained by the synchronous variable
  /// leaving the variable empty.
  val inline take: MVar<'a> -> Job<'a>

  /// Creates a job that takes the value of the variable and then fills the
  /// variable with the result of performing the given function.  Note that this
  /// operation is not atomic.  However, it is a common programming pattern to
  /// make it so that only the job that has emptied an MVar by taking a value
  /// from it is allowed to fill the MVar.  Such an access pattern makes
  /// operations on the MVar appear as atomic.
  val inline modifyFun: ('a -> 'a * 'b) -> MVar<'a> -> Job<'b>

  /// Creates a job that takes the value of the variable and then fills the
  /// variable with the result of performing the given job.  Note that this
  /// operation is not atomic.  However, it is a common programming pattern to
  /// make it so that only the job that has emptied an MVar by taking a value
  /// from it is allowed to fill the MVar.  Such an access pattern makes
  /// operations on the MVar appear as atomic.
  val inline modifyJob: ('a -> Job<'a * 'b>) -> MVar<'a> -> Job<'b>

  /// Selective operations on write many variables.
  module Alt =
    /// Creates an alternative that becomes available for picking when the
    /// variable contains a value and, if committed to, takes the value from
    /// the variable.
    val inline take: MVar<'a> -> Alt<'a>

/////////////////////////////////////////////////////////////////////////

/// Operations on buffered mailboxes.
module Mailbox =
  /// Immediate or non-workflow operations on buffered mailboxes.
  module Now =
    /// Creates a new mailbox.
    val inline create: unit -> Mailbox<'a>

  /// Creates a job that creates a new mailbox.
  val create: unit -> Job<Mailbox<'a>>

  /// Creates a job that sends the given value to the specified mailbox.  This
  /// operation never blocks.
  val inline send: Mailbox<'a> -> 'a -> Job<unit>

  /// Creates a job that waits until the specified mailbox contains at least
  /// one value and then takes a value from the mailbox and returns it.  Values
  /// are taken in FIFO order.
  val inline take: Mailbox<'a> -> Job<'a>

  /// Selective operations on buffered mailboxes.
  module Alt =
    /// Creates and alternative that becomes available for picking when the
    /// mailbox contains at least one value and, if committed to, takes a value
    /// from the mailbox.
    val inline take: Mailbox<'a> -> Alt<'a>

/////////////////////////////////////////////////////////////////////////

/// Operations on promises.
module Promise =
  /// Immediate or non-workflow operations on promises.
  module Now =
    /// Creates a promise whose value is computed lazily with the given job
    /// when an attempt is made to read the promise.  Although the job is not
    /// started immediately, the effect is that the delayed job will be run as
    /// a separate job, which means it is possible to communicate with it as
    /// long the delayed job is started before trying to communicate with it.
    val inline delay: Job<'a> -> Promise<'a>

    /// Creates a promise with the given value.
    val inline withValue: 'a -> Promise<'a>

    /// Creates a promise with the given failure exception.
    val inline withFailure: exn -> Promise<'a>

  /// Creates a job that creates a promise, whose value is computed eagerly
  /// with the given job, which is started to run as a separate parallel job.
  val start: Job<'a> -> Job<Promise<'a>>

  /// Creates a job that creates a promise, whose value is computed with the
  /// given job, when an attempt is made to read the promise.  Although the job
  /// is not started immediately, the effect is that the delayed job will be
  /// run as a separate job, which means it is possible to communicate with it
  /// as long the delayed job is started before trying to communicate with it.
  val delay: Job<'a> -> Job<Promise<'a>>

  /// Creates a job that waits for the promise to be computed and then
  /// returns its value (or fails with exception).  If the job of promise was
  /// delayed, it is first started as a separate job.
  val inline read: Promise<'a> -> Job<'a>

  /// Selective operations on promises.
  module Alt =
    /// Creates an alternative for reading the promise.  If the job of the
    /// promise was delayed, it is started as a separate job.
    val inline read: Promise<'a> -> Alt<'a>

/////////////////////////////////////////////////////////////////////////

/// Operations on mutual exclusion locks.
module Lock =
  /// Creates a job that creates a new mutual exclusion lock.
  val create: unit -> Job<Lock>

  /// Creates a job that calls the given function so that the lock is held
  /// during the execution of the function.  This locking method is optimized
  /// for short duration locking so that other jobs contending for the lock
  /// will keep spinning during the execution of the function.
  val inline duringFun: Lock -> (unit -> 'a) -> Job<'a>

  /// Creates a job that runs the given job so that the lock is held during
  /// the execution of the given job.  This locking method is optimized for
  /// longer duration locking so that other jobs contending for the lock will
  /// be blocked to a queue during the execution of the function.  Note that
  /// blocking and resuming a job is much faster than blocking and resuming a
  /// native thread.
  val inline duringJob: Lock -> Job<'a> -> Job<'a>

  /// Immediate or non-workflow operations on locks.
  module Now =
    /// Creates a new lock.
    val inline create: unit -> Lock

/////////////////////////////////////////////////////////////////////////
#if NOT_YET_IMPLEMENTED
/// Operations on condition variables.
module Cond =
  val create: Lock -> (unit -> bool) -> Job<Cond>
  val wait: Cond -> Job<unit>
  val signal: Cond -> Job<unit>
  module Now =
    val create: unit -> Cond
#endif

/////////////////////////////////////////////////////////////////////////

/// Extensions to various system modules and types for programming with
/// parallel jobs.  You can open this module to use the extensions much
/// like as if they were part of the existing modules and types.
module Extensions =
  /// Operations for processing arrays with parallel jobs.
  module Array =
    /// Sequentially maps the given job constructor to the elements of the
    /// array and returns an array of the results.
    val mapJob: ('a -> Job<'b>) -> array<'a> -> Job<array<'b>>

    /// Sequentially iterates the given job constructor over the given
    /// array.  The results, if any, of the jobs are ignored.
    val iterJob: ('a -> Job<_>) -> array<'a> -> Job<unit>

  /// Operations for processing sequences with parallel jobs.
  module Seq =
    /// Sequentially iterates the given job constructor over the given
    /// sequence.  The results, if any, of the jobs are ignored.
    val iterJob: ('a -> Job<_>) -> seq<'a> -> Job<unit>

    /// Sequentially maps the given job constructor to the elements of the
    /// sequence and returns a sequence of the results.
    val mapJob: ('a -> Job<'b>) -> seq<'a> -> Job<IList<'b>>

    /// Sequentially folds the job constructor over the given sequence and
    /// returns the result of the fold.
    val foldJob: ('a -> 'b -> Job<'a>) -> 'a -> seq<'b> -> Job<'a>

    /// Operations for processing sequences in parallel using Hopac jobs.
    module Parallel =
      /// Iterates the given job constructor over the given sequence, runs the
      /// constructed jobs potentially in parallel and waits until all of the
      /// jobs have finished.  Note that the results of the created jobs are
      /// ignored.  It is also not guaranteed that the jobs would be run as
      /// separate parallel jobs.
      val iterJob: ('a -> Job<_>) -> seq<'a> -> Job<unit>

      /// Iterates the given job constructor over the given sequence, runs the
      /// constructed jobs potentially in parallel and waits until all of the
      /// jobs have finished collecting the results into a new sequence.  It is
      /// not guaranteed that the jobs would be run as separate parallel jobs.
      val mapJob: ('a -> Job<'b>) -> seq<'a> -> Job<IList<'b>>

  /// Operations for interfacing tasks with parallel jobs.
  type [<Sealed>] Task =
    /// Creates a job that waits for the given task to finish and then returns
    /// the result of the task.  Note that this does not start the job.
    static member inline awaitJob: Threading.Tasks.Task<'x> -> Job<'x>

    /// Creates a job that waits until the given task finishes.  Note that this
    /// does not start the job.
    static member inline awaitJob: Threading.Tasks.Task -> Job<unit>
