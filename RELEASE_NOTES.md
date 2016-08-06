#### 0.3.5 - 2016-08-06
* Faster `Job.fromAsync`.

#### 0.3.4 - 2016-08-05
* Fixed `FSharp.Core ~> 3` dependency once again. :(

#### 0.3.3 - 2016-08-05
* Added `Hopac.startAsTask` and `Hopac.queueAsTask` for interop with tasks.

#### 0.3.2 - 2016-08-04
* Added `Job.toAsync` and marked `Async.ofJobOn` and `Async.Global.ofJob` as
  obsolete.
* Added `Alt.toAsync`.
* Renamed the `TopLevel` module to `Hopac`.  This rename is part of eliminating
  the scheduler concept and was made so that one can refer to e.g `Hopac.run`
  making it obvious where the functionality is coming from.
* Marked the `Job.Global` module as obsolete.  The `Hopac` module is the new
  home for the same functionality.
* Marked `Job.startWithFinalizer` as obsolete.  Use the `Proc` abstraction
  instead.
* Marked the `Timer` module for removal.  Use the `Hopac` module.
* Moved `Ch.Global.send -> Ch.Now.send` and `Mailbox.Global.send ->
  Mailbox.Now.send`.  This is part of eliminating the scheduler concept.
* Marked `*.create` functions obsolete where a constructor will do the job.
  Originally constructors were treated quite differently from other functions in
  F#.  Today the differences are smaller and not enough to justify duplication.

#### 0.3.1 - 2016-07-30
* Marked `Async.toJobOn` and `Async.toAltOn` as obsolete.

#### 0.3.0 - 2016-07-28
* Introduced new optimized primitives for `Task`, `Async` and `Begin-End`
  interop (see reference), obsoleting interop primitives `Async.toJob`,
  `Async.toAlt`, `Task.awaitJob`, and `Task.bindJob`.
* Deprecated `JobBuilder.Bind` and `JobBuilder.ReturnFrom` overloads with
  non-generic `Task` as argument, because they cause type-inference problems.

#### 0.2.1 - 2016-06-05
* Improved structure and formatting of the reference manual.
* Changed internal stack limit setting mechanism for trampolining.
* Potentially breaking change: `Stream.Builder.Plus` method was renamed into
  `Stream.Builder.Combine`.
* Added `Stream.tryPickJob` and `Stream.tryPickFun`.
* Fixed `Job.using` to not raise in case given disposable reference is `null`.

#### 0.2.0 - 2016-06-01
* Dropped obsolete definitions.
* Exception handling fixes to `<*>`.
* Allow null in `Job.using` for the disposable.
* Fixed `Stream.Src` ops to be atomic.
* Added various missing operators for symmetry.
* Added `IVar.tryFillFailure`.

#### 0.1.4 - 2016-05-09
* New package Hopac-StrongName for those requiring it.

#### 0.1.3 - 2015-12-15
* Added `*<+=>-` and `*<+=>=` for async calls.
* Reverted `FSharp.Core` dependency.

#### 0.1.2 - 2015-11-27
* Fixed lack of tail call workaround in `for` and `while`.

#### 0.1.1 - 2015-11-23
* XML documentation was missing from `0.1.0`.
* Updated `FSharp.Core` dependency to `4.0.0.1`.
* Redesigned infix operator symbols to allow most common expressions to be
  written without parentheses and also added several new infix operators.  See
  the documentation for details.
* Moved all infix operators to the `Hopac.Infixes` module so that one only has
  to open one module to use infix operators.
* Introduced `Alt.prepare*` and `Alt.after*` combinators for clarity and
  uniformity to replace `delay`, `guard` and `wrap`.
* Added `Job.isolate` as a workaround for scheduling issues.
* Minor additions: `Seq.foldFromJob`, `Proc.bind`, `Proc.map`.

#### 0.1.0 - 2015-11-23
* Redesigned infix operator symbols to allow most common expressions to be
  written without parentheses and also added several new infix operators.  See
  the documentation for details.
* Moved all infix operators to the `Hopac.Infixes` module so that one only has
  to open one module to use infix operators.
* Introduced `Alt.prepare*` and `Alt.after*` combinators for clarity and
  uniformity to replace `delay`, `guard` and `wrap`.
* Added `Job.isolate` as a workaround for scheduling issues.
* Minor additions: `Seq.foldFromJob`, `Proc.bind`, `Proc.map`.

#### 0.0.0.48 - 2015-07-15
* Fixed a bug in exception propagation of delayed promises.

#### 0.0.0.47 - 2015-05-27
* Fixed a bug in counting the number of active worker threads, which is only
  used by `Scheduler.wait`.

#### 0.0.0.46 - 2015-05-24
* Changed to use `FSharp.Core` from NuGet.
* Removed PCL profiles.
* Fixed potential leak with `Job.tryIn` and `Job.tryInDelay`.

#### 0.0.0.45 - 2015-04-10
* Added basic `Stream.buffer` combinator.
* Renamed `Stream.subscribe*` to `Stream.consume*` to better describe semantics.
* Doc refinements.
* Added `Stream.pullOn`, `Stream.skipWhileJob`, `Stream.skipWhileFun`,
  `Stream.pullOn`, `Stream.foldBack`, `Stream.mapIgnore`, `Stream.ambAll`,
  `Stream.appendAll`, `Stream.mergeAll` and `Stream.switchAll`
* Fixed to start reading the serialized variable immediately as documented.
* Added `Stream.subscribe*` as a shorthand for `Stream.iter* |> queue`.
* Added mutable `Stream.Property<'x>` that generates property change
  notifications for e.g. WPF data binding.
* Added `doFinalizeJob` and `doFinalizeFun`.
* Experimental wrapper for streams that tracks space safety via phantom types.
* Changed `groupByJob` and `groupByFun` to take an additional function/job for
  forming new groups.
* Added `Stream.tailsMapFun` and `Stream.initsMapFun` these are useful for
  lifting the `Stream.tails` and `Stream.inits` functions.
* Added `keepFollowing1` and renamed `keepLatest` to `keepPreceding` to make the
  naming more symmetric although the concepts of "following" and "preceding"
  aren't really fully symmetric (unless you allow time travel).
* A slightly more performant implementation of `keepLatestFuns`.
* Renamed from `lazify` to `keepLatest`.
* Lazification of live streams.
* Rethinking timing and throttling operations.
* Generalized `joinWith` and `mapJoin`.
* There is no sleep, only `timeOut`.
* Added ability to directly bind observables in job computation expressions.
* Removed superfluous methods from `Async.OnWithSchedulerBuilder`.

#### 0.0.0.44 - 2015-03-02
* Fixed to properly remove waiter in case it timed out rather than was
  signalled.
* Signal when top timed changes to improve timer accuracy.
* Add a test of the job computation builder.
* Remove redundant builder methods.
* Added `Job.tryFinallyFunDelay`, `Job.tryFinallyJobDelay`, `Job.tryWithDelay`
  and `Job.whileDoDelay`.
* Added `Stream.mapConst`.
* Removed `Stream.subscribeOnFirst`, `Stream.subscribeDuring` and
  `Stream.subscribingTo`, because `Stream.ofObservable` easily covers all of
  those use cases.

#### 0.0.0.43 - 2015-02-28
* Scale work sharing by the number of cores.

#### 0.0.0.42 - 2015-02-27
* Distribute work more eagerly.
* Name worker threads.
* Added monadic composition operator `>=>` for jobs.
* Updated documentation.
* Tuning streams.
* Tweaked Promisesc.
* Starting to benchmark streams.
* Avoid an allocation when choosing over lazy promises.
* Added `Stream.ofObservableOnMain`.
* Added `Async.getMain`.
* Added `Stream.ofObservableOn` and `Stream.ofObservable`.
* Added `IObservable<_>.onceAlt`.
* Renamed `withNack` as `withNackJob` and `wrapAbort` as `wrapAbortJob`.
* Added `Alt.wrapAbortFun`.
* Added `Alt.choosy`, which is an optimized version of `Alt.choose` for arrays.
* Added `Job.Scheduler.bind` for wrapping external asynchronous events.
* Added `Alt.withNackFun`.
* Inlineable `withNack` avoids closure allocation.
* Added experimental support for running async comptations on the main
  synchronization context, which must be explicitly configured by application
  code.
* Added `IObservable<'x>.onceAltOn` extension method for conveniently
  interfacing Hopac with suitable observables.
* Added `TopLevel.startDelay` and `TopLevel.queueDelay` for convenience.
* Renamed `Builder.Join` to `Builder.Plus` and recognized that `Zero´ must also
  be abstract.

#### 0.0.0.41 - 2015-02-11
* Make sure `StaticData` is initialized, fix for #52.
* Don't capture context, fix for #53.
* Added `Promise.Now.never`.
* Optimized anonymous class initialization patterns: small time and space
  improvement across the board.
* Another MissingMethodException workaround.

#### 0.0.0.40 - 2015-02-06
* Changed the priority queue to a simple leftist heap and added some extra logic
  to purge abandoned timeouts while merging. See #50.
* Added `TopLevel.memo` as an alias for `Promise.Now.delay`.
* Added `Stream.takeAndSkipUntil`.
* Disable inlining of `Latch.Now.create` as an attempt to work around a
  `MissingMethodException`.
* Fixing `combineLatest` to properly discard unmatched elements.
* Add more reference implementations to the docs.
* Renamed `Stream.switchOn` to `Stream.switchTo`.
* Make `Job.lift` inlineable.

#### 0.0.0.39 - 2015-02-01
* Added internal class for `for` -style jobs.
* Added `Job.tryInDelay`.
* Added `distinctUntilChanged`.
* Extended `groupByFun` and `groupByJob` with ability to close substreams.
* Added plain `iter`.
* Tests for streams.
* Shift is tricky to implement correctly.
* Documenting and refining streams.
* `sleep` doesn't exist anymore.
* Note about timeouts and liveness.

#### 0.0.0.38 - 2015-01-24
* Move Streams, BoundedMb and some functions form Hopac.Extra library to Hopac.
* Documenting and refining choice streams.
* Put `Streams` under `TopLevel`.
* Expose `Stream<'x>` at `Hopac` namespace level.
* Experimental streams builder.
* Refining streams.
* Lazy memoizing promise operators.
* Fixed missing init in IVar and Nack.

#### 0.0.0.37 - 2015-01-21
* Merge Hopac.Extra package.
* Renamed `Streams.finallyFun` to `Streams.onCloseFun` to emphasize semantic
  differences.
* Added `Streams.subscribeDuring`.
* Added `Streams.subscribe` to easily convert observables to streams.
* Added `Streams.finallyJob`.
* Added `Job.finallyFun`.

#### 0.0.0.36 - 2015-01-20
* Added `sleepMillis` and `timeOutMillis`.
* Flexible typing.
* `select` and `pick` are not necessary.
* Added `Streams.scanFrom*` and `Streams.foldFrom*`.
* Reduced the synchronization properties of `StreamVar` and `StreamSrc`.
* Added `Alt.raises` and `Streams.error`.
* Added experimental `IAsyncDisposable` interface and associated
  `Job.usingAsync` combinator.
* Added `Streams.tails`.
* Added `Alt.Ignore`.
* Generalized `MVar.modifyFun` and `MVar.modifyJob` to alternatives.

#### 0.0.0.35 - 2014-12-19
* Fixed to refer to Xamarin.iOS rather than MonoTouch.

#### 0.0.0.34 - 2014-12-19
* Removed ThreadPool and WaitHandle extension methods, because they are not
  supported on PCL profiles.
* Removed Scheduler ThreadPriority option, because ThreadPriority is not
  supported on PCL profiles.
* Reorganized to PCL and platform libraries.

#### 0.0.0.33 - 2014-12-11
* Added Async.toAltOn, Async.toAlt.
* Added Alt.tryFinallyFun, Alt.tryFinallyJob.

#### 0.0.0.32 - 2014-12-02
* Attempt to work around an inlining issue with Alt.never calls.

#### 0.0.0.31 - 2014-11-25
* Fixed bug in delayed promises as selective operations.

#### 0.0.0.30 - 2014-11-19
* Fixed a couple of (exception handling) cases where nacks were not triggered
  correctly.
* Added non-operator versions of bind, map and wrap for convenience.
* Reintroduced lazy promises.
* Enhanced timeOut to work with zero and infinite time spans.
* Added some more TopLevel combinators for convenience.

#### 0.0.0.29 - 2014-09-29
* Print warning on Mono when not using SGen.

#### 0.0.0.28 - 2014-09-29
* MonoAndroid

#### 0.0.0.27 - 2014-09-28
* MonoTouch

#### 0.0.0.26 - 2014-09-22
* Fixed not to rely on tail calls on Mono.

#### 0.0.0.25 - 2014-09-20
* Minor tweaks to make Hopac work more nicely on Mono 3.6.0+.

#### 0.0.0.24 - 2014-07-27
* Switched to .Net framework 4.5 (was 4.5.1).
* Removed For array overload to avoid typing issue.  `Array.iterJob` should now
  be used in performance critical cases.
* Added experimental Async <-> Job interop support.
* Renamed `<|>` to `<|>?` and added `<|>` with result type restricted to `Job`.
* IVar is now inherited from Promise and both now have low level polling ops.

#### 0.0.0.23 - 2014-07-22
* Fixed bug in `Ch.Try.give` introduced in previous version.
