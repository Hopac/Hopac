#### 0.0.0.45 - 10.04.2015
* Added basic `Stream.buffer` combinator.
* Renamed `Stream.subscribe*` to `Stream.consume*` to better describe semantics.
* Doc refinements.
* Added `Stream.pullOn`, `Stream.skipWhileJob`, `Stream.skipWhileFun`, `Stream.pullOn`, `Stream.foldBack`, `Stream.mapIgnore`, 
`Stream.ambAll`, `Stream.appendAll`, `Stream.mergeAll` and `Stream.switchAll`
* Fixed to start reading the serialized variable immediately as documented.
* Added `Stream.subscribe*` as a shorthand for `Stream.iter* |> queue`.
* Added mutable `Stream.Property<'x>` that generates property change notifications for e.g. WPF data binding.
* Added `doFinalizeJob` and `doFinalizeFun`.
* Experimental wrapper for streams that tracks space safety via phantom types.
* Changed `groupByJob` and `groupByFun` to take an additional function/job for forming new groups.
* Added `Stream.tailsMapFun` and `Stream.initsMapFun` these are useful for lifting the `Stream.tails` and `Stream.inits` functions.
* Added `keepFollowing1` and renamed `keepLatest` to `keepPreceding` to make the naming more symmetric although the concepts of "following" and "preceding" aren't really fully symmetric (unless you allow time travel).
* A slightly more performant implementation of `keepLatestFuns`.
* Renamed from `lazify` to `keepLatest`.
* Lazification of live streams.
* Rethinking timing and throttling operations.
* Generalized `joinWith` and `mapJoin`.
* There is no sleep, only `timeOut`.
* Added ability to directly bind observables in job computation expressions.
* Removed superfluous methods from `Async.OnWithSchedulerBuilder`.

#### 0.0.0.44 - 02.03.2015
* Fixed to properly remove waiter in case it timed out rather than was signalled.
* Signal when top timed changes to improve timer accuracy.
* Add a test of the job computation builder.
* Remove redundant builder methods.
* Added `Job.tryFinallyFunDelay`, `Job.tryFinallyJobDelay`, `Job.tryWithDelay` and `Job.whileDoDelay`.
* Added `Stream.mapConst`.
* Removed `Stream.subscribeOnFirst`, `Stream.subscribeDuring` and `Stream.subscribingTo`, because `Stream.ofObservable` easily covers all of those use cases. 

#### 0.0.0.43 - 28.02.2015
* Scale work sharing by the number of cores.

#### 0.0.0.42 - 27.02.2015
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
* Added experimental support for running async comptations on the main synchronization context, which must be explicitly configured by application code.
* Added `IObservable<'x>.onceAltOn` extension method for conveniently interfacing Hopac with suitable observables.
* Added `TopLevel.startDelay` and `TopLevel.queueDelay` for convenience.
* Renamed `Builder.Join` to `Builder.Plus` and recognized that `Zero´ must also be abstract.

#### 0.0.0.41 - 11.02.2015
* Make sure `StaticData` is initialized, fix for #52.
* Don't capture context, fix for #53.
* Added `Promise.Now.never`.
* Optimized anonymous class initialization patterns: small time and space improvement across the board.
* Another MissingMethodException workaround.

#### 0.0.0.40 - 06.02.2015
* Changed the priority queue to a simple leftist heap and added some extra logic to purge abandoned timeouts while merging. See #50.
* Added `TopLevel.memo` as an alias for `Promise.Now.delay`.
* Added `Stream.takeAndSkipUntil`.
* Disable inlining of `Latch.Now.create` as an attempt to work around a `MissingMethodException`.
* Fixing `combineLatest` to properly discard unmatched elements.
* Add more reference implementations to the docs.
* Renamed `Stream.switchOn` to `Stream.switchTo`.
* Make `Job.lift` inlineable.

#### 0.0.0.39 - 01.02.2015
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

#### 0.0.0.38 - 24.01.2015
* Move Streams, BoundedMb and some functions form Hopac.Extra library to Hopac.
* Documenting and refining choice streams.
* Put `Streams` under `TopLevel`.
* Expose `Stream<'x>` at `Hopac` namespace level.
* Experimental streams builder.
* Refining streams.
* Lazy memoizing promise operators.
* Fixed missing init in IVar and Nack.

#### 0.0.0.37 - 21.01.2015
* Merge Hopac.Extra package.
* Renamed `Streams.finallyFun` to `Streams.onCloseFun` to emphasize semantic differences.  
* Added `Streams.subscribeDuring`.
* Added `Streams.subscribe` to easily convert observables to streams.
* Added `Streams.finallyJob`.
* Added `Job.finallyFun`.

#### 0.0.0.36 - 20.01.2015
* Added `sleepMillis` and `timeOutMillis`.
* Flexible typing.
* `select` and `pick` are not necessary.
* Added `Streams.scanFrom*` and `Streams.foldFrom*`.
* Reduced the synchronization properties of StreamVar and StreamSrc.
* Added `Alt.raises` and `Streams.error`.
* Added experimental `IAsyncDisposable` interface and associated `Job.usingAsync` combinator.
* Added `Streams.tails`.
* Added `Alt.Ignore`.
* Generalized `MVar.modifyFun` and `MVar.modifyJob` to alternatives.

#### 0.0.0.35 - 19.12.2014
* Fixed to refer to Xamarin.iOS rather than MonoTouch.
	
#### 0.0.0.34 - 19.12.2014
* Removed ThreadPool and WaitHandle extension methods, because they are not supported on PCL profiles.
* Removed Scheduler ThreadPriority option, because ThreadPriority is not supported on PCL profiles.
* Reorganized to PCL and platform libraries.

#### 0.0.0.33 - 11.12.2014
* Added Async.toAltOn, Async.toAlt.
* Added Alt.tryFinallyFun, Alt.tryFinallyJob.

#### 0.0.0.32 - 02.12.2014
* Attempt to work around an inlining issue with Alt.never calls.

#### 0.0.0.31 - 25.11.2014
* Fixed bug in delayed promises as selective operations.

#### 0.0.0.30 - 19.11.2014
* Fixed a couple of (exception handling) cases where nacks were not triggered correctly.
* Added non-operator versions of bind, map and wrap for convenience.
* Reintroduced lazy promises.
* Enhanced timeOut to work with zero and infinite time spans.
* Added some more TopLevel combinators for convenience.

#### 0.0.0.29 - 29.09.2014
* Print warning on Mono when not using SGen.

#### 0.0.0.28 - 29.09.2014
* MonoAndroid

#### 0.0.0.27 - 28.09.2014
* MonoTouch

#### 0.0.0.26 - 22.09.2014 
* Fixed not to rely on tail calls on Mono.

#### 0.0.0.25 - 20.09.2014
* Minor tweaks to make Hopac work more nicely on Mono 3.6.0+.

#### 0.0.0.24 - 27.07.2014 
* Switched to .Net framework 4.5 (was 4.5.1).
* Removed For array overload to avoid typing issue.  Array.iterJob should now be used in performance critical cases.
* Added experimental Async &lt;-&gt; Job interop support.
* Renamed &lt;|&gt; to &lt;|&gt;? and added &lt;|&gt; with result type restricted to Job.
* IVar is now inherited from Promise and both now have low level polling ops.

#### 0.0.0.23 - 22.07.2014 
* Fixed bug in Ch.Try.give introduced in previous version.</releaseNotes>