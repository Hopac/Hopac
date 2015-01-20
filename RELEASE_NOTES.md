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