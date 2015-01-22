// Copyright (C) by Vesa Karvonen

namespace Hopac

open System

/// Operations on choice streams.
module Stream =
  /// Represents a point in a non-deterministic stream of values.
  type Cons<'x> =
    /// Communicates the end of the stream.
    | Nil
    /// Communicates a value and the remainder of the stream.
    | Cons of Value: 'x * Next: Alt<Cons<'x>>

  /// Represents a non-deterministic stream of values called a choice stream.
  type Stream<'x> = Alt<Cons<'x>>

  /// Represents an imperative source of a stream of values called a stream
  /// source.
  type Src<'x>

  /// Operations on stream sources.
  module Src =
    /// Creates a new stream source.
    val create: unit -> Src<'x>

    /// Appends a new value to the end of the generated stream.  This operation
    /// is atomic and can be safely used from multiple parallel jobs.
    val value: Src<'x> -> 'x -> Job<unit>

    /// Terminates the stream with an error.  The given exception is raised in
    /// the consumers of the stream if and when they reach the end of the
    /// stream.
    val error: Src<'x> -> exn -> Job<unit>

    /// Terminates the stream.
    val close: Src<'x> -> Job<unit>

    /// Returns the remainder of the generated stream after the point in time
    /// when `tap` is called.
    val tap: Src<'x> -> Stream<'x>

  /// Represents a mutable variable, called a stream variable, that generates a
  /// stream of values as a side-effect.
  type Var<'x>

  /// Operations on stream variables.
  module Var =
    /// Creates a new stream variable.
    val create: 'x -> Var<'x>

    /// Gets the value of the variable.
    val get: Var<'x> -> 'x

    /// Sets the value of the variable and appends the value to the end of the
    /// generated stream.  Note that while this operation is atomic, and can be
    /// safely used from multiple parallel jobs, a combination of `get` and
    /// `set` is not atomic.
    val set: Var<'x> -> 'x -> Job<unit>

    /// Returns the generated stream, including the current value of the
    /// variable, from the point in time when `tap` is called.
    val tap: Var<'x> -> Stream<'x>

  // Introducing streams

  /// A choice stream that never produces any values and never closes.
  val inline never<'x> : Stream<'x>

  /// Constructs a choice stream that is closed with an error.
  val inline error: exn -> Stream<'x>

  /// An empty or closed choice stream.
  val inline nil<'x> : Stream<'x>

  /// `cons x xs` constructs a choice stream whose first value is `x` and the
  /// rest of the stream is like `xs`.
#if DOC
  ///
  /// Node that `cons` and `nil` directly correspond to the ordinary list
  /// constructors `::` and `[]` and you can construct choice streams just like
  /// you would create ordinary lists.
#endif
  val cons: 'x -> Stream<'x> -> Stream<'x>

  /// `one x` is equivalent to `cons x nil`.
  val inline one: 'x -> Stream<'x>

  /// Preliminary and subject to change.
  val ofSeq: seq<'x> -> Stream<'x>

  /// Preliminary and subject to change.
  val foreverJob: Job<'x> -> Stream<'x>
  /// Preliminary and subject to change.
  val onceJob: Job<'x> -> Stream<'x>

  /// Preliminary and subject to change.
  val repeatJobs: Job<'x> -> Job<_> -> Stream<'x>

  /// Preliminary and subject to change.
  val unfoldJob: ('s -> #Job<option<'x * 's>>) -> 's -> Stream<'x>
  /// Preliminary and subject to change.
  val unfoldFun: ('s -> option<'x * 's>) -> 's -> Stream<'x>

  /// Preliminary and subject to change.
  val iterateJob: ('x -> #Job<'x>) -> 'x -> Stream<'x>
  /// Preliminary and subject to change.
  val iterateFun: ('x -> 'x) -> 'x -> Stream<'x>

  // Observable

  /// Creates a stream that subscribes to the observable when the first element
  /// of the stream is requested.  Conversely, if no elements are requested from
  /// the returned stream, no subscribe action is performed.  There is no way to
  /// explicitly unsubscribe.  To limit the subscription, you need to compose
  /// the observable in such a way that it is closed at the point when it needs
  /// to be unsubscribed.  See also: `subscribeDuring`.
#if DOC
  ///
  /// Note that to subscribe immediately, you can start the evaluation of the
  /// returned stream.  You can write, for example,
  ///
  ///> xObs
  ///> |> subscribeOnFirst
  ///> |>! startIgnore
  ///> |> ...
  ///
  /// where `|>!` is the function
  ///
  ///> let (|>!) x f = f x ; x
#endif
  val subscribeOnFirst: IObservable<'x> -> Stream<'x>

  /// Creates a stream, using the given function, that subscribes to the
  /// observable when the first element of the stream is requested and
  /// unsubscribes from the observable when the returned stream closes.  See
  /// also: `subscribeOnFirst`.
#if DOC
  ///
  /// For example,
  ///
  ///> xObs
  ///> |> subscribeDuring (fun xs ->
  ///>    xs
  ///>    |> take 1)
  ///> |> ...
  ///
  /// creates a stream that subscribes to the observable, takes (and produces)
  /// at most one element from the observable, and then unsubscribes from the
  /// observable and closes.
#endif
  val subscribeDuring: (Stream<'x> -> Stream<'y>) -> IObservable<'x> -> Stream<'y>

  /// Preliminary and subject to change.
  val subscribingTo: IObservable<'x> -> (Stream<'x> -> #Job<'y>) -> Job<'y>

  /// Preliminary and subject to change.
  val toObservable: Stream<'x> -> IObservable<'x>

  // Sequence combinators

  /// Preliminary and subject to change.
  val chooseJob: ('x -> #Job<option<'y>>) -> Stream<'x> -> Stream<'y>
  /// Preliminary and subject to change.
  val chooseFun: ('x -> option<'y>) -> Stream<'x> -> Stream<'y>
  /// Preliminary and subject to change.
  val choose: Stream<option<'x>> -> Stream<'x>

  /// Preliminary and subject to change.
  val filterJob: ('x -> #Job<bool>) -> Stream<'x> -> Stream<'x>
  /// Preliminary and subject to change.
  val filterFun: ('x -> bool) -> Stream<'x> -> Stream<'x>

  /// Preliminary and subject to change.
  val mapJob: ('x -> #Job<'y>) -> Stream<'x> -> Stream<'y>
  /// Preliminary and subject to change.
  val mapFun: ('x -> 'y) -> Stream<'x> -> Stream<'y>

  /// Preliminary and subject to change.
  val groupByJob: ('x -> #Job<'k>) -> Stream<'x> -> Stream<'k * Stream<'x>> when 'k: equality
  /// Preliminary and subject to change.
  val groupByFun: ('x -> 'k) -> Stream<'x> -> Stream<'k * Stream<'x>> when 'k: equality

  /// Preliminary and subject to change.
  val zip: Stream<'x> -> Stream<'y> -> Stream<'x * 'y>

  /// Preliminary and subject to change.
  val scanJob: ('s -> 'x -> #Job<'s>) -> 's -> Stream<'x> -> Stream<'s>
  /// Preliminary and subject to change.
  val scanFun: ('s -> 'x -> 's) -> 's -> Stream<'x> -> Stream<'s>

  /// Preliminary and subject to change.
  val scanFromJob: 's -> ('s -> 'x -> #Job<'s>) -> Stream<'x> -> Stream<'s>
  /// Preliminary and subject to change.
  val scanFromFun: 's -> ('s -> 'x -> 's) -> Stream<'x> -> Stream<'s>

  /// Preliminary and subject to change.
  val distinctByJob: ('x -> #Job<'k>) -> Stream<'x> -> Stream<'x> when 'k: equality
  /// Preliminary and subject to change.
  val distinctByFun: ('x -> 'k) -> Stream<'x> -> Stream<'x> when 'k: equality

  /// Preliminary and subject to change.
  val distinctUntilChangedWithJob: ('x -> 'x -> #Job<bool>) -> Stream<'x> -> Stream<'x>
  /// Preliminary and subject to change.
  val distinctUntilChangedWithFun: ('x -> 'x -> bool) -> Stream<'x> -> Stream<'x>

  /// Preliminary and subject to change.
  val distinctUntilChangedByJob: ('x -> #Job<'k>) -> Stream<'x> -> Stream<'x> when 'k: equality
  /// Preliminary and subject to change.
  val distinctUntilChangedByFun: ('x -> 'k) -> Stream<'x> -> Stream<'x> when 'k: equality

  // Joining streams

  /// Preliminary and subject to change.
  val amb: Stream<'x> -> Stream<'x> -> Stream<'x>
  /// Preliminary and subject to change.
  val merge: Stream<'x> -> Stream<'x> -> Stream<'x>
  /// Preliminary and subject to change.
  val append: Stream<'x> -> Stream<'x> -> Stream<'x>
  /// Preliminary and subject to change.
  val switch: Stream<'x> -> Stream<'x> -> Stream<'x>

  /// Preliminary and subject to change.
  val joinWith: (Stream<'x> -> Stream<'y> -> Stream<'y>) -> Stream<Stream<'x>> -> Stream<'y>

  /// Preliminary and subject to change.
  val mapJoin: (Stream<'y> -> Stream<'z> -> Stream<'z>) -> ('x -> Stream<'y>) -> Stream<'x> -> Stream<'z>

  /// Preliminary and subject to change.
  val ambMap: ('x -> Stream<'y>) -> Stream<'x> -> Stream<'y>
  /// Preliminary and subject to change.
  val mergeMap: ('x -> Stream<'y>) -> Stream<'x> -> Stream<'y>
  /// Preliminary and subject to change.
  val appendMap: ('x -> Stream<'y>) -> Stream<'x> -> Stream<'y>
  /// Preliminary and subject to change.
  val switchMap: ('x -> Stream<'y>) -> Stream<'x> -> Stream<'y>

  // Skipping and taking

  /// Preliminary and subject to change.
  val skip: int -> Stream<'x> -> Stream<'x>
  /// Preliminary and subject to change.
  val take: int -> Stream<'x> -> Stream<'x>

  /// Preliminary and subject to change.
  val skipUntil: Alt<_> -> Stream<'x> -> Stream<'x>
  /// Preliminary and subject to change.
  val takeUntil: Alt<_> -> Stream<'x> -> Stream<'x>

  /// Preliminary and subject to change.
  val switchOn: Stream<'x> -> Stream<'x> -> Stream<'x>

  // Exceptions

  /// Preliminary and subject to change.
  val catchOnce: (exn -> Stream<'x>) -> Stream<'x> -> Stream<'x>
  /// Preliminary and subject to change.
  val catch: (exn -> Stream<'x>) -> Stream<'x> -> Stream<'x>

  /// Returns a stream that is just like the given stream except that just
  /// before the returned stream is closed, due to the given stream being
  /// closed, whether with an error or without, the given job is executed.  In
  /// case the job raises an exception, that exception closes the returned
  /// stream.
  val onCloseJob: Job<unit> -> Stream<'x> -> Stream<'x>

  /// `xs |> onCloseFun u2u` is equivalent to `xs |> onCloseJob (Job.thunk
  /// u2u)`.
  val onCloseFun: (unit -> unit) -> Stream<'x> -> Stream<'x>

  // Timing

  /// Preliminary and subject to change.
  val sample: ticks: Stream<_> -> Stream<'x> -> Stream<'x>

  /// Preliminary and subject to change.
  val throttle: timeout: Alt<_> -> Stream<'x> -> Stream<'x>

  /// Preliminary and subject to change.
  val hold: timeout: Job<_> -> Stream<'x> -> Stream<'x>

  /// Preliminary and subject to change.
  val combineLatest: Stream<'x> -> Stream<'y> -> Stream<'x * 'y>

  /// Preliminary and subject to change.
  val delayEachBy: Job<_> -> Stream<'x> -> Stream<'x>

  /// Preliminary and subject to change.
  val atDateTimeOffsets: Stream<DateTimeOffset> -> Stream<DateTimeOffset>
  /// Preliminary and subject to change.
  val atDateTimeOffset: DateTimeOffset -> Stream<DateTimeOffset>

  /// Preliminary and subject to change.
  val afterTimeSpan: TimeSpan -> Stream<unit>

  // Eliminating streams

  /// Preliminary and subject to change.
  val toSeq: Stream<'x> -> Job<ResizeArray<'x>>

  /// Preliminary and subject to change.
  val values: Stream<'x> -> Job<Alt<'x>>

  /// Preliminary and subject to change.
  val foldJob: ('s -> 'x -> #Job<'s>) -> 's -> Stream<'x> -> Job<'s>
  /// Preliminary and subject to change.
  val foldFun: ('s -> 'x -> 's) -> 's -> Stream<'x> -> Job<'s>

  /// Preliminary and subject to change.
  val foldFromJob: 's -> ('s -> 'x -> #Job<'s>) -> Stream<'x> -> Job<'s>
  /// Preliminary and subject to change.
  val foldFromFun: 's -> ('s -> 'x -> 's) -> Stream<'x> -> Job<'s>

  /// Preliminary and subject to change.
  val iterJob: ('x -> #Job<unit>) -> Stream<'x> -> Job<unit>
  /// Preliminary and subject to change.
  val iterFun: ('x -> unit) -> Stream<'x> -> Job<unit>

  /// Preliminary and subject to change.
  val count: Stream<'x> -> Alt<int>

  /// Preliminary and subject to change.
  val head: Stream<'x> -> Alt<'x>
  /// Preliminary and subject to change.
  val tail: Stream<'x> -> Stream<'x>

  /// Preliminary and subject to change.
  val tails: Stream<'x> -> Stream<Stream<'x>>

  /// Preliminary and subject to change.
  val last: Stream<'x> -> Alt<'x>

  /// Preliminary and subject to change.
  val single: Stream<'x> -> Alt<'x>

  /// Preliminary and subject to change.
  type Builder =
    new: unit -> Builder
    member inline Bind: Stream<'x> * ('x -> Stream<'y>) -> Stream<'y>
    member inline Combine: Stream<'x> * Stream<'x> -> Stream<'x>
    member inline Delay: (unit -> Stream<'x>) -> Stream<'x>
    member inline Zero: unit -> Stream<'x>
    member inline For: seq<'x> * ('x -> Stream<'y>) -> Stream<'y>
    member inline TryWith: Stream<'x> * (exn -> Stream<'x>) -> Stream<'x>
    member While: (unit -> bool) * Stream<'x> -> Stream<'x>
    member inline Yield: 'x -> Stream<'x>
    member inline YieldFrom: Stream<'x> -> Stream<'x>
