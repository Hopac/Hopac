// Copyright (C) by Vesa Karvonen

namespace Hopac

open System

/// Operations on choice streams.
module Stream =
  /// Represents a point in a non-deterministic stream of values.
  type Cons<'x> =
    /// Communicates a value and the remainder of the stream.
    | Cons of Value: 'x * Next: Alt<Cons<'x>>
    /// Communicates the end of the stream.
    | Nil

  /// Represents a non-deterministic stream of values called a choice stream.
#if DOC
  ///
  /// Choice streams can be used in ways similar to Rx observable sequences.
  /// However, the underlying implementations of choice streams and observable
  /// sequences are almost polar opposites: choice streams are pull based while
  /// obserable sequences are push based.
  ///
  /// Probably the most notable advantage of observable sequences over choice
  /// streams is that observables support disposables via their subscription
  /// protocol.  Choice streams do not have a subscription protocol and cannot
  /// support disposables in the same manner.
  ///
  /// On the other hand, choice streams offer several advantages over observable
  /// sequences:
  ///
  /// - Choice streams are simple and allow consumers and producers to be
  /// written using simple programming techniques such as lexical binding,
  /// recursion and immutable data structures.  You can see many examples of
  /// this in the reference implementations of various stream combinators.
  /// Observable sequences can only be subscribed to by imperative callbacks.
  /// The implementation of choice streams is two orders of magnitude shorter
  /// than the implementation of .Net Rx.
  ///
  /// - Choice streams are designed to be consistent in that they generate the
  /// same sequence of values for every consumer.  There are no hot and cold
  /// observables like with observable sequences.  Many trivial choice stream
  /// combinators, such as `tails`, can be very challenging, if not impossible,
  /// to specify and implement meaningfully for observable sequences.
  ///
  /// - Choice streams allow the use of asynchronous programming at any point.
  /// For example, `iterJob` waits for the asynchronous job to finish before
  /// consuming the next value from the stream.  The `Subscribe` operation of
  /// observables cannot support such behavior, because `OnNext` calls are
  /// synchronous.
  ///
  /// - Choice streams allow values to be generated both lazily in response to
  /// consumers, see the fibonacci example in `delay`, and eagerly in response
  /// to producers, see `shift`.  Observable sequences can only be generated
  /// eagerly in response to producers.  The fibonacci example cannot be
  /// expressed using observable sequences, because an observable sequence would
  /// enumerate the fibonacci sequence eagerly, and combinators like `afterEach`
  /// and `beforeEach` cannot be implemented for observable sequences, because
  /// observables do not have a protocol for requesting elements one by one.
  ///
  /// All of the above advantages are strongly related and result from the pull
  /// based nature of choice streams.
  ///
  /// While the most common operations are very easy to implement on choice
  /// streams, some operations perhaps require more intricate programming than
  /// with push based models.  For example, `groupByFun` and `shift`, that
  /// corresponds to `Delay` in Rx, are non-trivial, although both
  /// implementations are actually much shorter than their .Net Rx counterparts.
#endif
  type Stream<'x> = Alt<Cons<'x>>

  /// Represents an imperative source of a stream of values called a stream
  /// source.
#if DOC
  ///
  /// A basic use for a stream source would be to produce a stream in response
  /// to events from a GUI.  For example, given a GUI button, one could write
  ///
  ///> let buttonClickSrc = Stream.Src.create ()
  ///> button.Click.Add (ignore >> Stream.Src.value buttonClickSrc >> start)
  ///
  /// to produce a stream of button clicks.  The `Stream.Src.tap` function
  /// returns the generated stream, which can then be manipulated using stream
  /// combinators.
  ///
  /// Here is a silly example.  We could write a stream combinator that counts
  /// the number of events within a given timeout period:
  ///
  ///> let eventsWithin timeout xs =
  ///>   let inc = xs |> Stream.mapFun (fun _ -> +1)
  ///>   let dec = xs |> Stream.mapFun (fun _ -> -1) |> Stream.shift timeout
  ///>   Stream.merge inc dec
  ///>   |> Stream.scanFromFun 0 (+)
  ///
  /// Given two stream sources for buttons, the following program would then
  /// print "That was fast!" whenever both buttons are clicked within 500ms.
  ///
  ///> let t500ms = timeOutMillis 500
  ///> let n1s = Stream.Src.tap button1ClickSrc |> eventsWithin t500ms
  ///> let n2s = Stream.Src.tap button2ClickSrc |> eventsWithin t500ms
  ///> Stream.combineLatest n1s n2s
  ///> |> Stream.chooseFun (fun (n1, n2) ->
  ///>    if n1 > 0 && n2 > 0 then Some () else None)
  ///> |> Stream.iterFun (fun () -> printfn "That was fast!")
  ///> |> queue
  ///
  /// Note that there are no special hidden mechanisms involved in the
  /// implementation of stream sources.  You can easily implement similar
  /// imperative mechanisms outside of the stream library.
#endif
  type Src<'x>

  /// Operations on stream sources.
  module Src =
    /// Creates a new stream source.
    val create: unit -> Src<'x>

    /// Appends a new value to the end of the generated stream.  This operation
    /// is atomic and non-blocking and can be safely used from multiple parallel
    /// jobs.
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
#if DOC
  ///
  /// The difference between a stream variable and a stream source is that a
  /// stream variable cannot be closed and always has a value.  Stream variables
  /// are one way to represent state, or the model, manipulated by a program
  /// using streams.
  ///
  /// Note that there are no special hidden mechanisms involved in the
  /// implementation of stream variables.  You can easily implement similar
  /// imperative mechanisms outside of the stream library.
#endif
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

  /// An empty or closed choice stream.
  val inline nil<'x> : Stream<'x>

  /// `cons x xs` constructs a choice stream whose first value is `x` and the
  /// rest of the stream is like `xs`.
#if DOC
  ///
  /// Note that `cons` and `nil` directly correspond to the ordinary list
  /// constructors `::` and `[]` and, aside from the obvious notational
  /// differences, you can construct choice streams just like you would create
  /// ordinary lists.
#endif
  val cons: 'x -> Stream<'x> -> Stream<'x>

  /// `delay` creates a stream that is constructed lazily.  Use `delay` to avoid
  /// unbounded eager recursion.
#if DOC
  ///
  /// Note that with `delay`, `cons` and `nil`, you can express arbitrary lazy
  /// streams.  For example,
  ///
  ///> let fibs: Stream<BigInteger> =
  ///>   let rec lp f0 f1 = cons f0 << delay <| fun () -> lp f1 (f0 + f1)
  ///>   lp 0I 1I
  ///
  /// is the stream of all fibonacci numbers.
  ///
  /// The above `fibs` streams produces results lazily, but can do so at a
  /// relatively fast rate when it is being pulled eagerly.  The following
  ///
  ///> let slowFibs =
  ///>   fibs
  ///>   |> afterEach (timeOutMillis 1000)
  ///
  /// stream would produce the fibonacci sequence with at most one element per
  /// second.
#endif
  val inline delay: (unit -> #Stream<'x>) -> Stream<'x>

  /// A choice stream that never produces any values and never closes.
  val inline never<'x> : Stream<'x>

  /// Constructs a choice stream that is closed with an error.
  val inline error: exn -> Stream<'x>

  /// `one x` is equivalent to `cons x nil`.
  val inline one: 'x -> Stream<'x>

  /// Converts the given sequence to a lazy stream.
  val ofSeq: seq<'x> -> Stream<'x>

  /// Generates a stream by repeating the given job indefinitely.  For example,
  /// given a channel, `xCh`, a stream can be created, `indefinitely xCh`,
  /// through which all the values given on the channel can be observed.  See
  /// also: `values`.
  val indefinitely: Job<'x> -> Stream<'x>

  /// `once xJ` is equivalent to `indefinitely xJ |> take 1`.
  val once: Job<'x> -> Stream<'x>

  /// Preliminary and subject to change.
  val unfoldJob: ('s -> #Job<option<'x * 's>>) -> 's -> Stream<'x>
  /// Preliminary and subject to change.
  val unfoldFun: ('s -> option<'x * 's>) -> 's -> Stream<'x>

  /// Preliminary and subject to change.
  val iterateJob: ('x -> #Job<'x>) -> 'x -> Stream<'x>
  /// Preliminary and subject to change.
  val iterateFun: ('x -> 'x) -> 'x -> Stream<'x>

  /// Creates an infinite stream of the given value.
  val repeat: 'x -> Stream<'x>

  /// Creates an infinite repetition of the given stream.  For infinite streams
  /// `cycle` is the identity function.
  val cycle: Stream<'x> -> Stream<'x>

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
  val subscribeDuring: (Stream<'x> -> #Stream<'y>) -> IObservable<'x> -> Stream<'y>

  /// Preliminary and subject to change.
  val subscribingTo: IObservable<'x> -> (Stream<'x> -> #Job<'y>) -> Job<'y>

  /// Returns observable that eagerly consumes the stream
  val toObservable: Stream<'x> -> IObservable<'x>

  // Sequence combinators

  /// Returns a stream that produces results whenever the given stream produces
  /// an element and the given job returns `Some` result from that element.
  val chooseJob: ('x -> #Job<option<'y>>) -> Stream<'x> -> Stream<'y>

  /// Returns a stream that produces results whenever the given stream produces
  /// an element and the given function returns `Some` result from that element.
  val chooseFun: ('x -> option<'y>) -> Stream<'x> -> Stream<'y>

  /// `xs |> choose` is equivalent to `xs |> chooseFun id`.
  val choose: Stream<option<'x>> -> Stream<'x>

  /// Preliminary and subject to change.
  val filterJob: ('x -> #Job<bool>) -> Stream<'x> -> Stream<'x>
  /// Preliminary and subject to change.
  val filterFun: ('x -> bool) -> Stream<'x> -> Stream<'x>

  /// Returns a stream that produces elements passed through the given job
  /// whenever the given streams produces elements.
  ///
  /// Reference implementation:
  ///
  ///> let rec mapJob x2yJ xs =
  ///>   memo (xs >>= function Nil -> nil
  ///>                       | Cons (x, xs) ->
  ///>                         x2yJ x |>>? fun y -> Cons (y, mapJob x2yJ xs))
  ///
  /// Above, `memo` is `fun x -> Promise.Now.delay x :> Alt<_>`.
  val mapJob: ('x -> #Job<'y>) -> Stream<'x> -> Stream<'y>

  /// Returns a stream that produces elements passed through the given function
  /// whenever the given streams produces elements.
  val mapFun: ('x -> 'y) -> Stream<'x> -> Stream<'y>

  /// Preliminary and subject to change.
  val groupByJob: ('x -> #Job<'k>) -> Stream<'x> -> Stream<'k * Stream<'x>> when 'k: equality
  /// Preliminary and subject to change.
  val groupByFun: ('x -> 'k) -> Stream<'x> -> Stream<'k * Stream<'x>> when 'k: equality

  /// Returns a stream of pairs of elements from the given pair of streams.  No
  /// elements from either stream are skipped and each element is used only
  /// once.  See also: `combineLatest`.
#if DOC
  ///
  /// For example,
  ///
  ///> zip xs (tail xs)
  ///
  /// is a stream of consecutive pairs from the stream `xs`.
#endif
  val zip: Stream<'x> -> Stream<'y> -> Stream<'x * 'y>

  /// Preliminary and subject to change.
  val scanJob: ('s -> 'x -> #Job<'s>) -> 's -> Stream<'x> -> Stream<'s>
  /// Preliminary and subject to change.
  val scanFun: ('s -> 'x -> 's) -> 's -> Stream<'x> -> Stream<'s>

  /// `scanFromJob s sx2sJ xs` is equivalent to `scanJob sx2sJ s xs` and is
  /// often syntactically more convenient to use.
  val scanFromJob: 's -> ('s -> 'x -> #Job<'s>) -> Stream<'x> -> Stream<'s>

  /// `scanFromFun s sx2sJ xs` is equivalent to `scanFun sx2sJ s xs` and is
  /// often syntactically more convenient to use.
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

  /// Of the two given streams, returns the stream that first produces an
  /// element.  See also: `ambMap`.
  val amb: Stream<'x> -> Stream<'x> -> Stream<'x>

  /// Returns a stream that produces elements from both of the given streams so
  /// that elements from the streams are interleaved non-deterministically in
  /// the returned stream.  See also: `mergeMap`.
#if DOC
  ///
  /// Reference implementation:
  ///
  ///> let rec mergeSwap ls rs =
  ///>   ls >>= function Nil -> rs
  ///>                 | Cons (l, ls) -> cons l (merge rs ls)
  ///> and merge ls rs = mergeSwap ls rs <|>* mergeSwap rs ls
#endif
  val merge: Stream<'x> -> Stream<'x> -> Stream<'x>

  /// Concatenates the given two streams.  In other words, returns a stream that
  /// first produces all the elements from first stream and then all the
  /// elements from the second stream.  If the first stream is infinite, no
  /// elements are produced from the second stream.  See also: `appendMap`.
  val append: Stream<'x> -> Stream<'x> -> Stream<'x>

  /// Returns a stream that produces elements from the first stream as long as
  /// the second stream produces no elements.  As soon as the second stream
  /// produces an element, the returned stream only produces elements from the
  /// second stream.  See also: `switchMap`.
#if DOC
  ///
  ///>  first: a b    c   d
  ///> second:      1  2 3  4 ...
  ///> output: a b  1  2 3  4 ...
#endif
  val switch: Stream<'x> -> Stream<'x> -> Stream<'x>

  /// Joins all the streams in the given stream of streams together with the
  /// given binary join combinator.
  val joinWith: (Stream<'x> -> Stream<'y> -> #Stream<'y>) -> Stream<#Stream<'x>> -> Stream<'y>

  /// `mapJoin j f xs` is equivalent to `joinWith j (mapFun f xs)`.
  val mapJoin: (Stream<'y> -> Stream<'z> -> #Stream<'z>) -> ('x -> #Stream<'y>) -> Stream<'x> -> Stream<'z>

  /// Maps and joins all the streams together with `amb`.  This corresponds to
  /// the idea of starting several alternative streams in parallel and then only
  /// using the one that produces the first result.
  val ambMap: ('x -> #Stream<'y>) -> Stream<'x> -> Stream<'y>

  /// Maps and joins all the streams together with `merge`.  This corresponds to
  /// interleaving results based on all sources of information.  While this is a
  /// theoretically important combinator, `mergeMap` is probably not the most
  /// useful binding form on choice streams.
  val mergeMap: ('x -> #Stream<'y>) -> Stream<'x> -> Stream<'y>

  /// Maps and joins all the streams together with `append`.  This is roughly
  /// the same function as `Seq.collect`, but is probably less frequently used
  /// with choice streams.
  val appendMap: ('x -> #Stream<'y>) -> Stream<'x> -> Stream<'y>

  /// Maps and joins all the streams together with `switch`.  This is perhaps
  /// the most useful binding form with choice streams as this correspond to the
  /// idea of producing results based only on the latest source of information.
  val switchMap: ('x -> #Stream<'y>) -> Stream<'x> -> Stream<'y>

  // Skipping and taking

  /// `skip n xs` returns a stream without the first `n` elements of the given
  /// stream.  If the given stream is shorter than `n`, then the returned stream
  /// will be empty.  Note that if `n` is non-negative, then `append (take n xs)
  /// (skip n xs)` is equivalent to `xs`.
  val skip: int64 -> Stream<'x> -> Stream<'x>

  /// `take n` returns a stream that has the first `n` elements of the given
  /// stream.  If the given stream is shorter than `n`, then `take n` is the
  /// identity function.  Note that if `n` is non-negative, then `append (take n
  /// xs) (skip n xs)` is equivalent to `xs`.
  val take: int64 -> Stream<'x> -> Stream<'x>

  /// Preliminary and subject to change.
  val skipUntil: Alt<_> -> Stream<'x> -> Stream<'x>
  /// Preliminary and subject to change.
  val takeUntil: Alt<_> -> Stream<'x> -> Stream<'x>

  /// Preliminary and subject to change.
  val switchOn: Stream<'x> -> Stream<'x> -> Stream<'x>

  // Exceptions

  /// Returns a stream that produces the same sequence of elements as the given
  /// stream.  If the given stream fails, a new stream is constructed by calling
  /// the given function and that stream becomes the remainder of the stream.
  val catch: (exn -> #Stream<'x>) -> Stream<'x> -> Stream<'x>

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

  /// `sample ticks elems` returns a stream that produces each `elem` that is
  /// followed by a `tick`.  Excess elements from both streams are skipped.  In
  /// other words, `elem` followed by `elem` and `tick` followed by `tick` is
  /// skipped.
#if DOC
  ///
  ///>  elems: 1  2  3        4 5 6  7
  ///>  ticks:     x    x    x    x    x
  ///> output:     2    3         6    7
#endif
  val sample: ticks: Stream<_> -> elems: Stream<'x> -> Stream<'x>

  /// Returns a stream that produces elements from the given stream so that an
  /// element is produced after the given timeout unless a new element is
  /// produced by the given stream in which case the timeout is restarted.  Note
  /// that if the given stream produces elements more frequently than the
  /// timeout, the returned stream never produces any elements.  See also:
  /// `throttle`.
#if DOC
  ///
  ///>   input: 1        2 3  4     5 6 7 8 9 ...
  ///> timeout: +---x    +-+--+---x +-+-+-+-+-...
  ///>  output:     1             4
#endif
  val debounce: timeout: Alt<_> -> Stream<'x> -> Stream<'x>

  /// Returns a stream that produces elements from the given stream so that
  /// after an element is produced by the given stream, a timeout is started and
  /// the latest element produced by the stream is produced when the timeout
  /// expires.  See also: `debounce`.
#if DOC
  ///
  ///>   input: 1        2 3   4
  ///> timeout: +---x    +---x +---x
  ///>  output:     1        3     4
#endif
  val throttle: timeout: Job<_> -> Stream<'x> -> Stream<'x>

  /// Returns a stream that produces a new pair of elements whenever either one
  /// of the given pair of streams produces an element.  If one of the streams
  /// produces multiple elements before any elements are produced by the other
  /// stream, then those elements are skipped.  See also: `zip`.
#if DOC
  ///
  ///>  xs: 1 2                  3     4
  ///>  ys:     a     b     c
  ///> xys:   (2,a) (2,b) (2,c)(3,c) (4,c)
#endif
  val combineLatest: Stream<'x> -> Stream<'y> -> Stream<'x * 'y>

  /// Returns a stream that produces the same sequence of elements as the given
  /// stream, but shifted in time by the given timeout.
#if DOC
  ///
  ///>   input: 1        2 3   4        5
  ///> timeout: +---x    +---x +---x
  ///>                     +---x        +---x
  ///>  output:     1        2 3   4        5
  ///
  /// The `shift` operation pulls the input while the stream returned by `shift`
  /// is being pulled.  If the stream produced by `shift` is not pulled, `shift`
  /// will stop pulling the input.  This basically means that the timing of the
  /// output can be determined by an eager producer of the input.
  ///
  /// Note that this operation has a fairly complex implementation.  Unless you
  /// absolutely want this behavior, you might prefer a combinator such as
  /// `delayEach`.
#endif
  val shift: timeout: Job<_> -> Stream<'x> -> Stream<'x>

  /// Returns a stream that produces the same elements as the given stream, but
  /// delays each pulled element using the given job.  If the given job fails,
  /// the returned stream also fails.  See also: `shift`.
#if DOC
  ///
  ///>   input: 1        2 3   4        5
  ///> timeout: +---x    +---x---x---x  +---x
  ///>  output:     1        2   3   4      5
  ///
  /// In the above, the `input` is considered to be independent of the pull
  /// operations performed by `delayEach`.  For streams that produce output
  /// infrequently in relation to the timeout, `delayEach` behaves similarly to
  /// `shift`.
#endif
  val delayEach: Job<_> -> Stream<'x> -> Stream<'x>

  /// Returns a stream that produces the same elements as the given stream, but
  /// after each element, the given job is used as a delay before a request is
  /// made to the given stream for the next element.  If the given job fails,
  /// the returned stream also fails.
#if DOC
  ///
  /// Suppose that an application needs to poll for some information, e.g. by
  /// making a http request, using a job named `poll`.  Using `indefinitely` and
  /// `afterEach` we can specify a stream for polling:
  ///
  ///> indefinitely poll
  ///> |> afterEach (timeOutMillis 10000)
  ///
  /// The above stream ensures that polls are at least 10 seconds apart.  Also
  /// when polls are requested less frequently, there is no delay before a poll.
#endif
  val afterEach: Job<_> -> Stream<'x> -> Stream<'x>

  /// Returns a stream that runs the given job each time a value is requested
  /// before requesting the next value from the given stream.  If the given job
  /// fails, the returned stream also fails.
#if DOC
  ///
  /// Reference implementation:
  ///
  ///> let rec beforeEach yJ xs =
  ///>   (yJ >>. xs) >>=* function Nil -> nil
  ///>                           | Cons (x, xs) -> cons x (beforeEach yJ xs)
#endif
  val beforeEach: Job<_> -> Stream<'x> -> Stream<'x>

  /// Preliminary and subject to change.
  val atDateTimeOffsets: Stream<DateTimeOffset> -> Stream<DateTimeOffset>
  /// Preliminary and subject to change.
  val atDateTimeOffset: DateTimeOffset -> Stream<DateTimeOffset>

  /// Preliminary and subject to change.
  val afterTimeSpan: TimeSpan -> Stream<unit>

  // Eliminating streams

  /// Preliminary and subject to change.
  val toSeq: Stream<'x> -> Job<ResizeArray<'x>>

  /// Creates an alternative through which all the values of the stream
  /// generated after the point at which the alternative has been created can be
  /// read.  See also: `indefinitely`.
  val values: Stream<'x> -> Alt<'x>

  /// Preliminary and subject to change.
  val foldJob: ('s -> 'x -> #Job<'s>) -> 's -> Stream<'x> -> Job<'s>
  /// Preliminary and subject to change.
  val foldFun: ('s -> 'x -> 's) -> 's -> Stream<'x> -> Job<'s>

  /// `foldFromJob s sx2sJ xs` is equivalent to `foldJob sx2sJ s xs` and is
  /// often syntactically more convenient to use.
  val foldFromJob: 's -> ('s -> 'x -> #Job<'s>) -> Stream<'x> -> Job<'s>
  /// `foldFromFun s sx2s xs` is equivalent to `foldFun sx2s s xs` and is often
  /// syntactically more convenient to use.
  val foldFromFun: 's -> ('s -> 'x -> 's) -> Stream<'x> -> Job<'s>

  /// Returns a job that sequentially iterates the given job constructor over
  /// the given stream.
#if DOC
  ///
  /// Reference implementation:
  ///
  ///> let rec iterJob x2uJ xs =
  ///>   xs >>= function Nil -> Job.unit ()
  ///>                 | Cons (x, xs) -> x2uJ x >>. iterJob x2uJ xs
#endif
  val iterJob: ('x -> #Job<unit>) -> Stream<'x> -> Job<unit>

  /// Returns a job that sequentially iterates the given function over the given
  /// stream.  See also: `iterJob`.
  val iterFun: ('x -> unit) -> Stream<'x> -> Job<unit>

  /// Returns a job that computes the length of the given stream.
  val count: Stream<'x> -> Job<int64>

  /// Returns a stream containing only the first element of the given stream.
  /// If the given stream is closed, the result stream will also be closed.
  /// Note that `append (head xs) (tail xs)` is equivalent to `xs`.
  val head: Stream<'x> -> Stream<'x>

  /// Returns a stream just like the given stream except without the first
  /// element.  If the given stream is closed, the result stream will also be
  /// closed.  Note that `append (head xs) (tail xs)` is equivalent to `xs`.
  val tail: Stream<'x> -> Stream<'x>

  /// Returns a stream of all final segments of the given stream from longest to
  /// shortest.
#if DOC
  ///
  /// Reference implementation:
  ///
  ///> let rec tails xs =
  ///>   cons xs (xs >>=* function Nil -> nil
  ///>                           | Cons (_, xs) -> tails xs)
#endif
  val tails: Stream<'x> -> Stream<Stream<'x>>

  /// Returns a stream containing the last element of the given stream.  If the
  /// given stream is closed, a closed stream is returned.  Note that `append
  /// (init xs) (last xs)` is equivalent to `xs`.
  val last: Stream<'x> -> Stream<'x>

  /// Returns a stream with all the elements of the given stream except the last
  /// element.  If the stream is closed, a closed stream is returned.  Note that
  /// `append (init xs) (last xs)` is equivalent to `xs`.
  val init: Stream<'x> -> Stream<'x>

  /// Returns a stream of all initial segments of the given stream from shortest
  /// to longest.
  val inits: Stream<'x> -> Stream<Stream<'x>>

  /// An experimental generic builder for streams.  The abstract `Join`
  /// operation needs to be implemented in a derived class.  The `Join`
  /// operation is then used to implement `Bind`, `Combine`, `For` and `While`
  /// to get a builder with consistent semantics.
  type [<AbstractClass>] Builder =
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
    abstract Join: Stream<'x> * Stream<'x> -> Stream<'x>

  /// This builder joins substreams with `amb` to produce a stream with the
  /// first results.
  val ambed: Builder

  /// This builder joins substreams with `append` to produce a stream with all
  /// results in sequential order.
  val appended: Builder

  /// This builder joins substreams with `merge` to produce a stream with all
  /// results in completion order.
  val merged: Builder

  /// This builder joins substreams with `switch` to produce a stream with the
  /// latest results.
  val switched: Builder
