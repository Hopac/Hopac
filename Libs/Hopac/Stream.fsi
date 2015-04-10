// Copyright (C) by Vesa Karvonen

namespace Hopac

open System
open System.Threading
open System.Collections.Generic
open System.ComponentModel

/// Operations on choice streams.
module Stream =
  /// Represents a point in a non-deterministic stream of values.
  type Cons<'x> =
    /// Communicates a value and the remainder of the stream.
    | Cons of Value: 'x * Next: Promise<Cons<'x>>
    /// Communicates the end of the stream.
    | Nil

  /// Represents a non-deterministic stream of values called a choice stream.
#if DOC
  ///
  /// Choice streams can be used in ways similar to Rx observable sequences.
  /// However, the underlying implementations of choice streams and observable
  /// sequences are almost polar opposites: choice streams are pull based while
  /// observable sequences are push based.
  ///
  /// Probably the most notable advantage of observable sequences over choice
  /// streams is that observables support disposables via their all-or-nothing
  /// subscription protocol.  Choice streams cannot support disposables in the
  /// exact same manner, because elements are requested asynchronously one at a
  /// time and choice streams do not have a subscription protocol.  However,
  /// `onCloseJob` and `doFinalizeJob` provide similar functionality.
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
  /// - Basically all operations on ordinary lazy streams can be implemented on
  /// and are meaningful on choice streams.  The same is not true of observable
  /// sequences, because they do not compose the same way.  Many trivial choice
  /// stream combinators, such as `foldBack` and `tails`, can be either
  /// impossible or very challenging to specify and implement meaningfully for
  /// observable sequences.
  ///
  /// - Choice streams allow for the use of asynchronous programming at any
  /// point.  Most higher-order choice stream combinators taking have both an
  /// asynchronous `Job` and a synchronous `Fun` form.  For example, `iterJob`
  /// waits for the asynchronous job to finish before consuming the next value
  /// from the stream.  The `Subscribe` operation of observables cannot support
  /// such behavior, because `OnNext` calls are synchronous.
  ///
  /// - Choice streams are consistent in that every consumer of a stream gets
  /// the exact same sequence of values unlike with observable sequences.  In
  /// other words, choice streams are immutable and elements are not discarded
  /// implicitly.  There is no need for `Connect` and `Publish` or `Replay` like
  /// with observable sequences.
  ///
  /// - The asynchronous one element at a time model of choice streams allows
  /// for a basic form of backpressure or the flow of synchronization from
  /// consumers to producers.  This allows for many new operations to expressed.
  /// For example, `keepPreceding1` and `keepFollowing1` cannot be implemented
  /// for observable sequences.  Operations such as `afterEach` and `beforeEach`
  /// have semantics that are pull based and lazy and cannot be implemented for
  /// observable sequences.  Operations such as `pullOn`, or `zip` in disguise,
  /// have new uses.
  ///
  /// All of the above advantages are strongly related and result from the pull
  /// based nature of choice streams.  Pull semantics puts the consumer in
  /// control.
  ///
  /// While the most common operations are very easy to implement on choice
  /// streams, some operations perhaps require more intricate programming than
  /// with push based models.  For example, `groupByFun` and `shift`, which
  /// corresponds to `Delay` in Rx, are non-trivial, although both
  /// implementations are actually much shorter than their .Net Rx counterparts.
#endif
  type Stream<'x> = Promise<Cons<'x>>

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
  /// combinators.  See also: `ofObservableOnMain`.
  ///
  /// Here is a silly example.  We could write a stream combinator that counts
  /// the number of events within a given timeout period:
  ///
  ///> let eventsWithin timeout xs =
  ///>   let inc = xs |> Stream.mapConst +1
  ///>   let dec = xs |> Stream.mapConst -1 |> Stream.shift timeout
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
  ///> |> Stream.consumeFun (fun () -> printfn "That was fast!")
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
  /// stream of values as a side-effect.  See also: `MVar<'x>`.
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
    /// `set` is not atomic.  See also: `MVar<'x>`.
    val set: Var<'x> -> 'x -> Job<unit>

    /// Returns the generated stream, including the current value of the
    /// variable, from the point in time when `tap` is called.
    val tap: Var<'x> -> Stream<'x>

  /// Represents a serialized mutable stream variable that generates a stream of
  /// values as a side-effect.  The difference between `MVar<'x>` and `Var<'x>`
  /// is that read-modify-write operations, such as `MVar.updateJob`, are
  /// serialized, so they effectively appear as atomic, like with the ordinary
  /// `MVar<'x>`.
  type MVar<'x>

  /// Operations on serialized stream variables.
  module MVar =
    /// Creates a new serialized stream variable.
    val create: 'x -> MVar<'x>

    /// Returns a job that gets the value of the variable.
    val get: MVar<'x> -> Job<'x>

    /// Creates a job that sets the value of the variable.  Note that a
    /// combination of `get` and `set` is not serialized.  See also:
    /// `updateFun`.
    val set: MVar<'x> -> 'x -> Job<unit>

    /// Creates a job that updates the value of the variable with the given
    /// function in a serialized fashion.  If the function raises an exception,
    /// the variable will not be modified.  See also: `updateJob`.
    val updateFun: MVar<'x> -> ('x -> 'x) -> Job<unit>

    /// Creates a job that updates the value of the variable with the given job
    /// in a serialized fashion.  If the job raises an exception, the variable
    /// will not be modified.  See also: `updateFun`.
    val updateJob: MVar<'x> -> ('x -> #Job<'x>) -> Job<unit>

    /// Returns the generated stream, including the current value of the
    /// variable, from the point in time when `tap` is called.
    val tap: MVar<'x> -> Stream<'x>

  /// Represents a mutable property, much like a stream variable, that generates
  /// a stream of values and property change notifications as a side-effect.
  type Property<'x> =
    interface INotifyPropertyChanged

    /// Creates a new property with the specified initial value.
    new: 'x -> Property<'x>

    /// Allows to get and set the value of a `Property<'x>`.
    member Value: 'x with get, set

    /// Returns the generated stream, including the current value of the
    /// property, from the point in time when `Tap` is called.
    member Tap: unit -> Stream<'x>

  // Introducing streams

  /// An empty or closed choice stream.  `nil` is also the identity element for
  /// the `merge` and `append` combinators.  `nil` is equivalent to
  /// `Promise.Now.withValue Nil`.
  val inline nil<'x> : Stream<'x>

  /// `cons x xs` constructs a choice stream whose first value is `x` and the
  /// rest of the stream is computed using `xs`.  For example, `cons 1 << cons 2
  /// << cons 3 <| cons 4 nil` is a stream producing the sequence `1 2 3 4`.
  /// See also: `delay`.
#if DOC
  ///
  /// `cons x xs` is equivalent to `Promise.Now.withValue (Cons (x, xs))`.
  ///
  /// Note that `cons` and `nil` directly correspond to the ordinary list
  /// constructors `::` and `[]` and, aside from the obvious notational
  /// differences, you can construct choice streams just like you would create
  /// ordinary lists.
#endif
  val inline cons: 'x -> Stream<'x> -> Stream<'x>

  /// `delay` creates a stream that is constructed lazily.  Use `delay` to make
  /// lazy streams and to avoid unbounded eager recursion.
#if DOC
  ///
  /// `delay u2xs` is equivalent to `Promise.Now.delay <| Job.delay u2xs`.
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
  val inline delay: (unit -> #Job<Cons<'x>>) -> Stream<'x>

  /// A choice stream that never produces any values and never closes.  While
  /// perhaps rarely used, this is theoretically important as the identity
  /// element for the `switch` and `amb` combinators.
  val inline never<'x> : Stream<'x>

  /// Constructs a choice stream that is closed with an error.
  val inline error: exn -> Stream<'x>

  /// Returns a stream of length one containing the given value.  `one x` is
  /// equivalent to `cons x nil`.
  val one: 'x -> Stream<'x>

  /// Converts the given sequence to a lazy stream.
  val ofSeq: seq<'x> -> Stream<'x>

  /// Returns a lazy stream whose elements are generated by running the given
  /// job.  For example, given a channel, `xCh`, a stream can be created,
  /// `indefinitely xCh`, through which all the values given on the channel can
  /// be observed.  See also: `values`.
#if DOC
  ///
  /// Reference implementation:
  ///
  ///> let rec indefinitely xJ = xJ >>=* fun x -> cons x (indefinitely xJ)
#endif
  val indefinitely: Job<'x> -> Stream<'x>

  /// Returns a lazy stream that, when pulled, runs the given job, produces the
  /// result of the job and closes.  `once xJ` is equivalent to `indefinitely xJ
  /// |> take 1`.
  val once: Job<'x> -> Stream<'x>

  /// Returns a lazy stream that contains the elements generated by the given
  /// job.  See also: `foldBack`.
#if DOC
  ///
  /// For example, `mapJob` could be defined via `unfoldJob` as follows:
  ///
  ///> let mapJob x2yJ xs =
  ///>   xs
  ///>   |> unfoldJob (fun xs ->
  ///>      xs >>= function Nil -> Job.result None
  ///>                    | Cons (x, xs) ->
  ///>                      x2yJ x |>> fun y -> Some (y, xs))
  ///
  /// Reference implementation:
  ///
  ///> let rec unfoldJob f s =
  ///>   f s >>=* function None -> nil
  ///>                   | Some (x, s) -> cons x (unfoldJob f s)
#endif
  val unfoldJob: ('s -> #Job<option<'x * 's>>) -> 's -> Stream<'x>

  /// Returns a lazy stream that contains the elements generated by the given
  /// function.
  val unfoldFun: ('s -> option<'x * 's>) -> 's -> Stream<'x>

  /// Generator functions for `generateFuns`.
  type [<AbstractClass>] GenerateFuns<'s, 'x> =
    new: unit -> GenerateFuns<'s, 'x>
    abstract While: 's -> bool
    abstract Next: 's -> 's
    abstract Select: 's -> 'x

  /// Generates a stream from the given state using the given function object.
  val generateFuns: 's -> GenerateFuns<'s, 'x> -> Stream<'x>

  /// Generates a stream.
  val inline generateFun: 's -> ('s -> bool) -> ('s -> 's) -> ('s -> 'x) -> Stream<'x>

  /// Returns an infinite stream of repeated applications of the given job to
  /// the given initial value.
#if DOC
  ///
  /// Reference implementation:
  ///
  ///> let rec iterateJob f x = cons x (f x >>=* iterateJob f)
#endif
  val iterateJob: ('x -> #Job<'x>) -> 'x -> Stream<'x>

  /// Returns an infinite stream of repeated applications of the given function
  /// to the given initial value.
  val iterateFun: ('x -> 'x) -> 'x -> Stream<'x>

  /// Creates an infinite stream of the given value.
  val repeat: 'x -> Stream<'x>

  /// Creates an infinite repetition of the given stream.  For infinite streams
  /// `cycle` is the identity function.
  val cycle: Stream<'x> -> Stream<'x>

  // Observables

  /// Subscribes to the given observable on the specified synchronization
  /// context and returns the events pushed by the observable as a stream.  A
  /// finalizer is used to automatically unsubscribe from the observable after
  /// the stream is no longer reachable.
  val ofObservableOn: subscribeOn: SynchronizationContext
                   -> IObservable<'x>
                   -> Stream<'x>

  /// `ofObservableOnMain xO` is equivalent to `ofObservable main xO`, where
  /// `main` is the main synchronization context as set by application code
  /// using `setMain`.
  val ofObservableOnMain: IObservable<'x> -> Stream<'x>

  /// `ofObservable xO` is equivalent to `ofObservableOn null xO`.  Note that it
  /// is often necessary to specify the synchronization context to subscribe on.
  /// See also: `Observable.SubscribeOn`.
  val ofObservable: IObservable<'x> -> Stream<'x>

  /// Returns an observable that eagerly consumes the given stream.
  val toObservable: Stream<'x> -> IObservable<'x>

  // Sequence combinators

  /// Returns a stream that produces results whenever the given stream produces
  /// an element and the given job returns `Some` result from that element.
#if DOC
  ///
  /// Reference implementation:
  ///
  ///> let rec chooseJob f xs =
  ///>   xs >>=* function Nil -> nil
  ///>                  | Cons (x, xs) ->
  ///>                    f x >>=* function None -> chooseJob f xs
  ///>                                    | Some y -> cons y (chooseJob f xs)
#endif
  val chooseJob: ('x -> #Job<option<'y>>) -> Stream<'x> -> Stream<'y>

  /// Returns a stream that produces results whenever the given stream produces
  /// an element and the given function returns `Some` result from that element.
  val chooseFun: ('x -> option<'y>) -> Stream<'x> -> Stream<'y>

  /// `xs |> choose` is equivalent to `xs |> chooseFun id`.
  val choose: Stream<option<'x>> -> Stream<'x>

  /// Returns a stream that contains the elements from the given stream for
  /// which the given job returns `true`.
  val filterJob: ('x -> #Job<bool>) -> Stream<'x> -> Stream<'x>

  /// Returns a stream that contains the elements from the given stream for
  /// which the given function returns `true`.
  val filterFun: ('x -> bool) -> Stream<'x> -> Stream<'x>

  /// Returns a stream that produces elements passed through the given job
  /// whenever the given streams produces elements.
#if DOC
  ///
  /// Reference implementation:
  ///
  ///> let rec mapJob f xs =
  ///>   xs >>=* function Nil -> nil
  ///>                  | Cons (x, xs) ->
  ///>                    f x >>=* fun y -> cons y (mapJob f xs)
#endif
  val mapJob: ('x -> #Job<'y>) -> Stream<'x> -> Stream<'y>

  /// Returns a stream that produces elements passed through the given function
  /// whenever the given streams produces elements.  `mapFun x2y` is equivalent
  /// to `mapJob (Job.lift x2y)`.
  val mapFun: ('x -> 'y) -> Stream<'x> -> Stream<'y>

  /// Returns a stream that produces the given element each time the given
  /// stream produces an element.
  val mapConst: 'y -> Stream<'x> -> Stream<'y>

  /// `xs |> mapIgnore` is equivalent to `xs |> mapConst ()`.
  val mapIgnore: Stream<'x> -> Stream<unit>

  /// `groupByJob newGroup keyOf elems` splits the given source stream into
  /// substreams or groups based on the keys extracted from the elements by
  /// `keyOf` and formed using `newGroup`.  See also: `groupByFun`.
#if DOC
  ///
  /// New groups are formed by calling the given function with a key, a job for
  /// closing the substream and the substream.  Unless explicitly closed,
  /// substreams remain alive as long as the source stream.  When closing
  /// substreams, it is important to understand that streams operate
  /// concurrently.  This means that one should always consume the substream
  /// until it ends after closing it.  If, after closing a substream, the given
  /// stream produces more elements with the same key, a new substream with the
  /// key will be opened.
#endif
  val groupByJob: ('k -> Job<unit> -> Stream<'x> -> #Job<'y>)
               -> ('x -> #Job<'k>)
               -> Stream<'x>
               -> Stream<'y> when 'k: equality

  /// `groupByJob newGroup keyOf elems` splits the given source stream into
  /// substreams or groups based on the keys extracted from the elements by
  /// `keyOf` and formed using `newGroup`.  See `groupByJob` for further
  /// details.
  val groupByFun: ('k -> Job<unit> -> Stream<'x> -> 'y)
               -> ('x -> 'k)
               -> Stream<'x>
               -> Stream<'y> when 'k: equality

  /// Converts a stream of elements into a stream of non-overlapping buffers of
  /// at most given number of elements.
  val buffer: int -> Stream<'x> -> Stream<array<'x>>

  /// Returns a stream of pairs of elements from the given pair of streams.  No
  /// elements from either stream are skipped and each element is used only
  /// once.  In `zip xs ys`, the `xs` stream is examined first.  See also:
  /// `pullOn`, `combineLatest`.
#if DOC
  ///
  /// For example,
  ///
  ///> zip xs (tail xs)
  ///
  /// is a stream of consecutive pairs from the stream `xs`.
  ///
  /// Note that `zip` consumes the same number of elements from both given
  /// streams.  If one of the streams accumulates elements faster than the other
  /// stream, there will be an effective space leak.
#endif
  val zip: Stream<'x> -> Stream<'y> -> Stream<'x * 'y>

  /// `zipWithFun f xs ys` is equivalent to `zip xs ys |> mapFun (fun (x, y) ->
  /// f x y)`.
  val zipWithFun: ('x -> 'y -> 'z) -> Stream<'x> -> Stream<'y> -> Stream<'z>

  /// Returns a stream whose elements are computed using the given job and
  /// initial state as with `foldJob`.
#if DOC
  ///
  ///> let rec scanJob f s xs =
  ///>   cons s (xs >>=* function Nil -> nil
  ///>                          | Cons (x, xs) ->
  ///>                            f s x >>=* fun s -> scanJob f s xs)
#endif
  val scanJob: ('s -> 'x -> #Job<'s>) -> 's -> Stream<'x> -> Stream<'s>

  /// Returns a stream whose elements are computed using the given function and
  /// initial state as with `foldFun`.
  val scanFun: ('s -> 'x -> 's) -> 's -> Stream<'x> -> Stream<'s>

  /// `scanFromJob s sx2sJ xs` is equivalent to `scanJob sx2sJ s xs` and is
  /// often syntactically more convenient to use.
  val inline scanFromJob: 's -> ('s -> 'x -> #Job<'s>) -> Stream<'x> -> Stream<'s>

  /// `scanFromFun s sx2sJ xs` is equivalent to `scanFun sx2sJ s xs` and is
  /// often syntactically more convenient to use.
  val inline scanFromFun: 's -> ('s -> 'x -> 's) -> Stream<'x> -> Stream<'s>

  /// Returns a stream that contains no duplicate entries based on the keys
  /// returned by the given job.
  val distinctByJob: ('x -> #Job<'k>) -> Stream<'x> -> Stream<'x> when 'k: equality

  /// Returns a stream that contains no duplicate entries based on the keys
  /// returned by the given function.
  val distinctByFun: ('x -> 'k) -> Stream<'x> -> Stream<'x> when 'k: equality

  /// Returns a stream that contains no successive duplicate elements according
  /// to the given comparison job.
  val distinctUntilChangedWithJob: ('x -> 'x -> #Job<bool>) -> Stream<'x> -> Stream<'x>

  /// Returns a stream that contains no successive duplicate elements according
  /// to the given comparison function.
  val distinctUntilChangedWithFun: ('x -> 'x -> bool) -> Stream<'x> -> Stream<'x>

  /// Returns a stream that contains no successive duplicate elements based on
  /// the keys returned by the given job.
  val distinctUntilChangedByJob: ('x -> #Job<'k>) -> Stream<'x> -> Stream<'x> when 'k: equality

  /// Returns a stream that contains no successive duplicate elements based on
  /// the keys returned by the given function.
  val distinctUntilChangedByFun: ('x -> 'k) -> Stream<'x> -> Stream<'x> when 'k: equality

  /// Returns a stream that contains no successive duplicate elements.
#if DOC
  ///
  /// Reference implementation:
  ///
  ///> let distinctUntilChanged xs =
  ///>   append (head xs)
  ///>    (zip xs (tail xs)
  ///>     |> chooseFun (fun (x0, x1) ->
  ///>        if x0 <> x1 then Some x1 else None))
#endif
  val distinctUntilChanged: Stream<'x> -> Stream<'x> when 'x: equality

  // Joining streams

  /// Of the two given streams, returns the stream that first produces an
  /// element or is closed.  See also: `ambMap`, `never`.
#if DOC
  ///
  /// Reference implementation:
  ///
  ///> let amb ls rs = ls <|>* rs
#endif
  val amb: Stream<'x> -> Stream<'x> -> Stream<'x>

  /// Returns a stream that produces elements from both of the given streams so
  /// that elements from the streams are interleaved non-deterministically in
  /// the returned stream.  See also: `mergeMap`, `nil`.
#if DOC
  ///
  /// Reference implementation:
  ///
  ///> let rec mergeSwap ls rs =
  ///>   ls >>=? function Nil -> rs
  ///>                  | Cons (l, ls) -> cons l (merge rs ls)
  ///> and merge ls rs = mergeSwap ls rs <|>* mergeSwap rs ls
#endif
  val merge: Stream<'x> -> Stream<'x> -> Stream<'x>

  /// Concatenates the given two streams.  In other words, returns a stream that
  /// first produces all the elements from first stream and then all the
  /// elements from the second stream.  If the first stream is infinite,
  /// `append` should not be used, because no elements would be produced from
  /// the second stream.  See also: `appendMap`, `nil`.
  val append: Stream<'x> -> Stream<'x> -> Stream<'x>

  /// Returns a stream that produces elements from the first stream as long as
  /// the second stream produces no elements.  As soon as the second stream
  /// produces an element, the returned stream only produces elements from the
  /// second stream.  See also: `switchTo`, `switchMap`, `never`.
#if DOC
  ///
  ///>  first: a b    c   d
  ///> second:      1  2 3  4 ...
  ///> output: a b  1  2 3  4 ...
  ///
  /// Reference implementation:
  ///
  ///> let rec switch ls rs =
  ///>   rs <|>* (ls >>=? function Nil -> rs
  ///>                           | Cons (l, ls) -> cons l (switch ls rs))
#endif
  val switch: Stream<'x> -> Stream<'x> -> Stream<'x>

  /// `switchTo lhs rhs` is equivalent to `switch rhs lhs`.
#if DOC
  ///
  /// `switchTo` is designed to be used in pipelines:
  ///
  ///> firstStream
  ///> |> switchTo otherStream
#endif
  val switchTo: Stream<'x> -> Stream<'x> -> Stream<'x>

  /// Joins all the streams in the given stream of streams together with the
  /// given binary join combinator.
  val joinWith: ('x -> Stream<'y> -> #Job<Cons<'y>>) -> Stream<'x> -> Stream<'y>

  /// Joins all the streams together with `amb`.
  val ambAll: Stream<#Stream<'x>> -> Stream<'x>

  /// Joins all the streams together with `merge`.
  val mergeAll: Stream<#Stream<'x>> -> Stream<'x>

  /// Joins all the streams together with `append`.
  val appendAll: Stream<#Stream<'x>> -> Stream<'x>

  /// Joins all the streams together with `switch`.
  val switchAll: Stream<#Stream<'x>> -> Stream<'x>

  /// `mapJoin j f xs` is equivalent to `joinWith j (mapFun f xs)`.
  val inline mapJoin: ('y -> Stream<'z> -> #Job<Cons<'z>>) -> ('x -> 'y) -> Stream<'x> -> Stream<'z>

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

  /// Returns a stream that discards elements from the given stream until the
  /// given alternative is committed to after which the remainder of the given
  /// stream is produced.  Note that `append (takeUntil evt xs) (skipUntil evt
  /// xs)` may not be equivalent to `xs`, because there is an inherent
  /// race-condition.  See also: `takeAndSkipUntil`.
  val skipUntil: Alt<_> -> Stream<'x> -> Stream<'x>

  /// Returns a pair of streams of which the first one takes elements from the
  /// given stream and the second skips elements from the given stream until the
  /// given alternative is committed to.  It is guaranteed that
  /// `takeAndSkipUntil evt xs |> fun (hs, ts) -> append hs ts` is equivalent to
  /// `xs`.  See also: `skipUntil`, `takeUntil`.
  val takeAndSkipUntil: Alt<_> -> Stream<'x> -> Stream<'x> * Stream<'x>

  /// Returns a stream that produces elements from the given stream until the
  /// given alternative is committed to after which the returned stream is
  /// closed.  Note that `append (takeUntil evt xs) (skipUntil evt xs)` may not
  /// be equivalent to `xs`, because there is an inherent race-condition.  See
  /// also: `takeAndSkipUntil`.
  val takeUntil: Alt<_> -> Stream<'x> -> Stream<'x>

  /// Returns the stream without the maximal prefix of elements that satisfy the
  /// given predicate given as a job.
  val skipWhileJob: ('x -> #Job<bool>) -> Stream<'x> -> Stream<'x>

  /// Returns the stream without the maximal prefix of elements that satisfy the
  /// given predicate given as a function.
  val skipWhileFun: ('x -> bool) -> Stream<'x> -> Stream<'x>

  /// Returns the maximal prefix of the given stream of elements that satisfy
  /// the given predicate given as a job.
  val takeWhileJob: ('x -> #Job<bool>) -> Stream<'x> -> Stream<'x>

  /// Returns the maximal prefix of the given stream of elements that satisfy
  /// the given predicate given as a function.
  val takeWhileFun: ('x -> bool) -> Stream<'x> -> Stream<'x>

  // Exceptions

  /// Returns a stream that produces the same sequence of elements as the given
  /// stream.  If the given stream fails, a new stream is constructed by calling
  /// the given function and that stream becomes the remainder of the stream.
  val catch: (exn -> #Stream<'x>) -> Stream<'x> -> Stream<'x>

  /// Returns a stream that is just like the given stream except that just
  /// before the returned stream is closed, due to the given stream being
  /// closed, whether with an error or without, the given job is executed.  In
  /// case the job raises an exception, that exception closes the returned
  /// stream.  See also: `onCloseFun`, `doFinalizeJob`.
  val onCloseJob: Job<unit> -> Stream<'x> -> Stream<'x>

  /// Returns a stream that is just like the given stream except that just
  /// before the returned stream is closed, due to the given stream being
  /// closed, whether with an error or without, the given function is called.
  /// In case the function raises an exception, that exception closes the
  /// returned stream.  See also: `onCloseJob`, `doFinalizeFun`.
  val onCloseFun: (unit -> unit) -> Stream<'x> -> Stream<'x>

  /// Returns a stream that is just like the given stream except that after
  /// the returned stream is closed or becomes garbage, the given job is
  /// started as a separate concurrent job.  See also: `doFinalizeFun`,
  /// `onCloseJob`.
  val doFinalizeJob: Job<unit> -> Stream<'x> -> Stream<'x>

  /// Returns a stream that is just like the given stream except that after
  /// the returned stream is closed or becomes garbage, a separate job is
  /// started that calls the given function.  See also: `doFinalizeJob`,
  /// `onCloseFun`.
  val doFinalizeFun: (unit -> unit) -> Stream<'x> -> Stream<'x>

  // Lazifying

  /// Functions for collecting elements from a live stream to be lazified.
  type [<AbstractClass>] KeepPrecedingFuns<'x, 'y> =
    /// Empty constructor.
    new: unit -> KeepPrecedingFuns<'x, 'y>

    /// Called to begin the next batch of elements.
    abstract First: 'x -> 'y

    /// Called to add an element to the current batch.
    abstract Next: 'y * 'x -> 'y

  /// Converts a given imperative live stream into a lazy stream by spawning a
  /// job to eagerly consume and collect elements from the live stream using the
  /// given `KeepPrecedingFuns<_, _>` object.
  val keepPrecedingFuns: KeepPrecedingFuns<'x, 'y> -> Stream<'x> -> Stream<'y>

  /// Converts a given imperative live stream into a lazy stream of queued
  /// elements by spawning a job to eagerly consume and queue elements from the
  /// live stream.  At most `maxCount` most recent elements are kept in a queue
  /// and after that the oldest elements are thrown away.  See also:
  /// `keepPreceding1`.
  val keepPreceding: maxCount: int -> Stream<'x> -> Stream<Queue<'x>>

  /// Converts an imperative live stream into a lazy stream by spawning a job to
  /// eagerly consume (and throw away) elements from the live stream and to
  /// produce only one most recent element each time when requested.  See also:
  /// `pullOn`, `keepPreceding`, `keepFollowing1`.
#if DOC
  ///
  /// Basically,
  ///
  ///> live |> keepPreceding1 |> afterEach timeout
  ///
  /// is similar to
  ///
  ///> live |> ignoreWhile timeout
  ///
  /// and
  ///
  ///> live |> keepPreceding1 |> beforeEach timeout
  ///
  /// is similar to
  ///
  ///> live |> ignoreUntil timeout
  ///
  /// However, a lazified stream, assuming there is enough CPU time to consume
  /// elements from the live stream, never internally holds to an arbitrary
  /// number of stream conses.
#endif
  val keepPreceding1: Stream<'x> -> Stream<'x>

  /// Converts an imperative live stream into a lazy stream by spawning a job to
  /// eagerly consume (and throw away) elements from the live stream and to
  /// produce only one element after each pull request.  See also: `pullOn`,
  /// `keepPreceding1`.
  val keepFollowing1: Stream<'x> -> Stream<'x>

  /// Given a stream of ticks and a lazy stream of elements returns a stream of
  /// elements pulled from the lazy stream based on the ticks.  See also:
  /// `keepPreceding1`, `keepFollowing1`.
#if DOC
  ///
  /// For example,
  ///
  ///> live |> keepPreceding1 |> pullOn ticks
  ///
  /// is similar to
  ///
  ///> live |> samplesBefore ticks
  ///
  /// and
  ///
  ///> live |> keepFollowing1 |> pullOn ticks
  ///
  /// is similar to
  ///
  ///> live |> samplesAfter ticks
  ///
  /// However, a lazified stream, assuming there is enough CPU time to consume
  /// elements from the live stream, never internally holds to an arbitrary
  /// number of stream conses.
  ///
  /// `pullOn ts xs` is equivalent to `zipWithFun (fun _ x -> x) ts xs`.
#endif
  val pullOn: ticks: Stream<_> -> Stream<'x> -> Stream<'x>

  // Timing

  /// `debounce timeout elements` returns a stream so that after each element a
  /// timeout is started and the element is produced if no other elements is
  /// received before the timeout is signaled.  Note that if the given stream
  /// produces elements more frequently than the timeout, the returned stream
  /// never produces any elements.
#if DOC
  ///
  ///> elements: 1        2 3  4     5 6 7 8 9 ...
  ///>  timeout: +---x    +-+--+---x +-+-+-+-+-...
  ///>   output:     1             4
#endif
  val debounce: timeout: Alt<_> -> Stream<'x> -> Stream<'x>

  /// `samplesBefore ticks elements` returns a stream that consumes both ticks
  /// and elements and produces each element that precedes a tick.  Excess
  /// elements from both streams are skipped.
#if DOC
  ///
  ///> elements: 1  2  3        4 5 6  7
  ///>    ticks:     x    x    x     x   x
  ///>   output:     2    3          6   7
#endif
  val samplesBefore: ticks: Stream<_> -> Stream<'x> -> Stream<'x>

  /// `samplesAfter ticks elements` returns a stream that consumes both ticks
  /// and elements and produces each element that follows a tick.  Excess
  /// elements from both streams are skipped.
#if DOC
  ///
  ///> elements: 1  2  3        4 5 6  7
  ///>    ticks:     x    x    x     x   x
  ///>   output:       3        4      7
#endif
  val samplesAfter: ticks: Stream<_> -> Stream<'x> -> Stream<'x>

  /// `ignoreUntil timeout elements` returns a stream that, after getting an
  /// element, starts a timeout and produces the last element received when the
  /// timeout is signaled.
#if DOC
  ///
  ///> elements: 1   2      3       4 5 6
  ///> timeouts: +-----x    +-----x +-----x
  ///>   output:       2          3       6
#endif
  val ignoreUntil: timeout: Job<_> -> Stream<'x> -> Stream<'x>

  /// `ignoreWhile timeout elements` returns a stream that, after getting an
  /// element, starts a timeout, produces the element and ignores other elements
  /// until the timeout is signaled.
#if DOC
  ///
  ///> elements: 1   2      3       4 5 6
  ///> timeouts: +-----x    +-----x +-----x
  ///>   output: 1          3       4
#endif
  val ignoreWhile: timeout: Job<_> -> Stream<'x> -> Stream<'x>

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
  /// the returned stream also fails.  `delayEach yJ xs` is equivalent to
  /// `zipWithFun (fun x _ -> x) xs (indefinitely yJ)`.  See also: `shift`.
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
  val delayEach: timeout: Job<_> -> Stream<'x> -> Stream<'x>

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
  val afterEach: timeout: Job<_> -> Stream<'x> -> Stream<'x>

  /// Returns a stream that runs the given job each time a value is requested
  /// before requesting the next value from the given stream.  If the given job
  /// fails, the returned stream also fails.  `beforeEach yJ xs` is equivalent
  /// to `pullOn (indefinitely yJ) xs`.
  val beforeEach: timeout: Job<_> -> Stream<'x> -> Stream<'x>

  /// Given a stream of dates, returns a stream that produces the dates after
  /// the dates.
  val afterDateTimeOffsets: Stream<DateTimeOffset> -> Stream<DateTimeOffset>

  /// Returns a stream that produces the given date after the given date.
  val afterDateTimeOffset: DateTimeOffset -> Stream<DateTimeOffset>

  /// Returns a stream that produces an element after the given time span.  Note
  /// that streams are memoized.
  val afterTimeSpan: TimeSpan -> Stream<unit>

  // Eliminating streams

  /// Returns a job that collects all the elements from the stream.  This
  /// function is provided for testing purposes.
  val toSeq: Stream<'x> -> Job<ResizeArray<'x>>

  /// Creates an alternative through which all the values of the stream
  /// generated after the point at which the alternative has been created can be
  /// read.  See also: `indefinitely`.
  val values: Stream<'x> -> Alt<'x>

  /// Eagerly reduces the given stream using the given job.  See also:
  /// `foldBack`.
  val foldJob: ('s -> 'x -> #Job<'s>) -> 's -> Stream<'x> -> Job<'s>

  /// Eagerly reduces the given stream using the given function.
  val foldFun: ('s -> 'x -> 's) -> 's -> Stream<'x> -> Job<'s>

  /// `foldFromJob s sx2sJ xs` is equivalent to `foldJob sx2sJ s xs` and is
  /// often syntactically more convenient to use.
  val foldFromJob: 's -> ('s -> 'x -> #Job<'s>) -> Stream<'x> -> Job<'s>

  /// `foldFromFun s sx2s xs` is equivalent to `foldFun sx2s s xs` and is often
  /// syntactically more convenient to use.
  val foldFromFun: 's -> ('s -> 'x -> 's) -> Stream<'x> -> Job<'s>

  /// Performs a lazy backwards fold over the stream.  See also: `foldJob`,
  /// `unfoldJob`.
#if DOC
  ///
  /// `foldBack` is a fundamental function on streams.  Consider that `foldBack
  /// cons xs nil` is equivalent to `xs`.  Many other stream functions can be
  /// implemented using `foldBack`.  For example, `mapJob` could be defined
  /// using `foldBack` as follows:
  ///
  ///> let mapJob x2yJ xs =
  ///>   foldBack (fun x s -> x2yJ x >>=* fun y -> cons y s) xs nil
  ///
  /// Reference implementation:
  ///
  ///> let rec foldBack x2s2sJ xs s =
  ///>   xs >>=* function Nil -> s
  ///>                  | Cons (x, xs) -> x2s2sJ x (foldBack x2s2sJ xs s)
#endif
  val foldBack: ('x -> Promise<'s> -> 'sJ)
             -> Stream<'x>
             -> 'sJ
             -> Promise<'s> when 'sJ :> Job<'s>

  /// Returns a job that iterates the given job constructor over the given
  /// stream.  See also: `consumeJob`.
#if DOC
  ///
  /// Reference implementation:
  ///
  ///> let rec iterJob x2uJ xs =
  ///>   xs >>= function Nil -> Job.unit ()
  ///>                 | Cons (x, xs) -> x2uJ x >>. iterJob x2uJ xs
#endif
  val iterJob: ('x -> #Job<unit>) -> Stream<'x> -> Job<unit>

  /// Returns a job that iterates the given function over the given stream.  See
  /// also: `iterJob`, `consumeFun`.
  val iterFun: ('x -> unit) -> Stream<'x> -> Job<unit>

  /// Returns a job that iterates over all the elements of the given stream.
  /// `iter xs` is equivalent to `iterFun ignore xs`.  See also: `consume`.
  val iter: Stream<'x> -> Job<unit>

  /// `xs |> consumeJob x2uJ` is equivalent to `xs |> iterJob x2uJ |> queue`.
  val consumeJob: ('x -> #Job<unit>) -> Stream<'x> -> unit

  /// `xs |> consumeFun x2u` is equivalent to `xs |> iterFun x2u |> queue`.
  val consumeFun: ('x -> unit) -> Stream<'x> -> unit

  /// `xs |> consume` is equivalent to `xs |> iter |> queue`.
  val consume: Stream<'x> -> unit

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

  /// `tailsMapFun xs2y xs` is equivalent to `tails xs |> mapFun xs2y`.
  val tailsMapFun: (Stream<'x> -> 'y) -> Stream<'x> -> Stream<'y>

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

  /// `initsMapFun xs2y xs` is equivalent to `inits xs |> mapFun xs2y`.
  val initsMapFun: (Stream<'x> -> 'y) -> Stream<'x> -> Stream<'y>

  /// An experimental generic builder for streams.  The abstract `Plus` and
  /// `Zero` operations need to be implemented in a derived class.  The
  /// operations are then used to implement `Bind`, `Combine`, `For` and `While`
  /// to get a builder with consistent semantics.
  type [<AbstractClass>] Builder =
    new: unit -> Builder
    member inline Bind: Stream<'x> * ('x -> Stream<'y>) -> Stream<'y>
    member inline Combine: Stream<'x> * Stream<'x> -> Stream<'x>
    member inline Delay: (unit -> Stream<'x>) -> Stream<'x>
    abstract Zero: unit -> Stream<'x>
    member inline For: seq<'x> * ('x -> Stream<'y>) -> Stream<'y>
    member inline TryWith: Stream<'x> * (exn -> Stream<'x>) -> Stream<'x>
    member While: (unit -> bool) * Stream<'x> -> Stream<'x>
    member inline Yield: 'x -> Stream<'x>
    member inline YieldFrom: Stream<'x> -> Stream<'x>
    abstract Plus: Stream<'x> * Stream<'x> -> Stream<'x>

  /// This builder joins substreams with `append` to produce a stream with all
  /// results in sequential order.
  val appended: Builder

  /// This builder joins substreams with `merge` to produce a stream with all
  /// results in completion order.
  val merged: Builder

  /// This builder joins substreams with `amb` to produce a stream with the
  /// first results.
  val ambed: Builder

  /// This builder joins substreams with `switch` to produce a stream with the
  /// latest results.
  val switched: Builder
