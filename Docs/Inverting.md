# Inverting Event Streams is a Choice with Concurrent Jobs

Concurrent ML, on which Hopac is based, is a concurrent programming language
designed by John Reppy in late 80s and early 90s.  The primary motivation for
the design of Concurrent ML was programming interactive systems such as
graphical user interfaces.  The hypothesis was that interactive systems are
naturally concurrent and programming them in sequential languages results in
awkward program structures.  As it happened, use of concurrent languages for
programming user interfaces did not gain much traction.  Instead, one of the
recent developments in user interface programming has been the introduction of
*event stream combinators*.  A prominent example of an event stream combinator
library would be
[The Reactive Extensions aka Rx](http://msdn.microsoft.com/en-us/data/gg577609.aspx).

Event streams are a powerful abstraction and can significantly help to reduce
the so called *callback hell*.  Nevertheless, event stream combinators do not
fundamentally change the fact that control is still in the event processing
*framework* rather than in the client program.  Adhering to the *Hollywood
-principle*, the event streams call your callbacks and not the other way around.
This makes it difficult to directly match control flow to program state.
Callbacks examine and *modify mutable program state* and the resulting program
can be difficult to understand and reason about.

One of the primary motivations for using lightweight threads for user interfaces
has been that they have allowed one to invert back the inversion of control
caused by events so that the control will be in client code.  Once simple
control flow is recovered, it is possible to directly match control flow with
program state, allowing the use of simple programming techniques such as
*lexical binding*, *recursion*, and *immutable data* structures that are easy to
understand and reason about.

In this note we will show that the selective synchronous operations aka
alternatives of Hopac are powerful enough to invert back the inversion of
control caused by event stream combinators.  The discovery of this _simple_
programming technique was triggered when Lev Gorodinski
[asked](https://twitter.com/eulerfx/status/534014908095287296) how "to do
non-deterministic merge" for a Hopac based sequence he was working on.  This
technique differs from Lev Gorodinski's approach only in that the streams are
directly represented as selective synchronous operations that are memoized.

## About Rx style Event Stream Combinators

Before we look at how event streams can be inverted, let's take a moment to
consider what Rx is all about.  We use Rx as an example, but pretty much any
event stream combinator library would do.  There are two central interfaces in
Rx. `IObservable`[*](http://msdn.microsoft.com/en-us/library/dd990377%28v=vs.103%29.aspx)

```fsharp
type IObservable<'x> =
  abstract Subscribe: IObserver<'x> -> IDisposable
```

represents a source or stream of events and
`IObserver`[*](http://msdn.microsoft.com/en-us/library/dd783449%28v=vs.110%29.aspx)

```fsharp
type IObserver<'x> =
  abstract OnCompleted: unit -> unit
  abstract OnError: exn -> unit
  abstract OnNext: 'x -> unit
```

represents a sink or a callback attached to an `IObservable`.

The power of Rx comes from a large number of
[combinators for observables](http://msdn.microsoft.com/en-us/library/system.reactive.linq.observable%28v=vs.103%29.aspx).

Using the Rx combinators, one can merge, filter, append, map, throttle, ... and
more to produce a stream of events that our application is interested in.  This
is quite nice, because such stream combinations can be declarative without any
direct use of shared mutable state.  Ultimately one has to *subscribe* a
callback to be called on events from the combined stream.  However, this also
means that Rx calls the callback function, which must then return to Rx.  So, in
practice, the callback function must be programmed in terms of shared mutable
state.

Could we invert back the inversion of control, allowing the use of programming
techniques such as lexical binding, recursion and immutable data structures,
while also supporting powerful combinators for combining events?

## Inverting Event Streams

An intrinsic part of the education of a functional programmer is the
introduction of the concept of *lazy streams*.  There are many ways to design
such lazy streams.  Here is one:

```fsharp
type Stream<'x> =
  | Nil
  | Cons of Value: 'x * Next: Streams<'x>
and Streams<'x> = Lazy<Stream<'x>>
```

All the combinators that an event stream combinator library like Rx supports can
easily be implemented for lazy streams&mdash;except for the fact that lazy
streams have no concept of time or choice.  Fortunately, to recover a concept of
time or choice, we can simply replace the `Lazy<_>` type constructor with the
`Alt<_>`[*](http://hopac.github.io/Hopac/Hopac.html#def:type%20Hopac.Alt)
type constructor:

```fsharp
type Stream<'x> =
  | Nil
  | Cons of Value: 'x * Next: Streams<'x>
and Streams<'x> = Alt<Stream<'x>>
```

We'll refer to the above kind of streams as *choice streams*, because they
support a kind of non-deterministic choice via the `Alt<_>` type constructor.

It is straightforward to convert all lazy stream combinators to choice stream
combinators.  For example, given a suitably defined bind operation `>>=`

```fsharp
let (>>=) (xL: Lazy<'x>) (x2yL: 'x -> Lazy<'y>) : Lazy<'y> =
  lazy (x2yL (xL.Force ())).Force ()
```

for the `Lazy<_>` type constructor and a lazy `cons` helper function

```fsharp
let cons x xs = lazy Cons (x, xs)
```

we could write lazy stream `append` as

```fsharp
let rec append (ls: Streams<'x>) (rs: Streams<'x>) : Streams<'x> =
  ls >>= function Nil -> rs
                | Cons (l, ls) -> cons l (append ls rs)
```

Similarly, given a suitably defined `cons` helper function

```fsharp
let cons x xs = Job.result (Cons (x, xs))
```

we can give an implementation of `append` for choice streams:

```fsharp
let rec append (ls: Streams<'x>) (rs: Streams<'x>) : Streams<'x> =
  ls >>=? function Nil -> upcast rs
                 | Cons (l, ls) -> cons l (append ls rs)
```

However, what is really important is that time is a part of the representation
of choice streams and using combinators such as
`Alt.choose`[*](http://hopac.github.io/Hopac/Hopac.html#def:val%20Hopac.Alt.choose)
and
`<|>?`[*](http://hopac.github.io/Hopac/Hopac.html#def:val%20Hopac.Alt.Infixes.%3C|%3E?)
it is possible to construct streams that may include timed operations and make
non-deterministic choices between multiple streams.

Here is a first attempt at implementing a `merge` combinator:

```fsharp
let rec merge (ls: Streams<'x>) (rs: Streams<'x>) : Streams<'x> =
  mergeSwap ls rs <|>? mergeSwap rs ls
and mergeSwap (ls: Streams<'x>) (rs: Streams<'x>) : Streams<'x> =
  ls |>>? function
     | Nil -> upcast rs
     | Cons (l, ls) -> cons l (merge rs ls)
```

There is no nice way to write the above kind of `merge` for lazy streams.
However, there is one problem with the above implementation.  Can you spot it?

The above version of `merge` produces an *ephemeral* stream: it could produce
different results each time it is examined.  We don't want that, because it
could lead to nasty inconsistencies when the stream is consumed by multiple
clients.  So, in practice, when writing choice stream combinators, we will make
sure that streams always produce the same results and in this case we can
*memoize* the stream.

To memoize choice streams, we introduce a couple of auxiliary memoizing
combinators using the lazy
`Promise.Now.delayAsAlt`[*](http://hopac.github.io/Hopac/Hopac.html#def:val%20Hopac.Promise.Now.delayAsAlt)
combinator:

```fsharp
let memo x = Promise.Now.delayAsAlt x
let (>>=*) x f = x >>= f |> memo
let (|>>*) x f = x |>> f |> memo
let (<|>*) x y = x <|> y |> memo
```

Using the above memoizing choice combinator, `<|>*`, we can now implement a
memoized `merge` as:

```fsharp
let rec merge (ls: Streams<'x>) (rs: Streams<'x>) : Streams<'x> =
  mergeSwap ls rs <|>* mergeSwap rs ls
and mergeSwap (ls: Streams<'x>) (rs: Streams<'x>) : Streams<'x> =
  ls |>>? function
     | Nil -> upcast rs
     | Cons (l, ls) -> cons l (merge rs ls)
```

What about `append`?  If we assume that both streams given to `append` already
always produce the same results, then the resulting stream will always be the
same.  Avoiding memoization can bring some performance benefits, but we can also
write a memoizing version of `append` as follows:

```fsharp
let rec append (ls: Streams<'x>) (rs: Streams<'x>) : Streams<'x> =
  ls >>=* function Nil -> upcast rs
                 | Cons (l, ls) -> cons l (append ls rs)
```

As can actually be seen from the above two examples already, given a choice
stream or any number of choice streams, one can process elements from such a
stream by means of a simple loop that essentially pulls elements from the choice
stream or streams.  When multiple streams need to be processed concurrently, one
can spawn a separate lightweight thread (or job) for each such concurrent
activity.

## Stream producers

Stream producers can be written in various ways.  One way is to write a loop
that simply constructs the stream using lazy promises&mdash;just like the
previously shown stream combinators do.  For example, a sequence can be lazily
converted to a choice stream using the below `ofSeq` function:

```fsharp
let rec ofEnum (xs: IEnumerator<'x>) = memo << Job.thunk <| fun () ->
  if xs.MoveNext () then
    Cons (xs.Current, ofEnum xs)
  else
    xs.Dispose ()
    Nil

let ofSeq (xs: seq<_>) = memo << Job.delay <| fun () ->
  upcast ofEnum (xs.GetEnumerator ())
```

Another way is to represent the tail of a choice stream using a write once
variable aka an
`IVar<_>`[*](http://hopac.github.io/Hopac/Hopac.html#def:type%20Hopac.IVar)
and have the producer
`fill`[*](http://hopac.github.io/Hopac/Hopac.html#def:val%20Hopac.IVar.fill)
that write once variable with a new stream node (containing a new write once
variable).  This way one can convert ordinary imperative event sources to choice
streams.  For example, the following scoped `subscribingTo` function subscribes
to an `IObservable<_>` and calls a job constructor with the resulting choice
stream:

```fsharp
let subscribingTo (xs: IObservable<'x>) (xs2yJ: Streams<'x> -> #Job<'y>) = job {
  let streams = ref (ivar ())
  use unsubscribe = xs.Subscribe {new IObserver<_> with
    override this.OnCompleted () = !streams <-= Nil |> start
    override this.OnError (e) = !streams <-=! e |> start
    override this.OnNext (value) =
      let next = ivar ()
      !streams <-= Cons (value, next) |> start
      streams := next}
  return! !streams |> xs2yJ :> Job<_>
}
```

Choice stream combinators are lazy.  Once a stream consumer stops pulling
elements from the stream and the variable referring to the stream is no longer
reachable, the stream can be garbage collected.  Once a stream producer is
garbage collected, threads waiting on the end of the associated stream can be
garbage collected.

Errors in choice streams are handled in the usual way.  The `memo` combinator we
made above uses a
`Promise`[*](http://hopac.github.io/Hopac/Hopac.html#def:type%20Hopac.Promise)
underneath.  If a choice stream producer raises an exception, it will be
captured by a promise and ultimately reraised when the promise is examined by a
choice stream consumer.

As can also be seen from the above examples, choice stream combinators look
quite familiar.  A functional programmer should not find it difficult to write
new combinators based on existing combinators or even to write new combinators
from scratch.

So, basically, with choice streams we can use functional programming to combine
choice streams and we can also use functional programming to consume choice
streams.  With Rx, we can use functional programming to combine event streams,
but we must then resort to imperative programming to consume event streams.  To
put it another way, with choice streams, you can have your cake and eat it too.
With Rx, you can have your cake, and then let Rx feed it to you.

## How to GroupBy without being pushy?

Is that all?  Can lazy *pull-based* choice streams do everything that
*push-based* Rx does?  In a recent
[talk](https://www.youtube.com/watch?v=pOl4E8x3fmw), Erik Meijer
[said](https://www.youtube.com/watch?v=pOl4E8x3fmw#t=36m42s) that

> if you're doing `groupBy` in a pull-based way [blah blah] buffer [blah blah]
> deadlock [blah blah] so, I don't know how to do `groupBy`

I've been thinking about interactive and reactive programming for a few weeks
now and I have to agree with Meijer that `groupBy` is difficult to do in a
pull-based manner.  So, is it possible?  Can we do `groupBy` in the lazy
pull-based manner in which choice streams operate?  Yes, we can.

Compared to the previously seen choice stream combinators, `groupBy` is
significantly more complicated.  Let's first look at the signature of `groupBy`:

```fsharp
val groupBy: keyOf: ('x -> 'k)
          -> Streams<'x>
          -> Streams<'k * Streams<'x>> when 'k: equality
```

`groupBy` is not just a simple transform on a fixed number of choice streams.
`groupBy` has to be able to incrementally create and return new streams based on
the elements pulled from the source stream.

What does `groupBy` really do?  It basically categorizes or filters elements
from the source stream into the groups.  However, we can't return new streams as
simple recursive loops filtering elements from the source stream, because that
would be inefficient.  We need to make it so that each element is categorized
only once.  Fortunately we already saw in the previous section how to create a
stream using write once variables.  We create a new stream tail variable for
each stream that `groupBy` returns and put those tails into a dictionary.  The
code that categorizes elements from the source stream then pushes elements to
those streams updating the dictionary.

The first instinct on where and how to do such categorizing might be to spawn a
concurrent job to pull the source stream and then push to the target streams.
But we can't have a concurrent job in `groupBy` eagerly pulling from the source
stream, because that would break the laziness and would cause a space-time leak.
Even if one would stop pulling elements from all the streams returned by
`groupBy` the categorizing process would remain live.  Instead of spawning a
dedicated process for categorizing elements, we need to make it so that pull
operations on the streams returned by `groupBy` co-operate.

To make such co-operation possible, we put the source stream into a serialized
variable[*](http://hopac.github.io/Hopac/Hopac.html#def:type%20Hopac.MVar)
to make it so that at most one process holds the reference to the original
stream at any time.  When a pull operation on a returned stream is started, an
attempt is made to take the source stream from the serialized variable or to
just take the next element from the readily categorized stream.  If the pull
operation gets the source stream, it then serves all the streams until it gets
the categorized element it desired.

So, let's finally look at the code with some additional comments:

```fsharp
let groupBy (keyOf: 'x -> 'k) (ss: Streams<'x>) : Streams<'k * Streams<'x>> =
  // Dictionary to hold the tail write once vars of categorized streams:
  let key2branch = Dictionary<'k, IVar<Stream<'x>>>()
  // Ref cell to hold the tail write once var of the main result stream:
  let main = ref (ivar ())
  // Serialized variable to hold the source stream:
  let baton = mvarFull ss
  // Shared code to propagate exception to all result streams:
  let raised e =
    key2branch.Values
    |> Seq.iterJob (fun i -> i <-=! e) >>.
    (!main <-=! e) >>! e
  // Shared code for the main and branch streams:
  let rec wrap self xs newK oldK =
    // Selective sync op to get next element from a specific stream:
    (xs >>=? function Nil -> nil | Cons (x, xs) -> cons x (self xs)) <|>*
    // Selective sync op to serve all the streams:
    (let rec serve ss =
       Job.tryIn ss
        <| function
            | Nil ->
              // Source stream closed, so close all result streams:
              key2branch.Values
              |> Seq.iterJob (fun i -> i <-= Nil) >>.
              (!main <-= Nil) >>% Nil
            | Cons (s, ss) ->
              tryIn <| fun () -> keyOf s
               <| fun k ->
                    match key2branch.TryGetValue k with
                     | Just i ->
                       // Push to previously created category:
                       let i' = ivar ()
                       key2branch.[k] <- i'
                       i <-= Cons (s, i') >>. oldK serve ss k s i'
                     | Nothing ->
                       // Create & push to main a new category:
                       let i' = ivar ()
                       let i = ivarFull (Cons (s, i'))
                       key2branch.Add (k, i')
                       let i' = ivar ()
                       let m = !main
                       main := i'
                       let ki = (k, wrapBranch k (i :> Streams<_>))
                       m <-= Cons (ki, i') >>. newK serve ss ki i'
               <| raised
        <| raised
     baton >>=? serve)
  // Wrapper for branch streams:
  and wrapBranch k xs =
    wrap (wrapBranch k) xs
     <| fun serve ss _ _ -> serve ss
     <| fun serve ss k' x xs ->
          // Did we get an element or do we continue serving?
          if k = k'
          then baton <<-= ss >>% Cons (x, wrapBranch k xs)
          else serve ss
  // Wrapper for the main stream:
  let rec wrapMain xs =
    wrap wrapMain xs
     <| fun _ ss ki i -> baton <<-= ss >>% Cons (ki, wrapMain i)
     <| fun serve ss _ _ _ -> serve ss
  // Return the main branch:
  !main |> wrapMain
```

That is a lot of code, but it is nothing compared to
[SelectMany.cs](https://github.com/mono/rx/blob/master/Rx/NET/Source/System.Reactive.Linq/Reactive/Linq/Observable/SelectMany.cs)
of Rx.  I plan to try and fit an implementation of choice streams that provides
functionality comparable to Rx in the number of lines in
[SelectMany.cs](https://github.com/mono/rx/blob/master/Rx/NET/Source/System.Reactive.Linq/Reactive/Linq/Observable/SelectMany.cs)
that contain only curly braces.

## GC'ing operations for composability

One interesting difference between Rx -style streams and choice streams is that
certain choice stream operations are garbage collected, like with traditional
purely functional streams, while their Rx equivalents aren't.

Consider a stream of the following kind:

```fsharp
append finiteStream otherStream
```

What happens when you consume the above stream and the `finiteStream` ends,
leaving only the `otherStream` to consume?

With choice streams the `append` operation simply returns the `otherStream` and
the append operation itself vanishes.  With Rx, events will continue being
passed through the `Concat` operation even though it does nothing.

This means that with Rx you cannot directly create new combinators by means of
recursively applying an operation to a stream.  Such a combinator would create a
space leak.  On the other hand, here is an example of such a combinator on
choice streams:

```fsharp
let rec mapJoin (join: Streams<'y> -> Streams<'z> -> Streams<'z>)
                (x2ys: 'x -> Streams<'y>)
                (xs: Streams<'x>) : Streams<'z> =
  xs >>=* function Nil -> nil
                 | Cons (x, xs) -> join (x2ys x) (mapJoin join x2ys xs)
```

As you can see, the binary `join` operation is applied recursively to the
stream.

This particular combinator makes it easy to generalize various binary
combinators on streams to work over streams of streams.  In particular, given
binary `amb`, `merge`, `append` and `switch` operations, we can generalize them
to work over streams of streams:

```fsharp
let ambMap x2ys xs = mapJoin amb x2ys xs
let mergeMap x2ys xs = mapJoin merge x2ys xs // SelectMany in Rx
let appendMap x2ys xs = mapJoin append x2ys xs
let switchMap x2ys xs = mapJoin switch x2ys xs
```

You may recall that in Rx, `s.SelectMany(f)` is sometimes defined to be
equivalent to `s.Select(f).Merge()`.  While this may seem just as nice, one
should understand that in Rx, the `Amb`, `Merge`, `Concat` and `Switch`
operations are primitively implemented to operate on streams of streams (except
for `Amb`, which is only implemented to operate on sequences of streams), while
with choice streams the much simpler binary versions will suffice.

## No wrapping locks over user code

One troublesome aspect of the Rx push approach is that Rx often needs to wrap
locks over user code, namely, the implementation of `OnNext`, `OnCompleted` and
`OnError` methods.  This is necessary to ensure that events are processed one at
a time.  Take a look at the `lock` statements in `SelectMany.cs`
e.g. [at line 229](https://github.com/mono/rx/blob/master/Rx/NET/Source/System.Reactive.Linq/Reactive/Linq/Observable/SelectMany.cs#L229).

This kind of locking is simply not necessary with pull based choice streams,
because the consumer is in control of requesting elements from the streams being
consumed.

Consider the following:

```fsharp
notifications
|> Streams.iterJob (fun n -> job {
     // show
     do! timeOutMillis 3000
     // hide
   })
```

The above choice stream snippet is a simplified version from an actual
application.  Its purpose is to display notifications to the user for 3 seconds
per notification.  These usually come at a very slow rate, but it might
occasionally happen that a few notifications would be posted less than 3 seconds
apart.  In such a case we still wish to display each notification for 3 seconds,
but we don't want to delay the display of notifications any further.

An attempt to convert it directly to Rx does not work:

```csharp
notifications
.Subscribe(async n => {
  // show
  await Task.Delay(3000);
  // hide
});
```

The problem is that Rx does not wait for the async callback to finish.  And it
really couldn't, because that would mean blocking the underlying thread and
stream.  With Rx, a more complicated stream needs to be used.  Here is one
potential stream:

```csharp
notifications
.Select(n =>
  Observable.Concat(
    Observable.Return(n),
    Observable.Delay(
      Observable.Return<Notification>(null),
      TimeSpan.FromSeconds(3))))
.Concat()
.Subscribe(n => {
  if (null == n) {
    // hide
  } else {
    // show
  }
});
```

It produces a stream with both the actual notifications and explicit `null`
values to signal when to hide the previous notifications.

## No direct support for disposables

There is a downside to the lack of an explicit unsubscribe protocol.  It means
that a choice stream combinator or producer gets no notification on when the
client is no longer going to request any more elements from the stream.
Operations such as `Finally` and `Using` cannot be expressed with the same
interface and semantics as with observables.

In some cases, the `onCloseJob` combinator may be sufficient:

```fsharp
let rec onCloseJob (uJ: Job<unit>) (xs: Streams<'x>) : Streams<'x> =
  Job.tryIn xs
     <| function Nil -> uJ >>% Nil
               | Cons (x, xs) -> upcast cons x (onCloseJob uJ xs)
     <| fun e -> uJ >>! e
  |> memo
```

It is similar to the Rx `Finally` in that once the stream is closed, the given
action is executed.  However, unlike with the Rx `Finally`, if the clients
consuming a stream returned by `onCloseJob` do not consume the stream until it
is closed, the action will not be performed.

Of course, it is also possible to implement an explicit notification interface
between a particular choice stream combinator or producer and its clients, but
such functionality is not built into the mechanism.

## Summary

Feature                    | Choice Streams    | Reactive Extensions  | Arguably better?
--------------------------:|:-----------------:|:--------------------:|:-----------------:
Approach                   | Lazy Pull         | Eager Push           | -
Merge                      | Yes               | Yes                  | -
GroupBy                    | Yes               | Yes                  | -
Compositions               | Declarative       | Declarative          | -
Consistent Streams         | Yes               | [No](http://nullzzz.blogspot.fi/2012/01/things-you-should-know-about-rx.html) | Choice streams
Consumers                  | Can be functional | Always imperative    | Choice streams
Fully GC'd                 | Yes               | No                   | Choice streams
Wraps locks over user code | No                | Yes                  | Choice streams
Directly supports dispose  | No                | Yes                  | Reactive Extensions

## Let's see some more code!

There is a library based on the above technique.  See:

* [Stream](http://hopac.github.io/Hopac/Hopac.html#def:module%20Hopac.Stream)
  module reference.
* [Stream.fsi](https://github.com/Hopac/Hopac/blob/master/Libs/Hopac/Stream.fsi)
* [Stream.fs](https://github.com/Hopac/Hopac/blob/master/Libs/Hopac/Stream.fs)

## Related work

Tomas Petricek has previously introduced the concept of
[F# asynchronous sequences](http://tomasp.net/blog/async-sequences.aspx/).
Building on top of the asynchronous workflows or `async` of F# Petricek's
streams fail to capture the full power of event streams.  The F# `async`
mechanism does not directly provide for a non-deterministic choice operator or
memoization (promises or futures).  In particular, asynchronous sequences do not
provide a `merge` combinator.  Hopac directly provides both a choice operator
and promises, which makes it straightforward to extend the power of streams.
