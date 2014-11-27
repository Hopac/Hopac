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
*framework* rather than in the client program.  Adhering to the Hollywood
-principle, the event streams call your callbacks and not the other way around.
This makes it difficult to directly match control flow to program state.
Callbacks examine and modify mutable program state and the resulting program can
be difficult to understand and reason about.

One of the primary motivations for using lightweight threads for user interfaces
has been that they have allowed one to invert back the inversion of control
caused by events so that the control will be in client code.  Once simple
control flow is recovered, it is possible to directly match control flow with
program state, allowing the use of simple programming techniques such as lexical
binding, recursion, and immutable data structures that are easy to understand
and reason about.

In this note we will show that the selective synchronous operations aka
alternatives of Hopac are powerful enough to invert back the inversion of
control caused by event stream combinators.  The discovery of this simple
programming technique was triggered when Lev Gorodinski asked how "to do
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
streams have no concept of time.  Fortunately, to recover a concept of time, we
can simply replace the `Lazy` type constructor with the `Alt` type constructor:

```fsharp
type Stream<'x> =
  | Nil
  | Cons of Value: 'x * Next: Streams<'x>
and Streams<'x> = Alt<Stream<'x>>
```

We'll refer to the above kind of streams as *choice streams*.

It is straightforward to convert all lazy stream combinators to choice stream
combinators.  For example, given a suitable defined bind operation `>>=`

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
choice streams and using combinators such as `Alt.choose` it is possible to
construct streams that depend on time.

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
However, there is one problem with the above formulation.  Can you spot it?

The above version of `merge` produces an *ephemeral* stream: it could produce
different results each time it is examined.  We don't want that, because it
could lead to nasty inconsistencies.  So, in practice, when writing choice
stream combinators, we will make sure that the we *memoize* the streams.

To memoize choice streams, we introduce a couple of auxiliary memoizing
combinators:

```fsharp
let inline memo x = Promise.Now.delayAsAlt x
let inline (>>=*) x f = x >>= f |> memo
let inline (|>>*) x f = x |>> f |> memo
let inline (<|>*) x y = x <|> y |> memo
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

What about `append`?  If we assume that both the streams given to `append` have
already been memoized, then the resulting stream will always be the same.
Avoiding memoization can bring some performance benefits, but we can also write
a memoizing version of `append` as follows:

```fsharp
let rec append (ls: Streams<'x>) (rs: Streams<'x>) : Streams<'x> =
  ls >>=* function Nil -> upcast rs
                 | Cons (l, ls) -> cons l (append ls rs)
```

As can actually be seen from the two examples already, given a choice stream or
any number of choice streams, one can process elements from such a stream by
means of a simple loop.

When multiple streams need to be processed concurrently, one can spawn a
separate lightweight thread (or job) for each such concurrent activity.

Choice streams are lazy.  Once you stop pulling elements from a stream and the
variable referring to the stream is no longer reachable, the stream can be
garbage collected.

So, basically, with choice streams we can now use functional programming to both
combine event streams and also use functional programming to consume streams.
With Rx, we can use functional programming techniques to combine streams, but
must then resort to imperative programming to consume streams.

## Further

There is an experimental library based on the above technique.  See:

* [Streams.fsi](https://github.com/VesaKarvonen/Hopac/blob/master/Libs/Hopac.Experimental/Streams.fsi)
* [Streams.fs](https://github.com/VesaKarvonen/Hopac/blob/master/Libs/Hopac.Experimental/Streams.fs)

## Related work

Tomas Petricek has previously introduced the concept of
[F# asynchronous sequences](http://tomasp.net/blog/async-sequences.aspx/).
Building on top of the asynchronous workflows or `async` of F# Petricek's
streams fail to capture the full power of event streams.  The F# `async`
mechanism does not directly provide for a non-deterministic choice operator or
memoization (promises or futures).  In particular, asynchronous sequences do not
provide a `merge` combinator.  Hopac directly provides both a choice operator
and promises, which makes it straightforward to extend the power of streams.
