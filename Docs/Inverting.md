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
This makes it difficult to precisely match control flow and program state.
There is no simple connection between control flow and possible program states.
As a result, callbacks examine and modify mutable program state and the
resulting program can be difficult to understand and reason about.

One of the primary motivations for using lightweight threads for user interfaces
has been that they have allowed one to invert back the inversion of control
caused by events so that the control is in client code.  Once simple control
flow is recovered, it is possible to directly match control flow with program
state, allowing the use of simple programming techniques such as lexical
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
consider what Rx is all about.

```fsharp
type IObservable<'x> =
  abstract Subscribe: IObserver<'x> -> IDisposable
```

```fsharp
type IObserver<'x> =
  abstract OnCompleted: unit -> unit
  abstract OnError: exn -> unit
  abstract OnNext: 'x -> unit
```

