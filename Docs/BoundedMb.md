# A Case Study of Expressiveness: A Bounded Mailbox

*Bounded blocking queues* are a useful tool for coordinating work among
co-operating processes.  They provide *slack* and *lubrication* in the form of
buffering between producers and consumers to allow them to proceed in parallel
and to smooth out small variations in processing speeds.  They also provide
[back-pressure](http://ferd.ca/queues-don-t-fix-overload.html) in the form of
blocking producers when consumers cannot keep up.

The .Net concurrent collections includes the
[BlockingCollection](http://msdn.microsoft.com/en-us/library/dd267312%28v=vs.110%29.aspx)
type and Tomas Petricek has previously described an `async` `MailboxProcessor`
based
[BlockingQueueAgent](http://tomasp.net/blog/parallel-extra-blockingagent.aspx/).
In this note we describe an implementation of a similar `BoundedMb` abstraction
using Hopac.  We will argue that, of the three variations, the Hopac based
implementation is the shortest, simplest and also the most flexible.

## About the BlockingCollection

The `BlockingCollection` of .Net concurrent collections works at the level of
native threads.  This means that accessing a `BlockingCollection` may involve
blocking native threads.  Depending on the situation, this can be problematic.
One nice feature of `BlockingCollection` is that it supports timeouts and also
provides methods like
[AddToAny](http://msdn.microsoft.com/en-us/library/dd394986%28v=vs.110%29.aspx)
and
[TakeFromAny](http://msdn.microsoft.com/en-us/library/dd381962%28v=vs.110%29.aspx)
that allow for performing an operation on one of a given set of collections.

There is a reference implementation of
[BlockingCollection.cs](http://referencesource.microsoft.com/#System/sys/system/collections/concurrent/BlockingCollection.cs).
As can be easily seen, it is by far the longest of the three implementations.

## About the BlockingQueueAgent

The feature that distinguishes Petricek's `BlockingQueueAgent` from
`BlockingCollection` is that accessing a `BlockingQueueAgent` does not block
native threads.  However, `BlockingQueueAgent` does not provide for the ability
to attempt to add or get items from multiple queues like `BlockingCollection`.
On the other hand, `BlockingQueueAgent` does allow timeouts to be specified, but
the feature is arguably broken, because timeouts are only observed on the client
side.  Here is a sample interactive session, using
[BlockingQueueAgent.fs](https://github.com/tpetricek/FSharp.AsyncExtensions/blob/master/src/Agents/BlockingQueueAgent.fs),
that demonstrates the issue:

```fsharp
> let q = BlockingQueueAgent<int> (1) ;;
val q : BlockingQueueAgent<int>
> q.AsyncAdd 123 |> Async.RunSynchronously ;;
val it : unit = ()
> q.AsyncAdd (321, 1000) |> Async.RunSynchronously ;;
System.TimeoutException: MailboxProcessor.PostAndAsyncReply timed out.
> q.AsyncGet () |> Async.RunSynchronously ;;
val it : int = 123
> q.AsyncGet () |> Async.RunSynchronously ;;
val it : int = 321
```

As demonstrated above, even though the second `AsyncAdd` operation timed out,
the item was added to the queue.  One could say that this is just a bug in
`BlockingQueueAgent`, but I would rather argue that the bug is in F#'s `async`
and `MailboxProcessor` abstractions, because they don't really support this kind
of compositional programming.

## Hopac based BoundedMb

Before jumping to the implementation, let's take a look at the signature of
`BoundedMb`:

```fsharp
type BoundedMb<'x>

module BoundedMb =
  val create: capacity: int -> Job<BoundedMb<'x>>

  module Alt =
    val put: BoundedMb<'x> -> 'x -> Alt<unit>
    val take: BoundedMb<'x> -> Alt<'x>
```

Looking at the above signature, you may be wondering where is the support for
timeouts or operations like `AddToAny` of `BlockingCollection`.  The answer is
simply that special case support for those is not needed.  Hopac's alternative
mechanism[*](http://hopac.github.io/Hopac/Hopac.html#def:type%20Hopac.Alt)
already provides for those.  Because the `put` and `take` operations are
provided as alternatives, a client can selectively synchronize on any number of
operations on bounded mailboxes,
timeouts[*](http://hopac.github.io/Hopac/Hopac.html#def:val%20Hopac.Timer.Global.timeOut)
and other synchronous operations:

```fsharp
Alt.select [
  timeout >>=? ...
  BoundedMb.Alt.put xMb1 x >>=? ...
  BoundedMb.Alt.take xMb2 >>=? ...
]
```

This means that `BoundedMb` is fundamentally more flexible than
`BlockingCollection` in terms of composability with its own or other synchronous
operations.

Let's then look at the implementation of `BoundedMb`:

```fsharp
type BoundedMb<'x> = {putCh: Ch<'x>; takeCh: Ch<'x>}

module BoundedMb =
  let create capacity = Job.delay <| fun () ->
    let self = {putCh = ch (); takeCh = ch ()}
    let queue = Queue<_>()
    let put = self.putCh |>>? queue.Enqueue
    let take () = self.takeCh <-? queue.Peek () |>>? (queue.Dequeue >> ignore)
    let proc = Job.delay <| fun () ->
      match queue.Count with
       | 0 -> upcast put
       | n when n = capacity -> upcast (take ())
       | _ -> take () <|> put
    Job.foreverServer proc >>% self
  module Alt =
    let put xB x = xB.putCh <-? x
    let take xB = xB.takeCh :> Alt<_>
```

Take a moment to read through the above implementation.  It uses the basic
client-server programming technique.  `put` and `take` requests use separate
channels.  Depending on the state of the underlying queue, the server responds
to either only `get`, only `take` or both kinds of requests.  In a simple case
like this, cancellation is already taken care of by the synchronous nature of
channels[*](http://hopac.github.io/Hopac/Hopac.html#def:type%20Hopac.Ch).
In a more complex scenario, we would make use of the
`withNack`[*](http://hopac.github.io/Hopac/Hopac.html#def:val%20Hopac.Alt.withNack)
combinator.

Here is a sample interactive session with `BoundedMb`:

```fsharp
> let mb: BoundedMb<int> = BoundedMb.create 1 |> run ;;
val mb : BoundedMb<int>
> BoundedMb.Alt.put mb 123 |> run ;;
val it : unit = ()
> BoundedMb.Alt.put mb 321 <|> timeout 1.0 |> run ;;
val it : unit = ()
> BoundedMb.Alt.take mb |> run ;;
val it : int = 123
> BoundedMb.Alt.take mb |>>? printfn "Got %A" <|> timeout 1.0 |> run ;;
val it : unit = ()
```

## Conclusion

At the time of writing this, I have not tested `BoundedMb` rigorously.  Yet, I'm
fairly confident that it is essentially bug-free, because it is simple enough
that I can keep it in my head.

Here is an exercise for the reader.  Forget for a moment that you already know
that timeouts in `BlockingQueueAgent` are broken.  Then take the three
implementations, read through their source code and try to either convince
yourself that each implementation is correct or, alternatively, demonstrate a
bug in an implementation.  Which implementation was the easiest to understand
and reason about?

## A More Verbose Rendering of a BoundedMb

As the symbolic operators might be difficult to read at first, here is a version
that only uses the basic monadic bind operator (`>>=`).  All other operations
are done using non-symbolic variations.

```fsharp
type BoundedMb<'x> = {putCh: Ch<'x>; takeCh: Ch<'x>}

module BoundedMb =
  let create capacity = Job.delay <| fun () ->
    let self = {putCh = ch (); takeCh = ch ()}
    let queue = Queue<_>()
    let put = Alt.map queue.Enqueue (Ch.Alt.take self.putCh)
    let take () =
      Alt.map (queue.Dequeue >> ignore)
       (Ch.Alt.give self.takeCh (queue.Peek ()))
    let proc = Job.delay <| fun () ->
      match queue.Count with
       | 0 -> upcast put
       | n when n = capacity -> upcast (take ())
       | _ -> Alt.select [take (); put] // <|> is a bit faster
    Job.foreverServer proc >>= fun () ->
    Job.result self
  module Alt =
    let put xB x = Ch.Alt.give xB.putCh x
    let take xB = Ch.Alt.take xB.takeCh
```
