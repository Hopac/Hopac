Actors and Hopac
================

The subject of Actors seems to come up often when discussing Hopac.  Both Hopac
and actors are models of
[message passing](http://en.wikipedia.org/wiki/Message_passing) concurrency, but
what are actors and what kind of similarities and differences do actors and
Hopac have?

What are actors?
----------------

It is somewhat difficult to pin down what exactly actors are, because there are
many different incarnations of actors.  For example,
[Erlang](http://www.erlang.org/) is often called an actor language and
[Akka](http://akka.io/) could be called an actor library.  While there are
similarities between those two, there are also fundamental differences between
them and countless other incarnations of ``actors''.

One thing that is common to many incarnations of the
[actor model](http://en.wikipedia.org/wiki/Actor_model) is that the actor
concept within them combines both a thread of execution and a message queue or
mailbox of some kind.  The thread of execution and the mailbox are implicitly
tied together so that, within the thread of an actor, a *receive* statement of
some form implicitly takes messages from the mailbox associated with the actor.

To confuse the matter further, the
[MailboxProcessor](http://msdn.microsoft.com/en-us/library/ee370357.aspx)
abstraction, also often described as an actor model, provided with the F# async
model gives an explicit name to the mailbox.  However, the MailboxProcessor
model requires that there is at most one concurrent reader of a particular
MailboxProcessor active at any moment.  This means that a MailboxProcessor is
effectively associated with a thread of execution.

What is the model provided by Hopac?
------------------------------------

Unlike those actor models, Hopac and
[Concurrent ML](http://en.wikipedia.org/wiki/Concurrent_ML), which is the
inspiration behind Hopac, provide a model in which threads of execution and
means of communication are separate similar to the
[π-calculus](http://en.wikipedia.org/wiki/%CE%A0-calculus).  In Hopac, threads
of execution are called *jobs* and various communication primitives, such as,
synchronous *channels*, asynchronous *mailboxes*, synchronous write once *ivar*
and write many *mvar* variables are provided.  In addition, a form of *selective
communication* called *alternatives* is provided that allows the expression of
first-class synchronous abstractions.  The document
[Programming in Hopac](Programming.md) contains some discussion and examples on
Hopac programming techniques.

Unlike the way actor models combine a thread of execution and a mailbox into an
actor, in Hopac, a single concurrent job may be receiving messages via any
number of explicitly named channels&mdash;including the possibility of not using
any channels&mdash;and, on the other hand, a single channel may be used by any
number of jobs for receiving messages.

Essence of the models as concrete signatures
--------------------------------------------

It is often illustrative to give a signature for the essence of a computational
model.  Let's do that for Hopac and actors.

### The essence of Hopac

In Hopac, the threads of execution called jobs and concurrent operations are
encoded using monadic combinators.  Here is an illustrative signature sketch of
the essence of the programming model provided by Hopac:

```fsharp
module HopacModel =
  type Job<'x>
  val (>>=): Job<'x> -> ('x -> Job<'y>) -> Job<'y>
  val result: 'x -> Job<'x>
  val start: Job<unit> -> unit
  type Ch<'x>
  val ch: unit -> Ch<'x>
  val give: Ch<'x> -> 'x -> Job<unit>
  val take: Ch<'x> -> Job<'x>
```

Using the monadic operations, **>>=** and **result**, one can define a thread of
execution and such threads can be started with **start**.  Any number of
channels can be created using **ch** and different channels may have different
types.  Within a thread of execution one can then perform **give** and **take**
operations on channels.  Both **give** and **take** operations are synchronous
and block the executing job until the other party of the communication is
present and the operation can be completed.

Please note that the above signature snippet is not a precise subset of Hopac
and that Hopac provides a much more comprehensive model as described in the
[Hopac Library Reference](http://htmlpreview.github.io/?https://github.com/VesaKarvonen/Hopac/blob/master/Docs/Hopac.html)
manual.

### The essence of an actor model

We could also similarly specify the essence of an actor model.  Here is a sketch
of such a model:

```fsharp
module ActorModel =
  type ActorThread<'a, 'x>
  val (>>=): ActorThread<'a, 'x> -> ('x -> ActorThread<'a, 'y>) -> ActorThread<'a, 'y>
  val result: 'x -> ActorThread<'a, 'x>
  val receive: ActorThread<'a, 'a>
  type Actor<'a>
  val start: ActorThread<'a, unit> -> Actor<'a>
  val send: Actor<'a> -> 'a -> unit
```

In this model, one can similarly define a thread of execution using **>>=** and
**result**.  Within an actor thread, one can receive messages via the mailbox of
the actor using the **receive** operation.  Only messages from the single
mailbox implicit to the actor thread can be directly received from.  An actor
thread is started using **start**, which also returns a handle for sending
messages to the actor using the **send** function.  The **send** operation is
asynchronous and does not block.  If the destination of a **send** operation is
not immediately ready to receive the message, then that message is queued to the
mailbox of the actor.

Please note that the above sketch of an actor model does not attempt to
precisely capture any specific actor model such as Akka or Erlang.  Like the
actual model provided by the Hopac library and the model specified in the
HopacModel signature, the above ActorModel is also drastically simpler than many
actual actor models.

### Encoding the actor model using Hopac

It is often instructive to see how one model of computation can be encoded in
another model of computation.  Here is an encoding of the above actor model
using Hopac:

```fsharp
module ActorModel =
  type ActorThread<'a, 'x> = AT of (Ch<'a> -> Job<'x>)
  let unAT (AT x) = x
  let (>>=) (xA: ActorThread<'a, 'x>) (x2yA: 'x -> ActorThread<'a, 'y>) : ActorThread<'a, 'y> =
    AT (fun aCh -> unAT xA aCh >>= fun x -> unAT (x2yA x) aCh)
  let result (x: 'x) : ActorThread<'a, 'x> =
    AT (fun aCh -> Job.result x)
  let receive : ActorThread<'a, 'a> =
    AT (fun aCh -> Ch.take aCh)
  type Actor<'a> = A of Ch<'a>
  let unA (A aCh) = aCh
  let start (uA: ActorThread<'a, unit>) : Actor<'a> =
    let aCh = Ch.Now.create ()
    Job.Global.start (unAT uA aCh)
    A aCh
  let send (aA: Actor<'a>) (a: 'a) : unit =
    Job.Global.start (Ch.give (unA aA) a)
```

As can be seen above, it is fairly straightforward to encode an actor model
using only a small subset of Hopac.  (Note that the **>>=** operation used
within the definition of **>>=** is the Hopac bind operation.)  The reverse
encoding, that is, an implementation of the **HopacModel** using the
**ActorModel**, is left as an exercise for the reader.

Please note that the above is not meant to demonstrate a practical way to do
actor style programming in Hopac.  The above is meant as an illustrative
encoding that hopefully helps to understand both models.  The above encoding is
not very practical, because it is very inefficient in a number of ways and it is
possible to implement actor models and program in actor model like styles
directly within Hopac.  While the above uses the actual Hopac library, so that
you can directly compile the above code, it makes no use of any primitives in
Hopac beyond what is in the previous HopacModel signature.

More practical actor style programming in Hopac
-----------------------------------------------

A merit of many of the actor models is that due to the marriage of threads and
mailboxes within those models there are often idiomatic ways to structure
programs.  The model provided by Hopac, due to the separation of threads and
channels, often allows for many more ways to structure computations and while
such flexibility can be advantageous it can also be overwhelming.  In this
section, we'll consider how some actor style idioms might translate to practical
programming styles in Hopac.

### MailboxProcessor

Let's start by implementing something similar to a subset of the F#
[MailboxProcessor](http://msdn.microsoft.com/en-us/library/ee370357.aspx) within
Hopac.  We'll just define an actor as an asynchronous
[Mailbox](http://htmlpreview.github.io/?https://github.com/VesaKarvonen/Hopac/blob/master/Docs/Hopac.html#dec:Hopac.Mailbox%3C%27x%3E):

```fsharp
type Actor<'m> = Mailbox<'m>
```

To start an actor, we use a helper function that constructs a job that creates a
mailbox and starts a job with that mailbox:

```fsharp
let actor (body: Mailbox<'m> -> Job<unit>) : Job<Actor<'m>> = Job.delay <| fun () ->
  let mA = mb ()
  Job.start (body mA) >>% mA
```

Within the body of an actor, the actor can simply receive messages from the
mailbox.  Let's make that a bit more concrete:

```fsharp
let receive (mA: Mailbox<'m>) : Job<'m> = Mailbox.take mA
```

For sending messages to an actor started with the help of **actor**, we can
simply use the operations provided for mailboxes.  But let's make that a bit
more concrete:

```fsharp
let post (mA: Actor<'m>) (m: 'm) : Job<unit> = mA <<-+ m
```

To allow an actor to provide a reply to a message, we can, similar to
MailboxProcessor, send the actor a message passing object of some kind.  In
Hopac we could use one of many different message passing objects.  Closest to
the [AsyncReplyChannel](http://msdn.microsoft.com/en-us/library/ee370529.aspx)
would be in
[IVar](http://htmlpreview.github.io/?https://github.com/VesaKarvonen/Hopac/blob/master/Docs/Hopac.html#dec:Hopac.IVar%3C%27x%3E):

```fsharp
let postAndReply (mA: Actor<'m>) (i2m: IVar<'r> -> 'm) : Job<'r> = Job.delay <| fun () ->
  let i = ivar ()
  mA <<-+ i2m i >>. i
```

To reply to a message, the agent then needs to write to the given **IVar**:

```fsharp
let reply (rI: IVar<'r>) (r: 'r) : Job<unit> = rI <-= r
```

Consider the following echo agent:

```fsharp
type Echo<'x> = Echo of 'x * AsyncReplyChannel<'x>
let echo () = MailboxProcessor.Start <| fun inbox -> async {
  while true do
    let! Echo (x, xArc) = inbox.Receive ()
    do xArc.Reply x
}
```

Using the previously defined combinators, we could express a similar agent as
follows:

```fsharp
type Echo<'x> = Echo of 'x * IVar<'x>
let echo () = actor <| fun inbox -> job {
  while true do
    let! Echo (x, xI) = receive inbox
    do! reply xI x
}
```

One difference in the MailboxProcessor like operations implemented here is that
the Hopac operations return jobs that then need to be run.  This is intentional.
The implementation of Hopac is such that the concurrent operations within Hopac
run fastest when they are executed by worker threads of a Hopac scheduler.
Running individual concurrent operations outside of workers incurs potentially
significant overheads.  Therefore the operations sketched here return jobs that
can then potentially be composed into longer jobs to run.

This is just a small sketch and omits many features of the MailboxProcessor.
You could continue extending these snippets to quite straightforwardly add
support for most of what the MailboxProcessor provides, including features such
as error events.  In most cases, however, there is no practical need to work
like that.  While the above **actor** and **postAndReply** functions could be
handy shortcuts in some cases, most of the other operations are readily
available within Hopac or similar functionality can be expressed more flexibly
and possibly more concisely.  For example, here is a more concise implementation
of the above echo example using operations directly available with Hopac:

```fsharp
type Echo<'x> = Echo of 'x * IVar<'x>
let echo () = actor <| fun mb ->
  Job.forever (mb >>= fun (Echo (x, xI)) -> xI <-= x)
```

Aside from being more concise, this version is also likely to be faster.
