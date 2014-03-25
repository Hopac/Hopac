Actors and Hopac
================

The subject of Actors seems to come up often when discussing Hopac.  Both Hopac
and actors are models of
[message passing](http://en.wikipedia.org/wiki/Message_passing) concurrency, but
what are actors and what kind of similarities and differences do actors and
Hopac have?

It is somewhat difficult to pin down what exactly are actors, because there are
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
abstraction, also often described as an actor model, provided with F# async
model gives an explicit name to the mailbox.  However, the MailboxProcessor
model requires that there is at most one concurrent reader of a particular
MailboxProcessor active at any moment.  This means that a MailboxProcessor is
effectively associated with a thread of execution.

Unlike those actor models, Hopac and
[Concurrent ML](http://en.wikipedia.org/wiki/Concurrent_ML), which is the
inspiration behind Hopac, provide a model in which threads of execution and
means of communication are separate similar to
[pi-calculus](http://en.wikipedia.org/wiki/%CE%A0-calculus).  In Hopac, threads
of execution are called *jobs* and various communication primitives, such as,
synchronous *channels*, asynchronous *mailboxes*, synchronous write once *ivars*
and write many *mvars* variables are provided.  In addition, a form of
*selective communication* called *alternatives* is provided that allows the
expression of first-class synchronous abstractions.

Unlike the way actor models combine a thread of execution and a mailbox into an
actor, in Hopac, a single concurrent job may be receiving messages via any
number of explicitly named channels&mdash;including the possibility of not using
any channels&mdash;and, on the other hand, a single channel may be used by any
number of jobs for receiving messages.

In Hopac, the threads of execution called jobs and concurrent operations are
encoded using monadic combinators.  Here is an illustrative signature sketch of
the essence of the programming model provided by Hopac:

```fsharp
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
execution and that thread can be started with **start**.  Any number of channels
can be created using **ch** and different channels may have different types.
Within a thread of execution one can then perform **give** and **take**
operations on channels.  Both **give** and **take** operations are synchronous
and block the executing job until the other party of the communication is
present and the operation can be completed.  Note that the above signature
snippet is not a precise subset of Hopac and that Hopac provides a much more
comprehensive model as described in the
[Hopac Library Reference](http://htmlpreview.github.io/?https://github.com/VesaKarvonen/Hopac/blob/master/Docs/Hopac.html)
manual.

We could also similarly encode the essence of an actor model.  Here is a sketch
of such a model:

```fsharp
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
