Programming in Hopac
====================

Hopac provides a programming model that is heavily inspired by
[John Reppy](http://people.cs.uchicago.edu/~jhr/)'s **Concurrent ML** language.
The book
[Concurrent Programming in ML](http://www.cambridge.org/us/academic/subjects/computer-science/distributed-networked-and-mobile-computing/concurrent-programming-ml)
is the most comprehensive introduction to Concurrent ML style programming.  This
document contains some discussion and examples on Hopac programming techniques.
In the future, this document might grow to a proper introduction to Hopac.
Feedback is welcome!

The
[Hopac.fsi](https://github.com/Hopac/Hopac/blob/master/Libs/Hopac/Hopac.fsi)
signature contains documentation comments on the Hopac primitives used in this
document.  There is also a
[Hopac Library Reference](http://hopac.github.io/Hopac/Hopac.html)
manual generated from the signature file and this documents links to specific
Hopac primitives descriptions in the reference manual.  It is recommended that
you open the Hopac solution in Visual Studio and start the F# interactive shell
so that you can look at the documentation comments and quickly try out examples
from this document.  You can use the
[Hopac.fsx](https://github.com/Hopac/Hopac/blob/master/Hopac.fsx) script
to prepare an environment in which you should be able to directly evaluate
example code from this document.

The Hopac Programming Model
---------------------------

There are two central aspects of Hopac that shape the programming model.

The first aspect is that, threads, which are called *jobs*, represented by the
type
`Job<'x>`[*](http://hopac.github.io/Hopac/Hopac.html#def:type%20Hopac.Job),
in Hopac are extremely lightweight.  On modern machines you can start tens of
millions of new jobs in a second.  Because a job takes only a very small amount
of memory, starting from tens of bytes, a program may have millions of jobs on a
modern machine at any moment.  (Of course, at any moment, most of those jobs are
suspended, because modern machines still only have a few, or at most a few
dozen, processor cores.)  When programming in Hopac, one can therefore start new
jobs in situations where it would simply be unthinkable when using heavyweight
threads.

The other aspect is that Hopac provides first-class, higher-order, selective,
synchronous, lightweight, message passing primitives in the form of *channels*,
represented by the type
`Ch<'x>`[*](http://hopac.github.io/Hopac/Hopac.html#def:type%20Hopac.Ch),
and *alternatives*, represented by the type
`Alt<'x>`[*](http://hopac.github.io/Hopac/Hopac.html#def:type%20Hopac.Alt),
for coordinating and communicating between jobs.  That is a mouthful!  Let's
open it up a bit.

* **First-class** means that channels and alternatives are ordinary values.
  They can be bound to variables, passed to and returned from functions and can
  even be sent from one job to another.
* **Higher-order** means that primitive alternatives can be combined and
  extended[*](http://hopac.github.io/Hopac/Hopac.html#def:val%20Hopac.Alt.Infixes.%3E%3E=?)
  with user defined procedures to
  build[*](http://hopac.github.io/Hopac/Hopac.html#def:val%20Hopac.Alt.guard)
  more
  complex[*](http://hopac.github.io/Hopac/Hopac.html#def:val%20Hopac.Alt.withNack)
  alternatives that encapsulate concurrent client-server protocols.
* **Selective** means that a form of
  choice[*](http://hopac.github.io/Hopac/Hopac.html#def:val%20Hopac.Alt.choose)
  or disjunction between alternatives is supported.  An alternative can be
  constructed that, for example, offers to
  give[*](http://hopac.github.io/Hopac/Hopac.html#def:val%20Hopac.Ch.Alt.give)
  a message to another job *or*
  take[*](http://hopac.github.io/Hopac/Hopac.html#def:val%20Hopac.Ch.Alt.take)
  a message from another job.  The choice of which operation is performed then
  depends on whichever alternative becomes available at run time.
* **Synchronous** means that rather than building up a queue of messages for
  another job to examine, jobs can communicate via *rendezvous*.  Two jobs can
  meet so that one job can give a message to another job that takes the message.
* **Lightweight** means that creating a new synchronous channel takes very
  little time (a single memory allocation) and a channel takes very little
  memory on its own.

What this all boils down to is that Hopac basically provides a kind of language
for expressing concurrent control flow.

### Potential Applications for Hopac

Hopac is by no means a panacea.  As discussed above, the essence of Hopac is
*lightweight* threads, called *jobs*, and flexible lightweight synchronous
message passing via *channels* (and other messaging primitives).  Hopac is
designed and optimized to scale as the number of such relatively independent
lightweight elements is increased.  That can be seen as a form of *data
parallelism* in which the data is the program entities implemented by the jobs
and communication primitives.

Problem domains that are more or less naturally expressed in terms of large
numbers of threads (one or more threads per program element) and message passing
are where Hopac should be able to shine in terms of performance and ease of
programming.  Parallel build systems, simulations, web servers and GUIs, for
example, fit this description.

On the other hand, problem domains that can be conveniently expressed with just
a small number of threads are unlikely to benefit from Hopac.  For example, if
you can conveniently express your system as a kind of fixed pipeline with a few
threads that you can afford to be kept spinning waiting for messages, then a
system like the [LMAX Disruptor](http://lmax-exchange.github.io/disruptor/)
might offer better performance than Hopac.

A Couple of Introductory Examples
---------------------------------

Rather than meticulously building up the primitives of Hopac let's just first
run through a couple of examples.  These examples are meant to be read through
fairly quickly.  We'll look at the primitives used in these examples in detail
later.

### Example: Updatable Storage Cells

In the book
[Concurrent Programming in ML](http://www.cambridge.org/us/academic/subjects/computer-science/distributed-networked-and-mobile-computing/concurrent-programming-ml),
[John Reppy](http://people.cs.uchicago.edu/~jhr/) presents as the first
programming example an implementation of updatable storage cells using
Concurrent ML channels and threads.  While this example is not exactly something
that one would do in practice, because F# already provides ref cells, it does a
fairly nice job of illustrating some core aspects of Concurrent ML.  So, let's
reproduce the same example with Hopac.

Here is the signature for our updatable storage cells:

```fsharp
type Cell<'a>
val cell: 'a -> Job<Cell<'a>>
val get: Cell<'a> -> Job<'a>
val put: Cell<'a> -> 'a -> Job<unit>
```

The `cell` function creates a
job[*](http://hopac.github.io/Hopac/Hopac.html#def:type%20Hopac.Job)
that creates a new storage cell.  The `get` function creates a job that returns
the contents of the cell and the `put` function creates a job that updates the
contents of the cell.

The basic idea behind the implementation is that the cell is a concurrent
*server* that responds to `Get` and `Put` request.  We represent the requests
using the `Request` discriminated union type:

```fsharp
type Request<'a> =
 | Get
 | Put of 'a
```

To communicate with the outside world, the server presents two channels: one
channel for requests and another channel for replies required by the get
operation.  The `Cell` type is a record of those two
channels[*](http://hopac.github.io/Hopac/Hopac.html#def:type%20Hopac.Ch):

```fsharp
type Cell<'a> = {
  reqCh: Ch<Request<'a>>
  replyCh: Ch<'a>
}
```

The `put` operation is a
job[*](http://hopac.github.io/Hopac/Hopac.html#def:val%20Hopac.TopLevel.job)
that simply
gives[*](http://hopac.github.io/Hopac/Hopac.html#def:val%20Hopac.Ch.give)
the `Put` request to the server via the request channel:

```fsharp
let put (c: Cell<'a>) (x: 'a) : Job<unit> = job {
  return! Ch.give c.reqCh (Put x)
}
```

The `get` operation gives the `Get` request to the server via the request
channel and then
takes[*](http://hopac.github.io/Hopac/Hopac.html#def:val%20Hopac.Ch.take)
the server's reply from the reply channel:

```fsharp
let get (c: Cell<'a>) : Job<'a> = job {
  do! Ch.give c.reqCh Get
  return! Ch.take c.replyCh
}
```

Finally, the `cell` operation actually creates the channels and
starts[*](http://hopac.github.io/Hopac/Hopac.html#def:val%20Hopac.Job.start)
the concurrent server job:

```fsharp
let cell (x: 'a) : Job<Cell<'a>> = job {
  let c = {reqCh = Ch.Now.create (); replyCh = Ch.Now.create ()}
  let rec server x = job {
        let! req = Ch.take c.reqCh
        match req with
         | Get ->
           do! Ch.give c.replyCh x
           return! server x
         | Put x ->
           return! server x
      }
  do! Job.start (server x)
  return c
}
```

The concurrent server is a job that loops indefinitely taking requests from the
request channel.  When the server receives a `Get` request, it gives the current
value of the cell on the reply channel and then loops to take another request.
When the server receives a `Put` request, the server loops with the new value to
take another request.

Here is sample output of an interactive session using a cell: 

```fsharp
> let c = run (cell 1) ;;
val c : Cell<int> = ...
> run (get c) ;;
val it : int = 1
> run (put c 2) ;;
val it : unit = ()
> run (get c) ;;
val it : int = 2
```

#### Garbage Collection

Running through the previous example you may have wondered about what happens to
server jobs that run inside those cells.  Shouldn't they be killed?  Indeed, one
aspect that is important to understand is that Hopac jobs and channels are basic
simple .Net objects and can be garbage collected.  Specifically, jobs and
channels do not inherently hold onto disposable system resources.  This is
unlike the
[MailboxProcessor](http://msdn.microsoft.com/en-us/library/ee370357.aspx), for
example, which is
[disposable](http://msdn.microsoft.com/en-us/library/system.idisposable.aspx).
What this means in practice is that most jobs do not necessarily need to
implement any special kill protocol.  A job that is blocked waiting for
communication on a channel that is no longer reachable can (and will) be garbage
collected.  Only jobs that explicitly hold onto some resource that needs to be
disposed must implement a kill protocol to explicitly make sure that the
resource gets properly disposed.

Consider the following interaction:

```fsharp
> GC.GetTotalMemory true ;;
val it : int64 = 39784152L
> let cs = ref (List.init 100000 <| fun i -> run (cell i)) ;;
// ...
> GC.GetTotalMemory true ;;
val it : int64 = 66296336L
> cs := [] ;;
val it : unit = ()
> GC.GetTotalMemory true ;;
val it : int64 = 39950064L
```

The above shows that after the list has become garbage, the cells have been
garbage collected.  (The above example interaction uses lists to avoid the
possibility that the objects would end up in the last generation or the LOH,
because when that happens it can be difficult to force .Net runtime to perform a
thorough enough GC for the memory estimate to be valid.)

#### On Memory Usage

Another important property of Hopac jobs and synchronous channels is that a
system that consist of **m** jobs that communicate with each other using
synchronous message passing over **n** channels requires **&Theta;(m + n)**
space for the jobs and channels.

That may sound obvious, but many concurrent systems,
e.g. [Erlang](http://www.erlang.org/) and F#'s
[MailboxProcessor](http://msdn.microsoft.com/en-us/library/ee370357.aspx), are
built upon asynchronous message passing primitives and in such systems message
queues can collect arbitrary numbers of messages when there are differences in
speed between producer and consumer threads.  Synchronous channels do not work
like that.  A synchronous channel doesn't hold a buffer of messages.  When a
producer job tries to give a message to a consumer job using a synchronous
channel, the producer is suspended until a consumer job is ready to take the
message.  A synchronous channel essentially provides a *simple rendezvous*
mechanism that is less like a passive buffer for passing data and more like a
control flow mechanism, like a kind of procedure call with no return value.
This property can make it easier to understand the behavior of concurrent
programs.

Of course, the bound **&Theta;(m + n)** does not take into account space that
the jobs otherwise accumulate in the form of data structures other than the
synchronous channels.

#### On Notation

There are two ways to write jobs in Hopac.  One way is to use the
`job`[*](http://hopac.github.io/Hopac/Hopac.html#def:val%20Hopac.TopLevel.job)
workflow builder like we did in the previous section.  The other way is to
directly use the monadic combinators,
`result`[*](http://hopac.github.io/Hopac/Hopac.html#def:val%20Hopac.Job.result)
and bind,
`>>=`[*](http://hopac.github.io/Hopac/Hopac.html#def:val%20Hopac.Job.Infixes.%3E%3E=),
that the workflow builder abstracts away.  I personally mostly prefer using the
monadic combinators with an occasional excursion with the workflow notation.  I
have a number of reasons for this:

* Using the combinators directly usually leads to more concise code.
* I often find it easier to understand the code when it is written with the
  monadic combinators.
* There are many very commonly used monadic combinators,
  e.g. `|>>`[*](http://hopac.github.io/Hopac/Hopac.html#def:val%20Hopac.Job.Infixes.|%3E%3E)
  and
  `>>%`[*](http://hopac.github.io/Hopac/Hopac.html#def:val%20Hopac.Job.Infixes.%3E%3E%),
  that do not have a corresponding workflow builder function and notation and
  use of those combinators leads to faster code.
* Using the combinators directly I can often avoid some unnecessary
  `delay`[*](http://hopac.github.io/Hopac/Hopac.html#def:val%20Hopac.Job.delay)
  operations the workflow notation introduces for safety reasons.

I'm afraid that to fully explain all of these issues would require quite a bit
of writing and I think that there are more interesting things to tell about
Hopac, so I'll skip it for now.  In the reminder of this document I will be
writing Hopac code in my preferred way.  If you prefer to make more use of the
workflow notation, you could consider it as an exercise to convert the examples
to use the workflow notation.  If you do that, make sure that you properly
retain tailcall properties of the original snippets.

Before we continue, I'd just like to show you the below rewrite of the updatable
storage cells using those monadic combinators directly.

```fsharp
let put (c: Cell<'a>) (x: 'a) : Job<unit> =
  Ch.give c.reqCh (Put x)

let get (c: Cell<'a>) : Job<'a> = Ch.give c.reqCh Get >>. Ch.take c.replyCh

let create (x: 'a) : Job<Cell<'a>> = Job.delay <| fun () ->
  let c = {reqCh = Ch.Now.create (); replyCh = Ch.Now.create ()}
  let rec server x =
    Ch.take c.reqCh >>= function
     | Get ->
       Ch.give c.replyCh x >>.
       server x
     | Put x ->
       server x
  Job.start (server x) >>% c
```

As you can see above, I've used
`delay`[*](http://hopac.github.io/Hopac/Hopac.html#def:val%20Hopac.Job.delay)
only once and if you count the number of words and lines, you'll find out that
that the code is more concise.  I personally find the monadic code roughly as
readable as the workflow notation.

In addition to the monadic job combinators, Hopac also provides symbolic
operators for some of the message passing operations.  Also, many of the
operations in Hopac are simple upcasts along the inheritance chain and can
either be eliminated completely or replaced by an actual F# `upcast` operation.
Furthermore, Hopac also provides a few shortcut convenience bindings and
combined operations for frequently used operations and programming idioms.
Using those shortcuts, and dropping unnecessary type ascriptions, we can write
the above cell example as:

```fsharp
let put c x = c.reqCh <-- Put x

let get c = c.reqCh <-- Get >>. c.replyCh

let create x = Job.delay <| fun () ->
  let c = {reqCh = ch (); replyCh = ch ()}
  Job.iterateServer x (fun x ->
   c.reqCh >>= function
    | Get -> c.replyCh <-- x >>% x
    | Put x -> Job.result x) >>% c
```

In this document, we will use type ascriptions so that one see the types without
compiling the code.  We will also avoid many of the shortcuts for conceptual and
syntactic clarity.  So, while you can take a value from a channel just by
binding it, and you might want to use that in production code, we will avoid
doing that in the examples of this document.

**Exercise:** As an alternative to having two preallocated channels `reqCh` and
`replyCh` one could also make it so that a fresh reply channel is allocated and
passed to the server by the `get` operation each time.  Change the
implementation to use this technique.  Explain what performance advantages and
disadvantages such an implementation might have?

### Example: Storage Cells Using Alternatives

The updatable storage cells in the previous section were built using only
channels and jobs.  In order to allow for the two different kind of requests,
`Get` and `Put`, the union type `Request` and pattern matching were used.  In
this section we look at an alternative implementation of storage cells using
selective communication.

As a reminder, here is the abstract signature that we'd like to implement:

```fsharp
type Cell<'a>
val cell: 'a -> Job<Cell<'a>>
val get: Cell<'a> -> Job<'a>
val put: Cell<'a> -> 'a -> Job<unit>
```

The idea for this implementation is that the server loop of storage cells
creates an alternative that either takes a new value on a channel for `put`
operations or gives the current value on a channel for `get` operations.  The
cell type just consists of these
channels[*](http://hopac.github.io/Hopac/Hopac.html#def:type%20Hopac.Ch):

```fsharp
type Cell<'a> = {
  getCh: Ch<'a>
  putCh: Ch<'a>
}
```

The `get` operation then simply
takes[*](http://hopac.github.io/Hopac/Hopac.html#def:val%20Hopac.Ch.take)
a value on the `getCh` channel from the server of a cell:

```fsharp
let get (c: Cell<'a>) : Job<'a> = Ch.take c.getCh
```

And the `put` operations
gives[*](http://hopac.github.io/Hopac/Hopac.html#def:val%20Hopac.Ch.give)
a value to the server on the `putCh` channel of the cell server:

```fsharp
let put (c: Cell<'a>) (x: 'a) : Job<unit> = Ch.give c.putCh x
```

The `cell` constructor then creates the channels and starts the server loop:

```fsharp
let cell x = Job.delay <| fun () ->
  let c = {getCh = Ch.Now.create (); putCh = Ch.Now.create ()}
  let rec server x =
    Alt.select [Ch.Alt.take c.putCh   >>=? fun x -> server x
                Ch.Alt.give c.getCh x >>=? fun () -> server x]
  Job.start (server x) >>% c
```

In the server loop, the above implementation uses
selective[*](http://hopac.github.io/Hopac/Hopac.html#def:val%20Hopac.Alt.select)
communication.  It uses a
choice[*](http://hopac.github.io/Hopac/Hopac.html#def:val%20Hopac.Alt.choose)
of two primitive
alternatives[*](http://hopac.github.io/Hopac/Hopac.html#def:type%20Hopac.Alt):

* The first alternative
  takes[*](http://hopac.github.io/Hopac/Hopac.html#def:val%20Hopac.Ch.Alt.take)
  a value on the `putCh` channel from a client and
  then[*](http://hopac.github.io/Hopac/Hopac.html#def:val%20Hopac.Alt.Infixes.%3E%3E=?)
  loops.
* The second alternative
  gives[*](http://hopac.github.io/Hopac/Hopac.html#def:val%20Hopac.Ch.Alt.take)
  a value on the `getCh` channel to a client and
  then[*](http://hopac.github.io/Hopac/Hopac.html#def:val%20Hopac.Alt.Infixes.%3E%3E=?)
  loops.

What this basically means is that the server makes an offer to perform the
alternatives.  Of the two offered alternatives, the alternative that becomes
available first will then be committed to.  The other offer will be withdrawn.

This pattern of carrying some value from one iteration of a server loop to the
next is common enough that there is a combinator
`iterate`[*](http://hopac.github.io/Hopac/Hopac.html#def:val%20Hopac.Job.iterate)
for that purpose.  Using `iterate` we would write:

```fsharp
let cell x = Job.delay <| fun () ->
  let c = {getCh = Ch.Now.create (); putCh = Ch.Now.create ()}
  Job.server
   (Job.iterate x <| fun x ->
    Alt.select [Ch.Alt.take c.putCh
                Ch.Alt.give c.getCh x >>%? x]) >>% c
```

The above also makes use of the function
`Job.server`[*](http://hopac.github.io/Hopac/Hopac.html#def:val%20Hopac.Job.server)
instead of
`Job.start`[*](http://hopac.github.io/Hopac/Hopac.html#def:val%20Hopac.Job.start).
`Job.server` takes advantage of the fact that the job it is given is known to
never return normally and starts it in a little bit lighter-weight form.

At this point you might want to try out the snippets of code from this section
in the F# interactive and verify that the alternative implementation of cells
works the same way as the previous version.

Inspired by these cell examples there is benchmark program, named
[Cell](https://github.com/Hopac/Hopac/tree/master/Benchmarks/Cell), that
creates large numbers of cells and large numbers of jobs running in parallel
that perform updates on randomly chosen cells.  While the benchmark program is
not terribly exciting, it nicely substantiates the claims made in the first
section about the lightweight nature of Hopac jobs and channels.

**Exercise:** It may seem odd that two bidirectional channels are needed to
implement the protocol.  Couldn't we use just a single channel and change the
server loop to give and take on that single channel.  Note that this is allowed
in Hopac and poses no problem.  A job cannot send itself a message using a
channel in a single synchronous operation.  Explain what would go wrong if there
was only one channel instead of separate `getCh` and `putCh` channels.  Hint:
Consider a situation with multiple clients.

### Example: Kismet

The updatable storage cell example in the previous sections may have seemed
rather unrealistic.  The server job of a storage cell doesn't do much and it
probably doesn't seem like something for which you'd even consider starting a
separate thread&mdash;no matter how lightweight such a thread would be.  In this
section we'll sketch an example that might be a bit more compelling, although in
a way it is also quite unreal.

[UnrealScript](http://en.wikipedia.org/wiki/UnrealScript) is the scripting
language of the [Unreal Engine](http://en.wikipedia.org/wiki/Unreal_Engine) and
is used for making games.
[Kismet](http://en.wikipedia.org/wiki/UnrealEd#Kismet) is a tool that enables
artists to create scripts in UnrealScript using a visual interface.  Working
with Kismet, artists can basically create games by combining building blocks
created by programmers.  Those building blocks can be seen as black boxes that
have some inputs, outputs and have some interesting behavior mapping the inputs
to outputs.

On the Wikipedia page on [UnrealEd](http://en.wikipedia.org/wiki/UnrealEd) there
is a screenshot of a simple system built using Kismet.  Take a moment to look at
the screenshot:
[Roboblitz](http://upload.wikimedia.org/wikipedia/en/e/e6/Kismet_Roboblitz.PNG).
As you can see, there are basic reusable blocks like `Bool`, `Compare Bool`,
`Delay`, and `Matinee` that have some inputs, outputs and some behavior.

Kismet, UnrealScript and the Unreal Engine, in general, have components and
semantics that have been designed for making games.  In fact, I've never
actually programmed in UnrealScript or used Kismet, but a curious mind might
wonder how could black boxes like that be implemented?  Could we build something
similar using Hopac?

Let's first consider the `Compare Bool` box.  Looking at the screenshot and
making an educated guess, it seems to have an input event `In` and two output
events `True` and `False` and it also seems to read a `Bool` variable.  It would
seem that the idea is that when the box receives the `In` event, it signals
either the `True` or the `False` event depending on the current value of the
`Bool` variable.  Something like that can be quite concisely expressed as a
Hopac job:

```fsharp
let CompareBool (comparand: ref<bool>)
                (input: Alt<'x>)
                (onTrue: 'x -> Job<unit>)
                (onFalse: 'x -> Job<unit>) : Job<unit> =
  Alt.pick input >>= fun x ->
  if !comparand then onTrue x else onFalse x
```

The `CompareBool` function creates a job that first
picks[*](http://hopac.github.io/Hopac/Hopac.html#def:val%20Hopac.Alt.pick)
the `input` alternative and then performs either the `onTrue` or the `onFalse`
action depending on the value of `comparand`.  As you can see, the above
`CompareBool` job doesn't care about the type of the alternatives.  It just
copies the received value `x` to the chosen output.

Let's then consider the `Delay` box.  Making another educated guess and
simplifying a bit, it has two input events `Start` and `Stop` (I leave `Pause`
as an exercise for the reader) and two output events `Finished` and `Aborted`
and also a time value `Duration`.  It would seem that the idea is that when the
box receives the `Start` event, it starts a timer that counts down for the
specified `Duration` after which the `Finished` event is signaled.  Also, if
during the countdown, a `Stop` signal is received then the `Aborted` signal is
signaled instead.  Here is how something like that could be expressed as a Hopac
job:

```fsharp
let Delay (duration: ref<TimeSpan>)
          (start: Alt<'x>)
          (stop: Alt<'y>)
          (finished: 'x -> Job<unit>)
          (aborted: 'y -> Job<unit>) : Job<unit> =
  Alt.pick start >>= fun x ->
  Alt.select [stop                             >>=? fun y -> aborted y
              Timer.Global.timeOut (!duration) >>=? fun () -> finished x]
```

The `Delay` function creates a job that first
picks[*](http://hopac.github.io/Hopac/Hopac.html#def:val%20Hopac.Alt.pick)
the `start` alternative.  It then
selects[*](http://hopac.github.io/Hopac/Hopac.html#def:val%20Hopac.Alt.select)
from two alternatives.  The first one is the given `stop` alternative and in
case that is committed to, the value obtained from `stop` is given to the
`aborted` action.  The second alternative starts a
`timeOut`[*](http://hopac.github.io/Hopac/Hopac.html#def:val%20Hopac.Timer.Global.timeOut)
alternative for the current value of `duration` and in case that is committed
to, the value received from `start` is given to the `finished` action.
Whichever of those alternatives becomes enabled first will then be committed to
during run time and the other will be discarded.

These building blocks may seem deceptively simple.  What is important about
these building blocks is that they take advantage of the ability of Hopac's jobs
to be blocked waiting for an alternative.  Without something like that, those
building blocks wouldn't have the kind of abilities, such as being able to wait
for a timeout, that are needed here.

Those previous snippets are just two of the necessary building blocks.  Assuming
we would have all of the building blocks packaged in similar style, what remains
is translation of the wiring configuration specified in the screenshot into
code.  Here is a small snippet of a sketch of what the end result could look
like:

```fsharp
let ch_1 = Ch.Now.create ()
let ch_2 = Ch.Now.create ()
let ch_3 = Ch.Now.create ()
// ...
let bMoved = ref false
// ...
do! CompareBool bMoved
                (Ch.Alt.take ch_1)
                (Ch.give ch_2)
                (fun _ -> Job.unit ())
    |> Job.forever |> Job.server
do! Delay (ref (TimeSpan.FromSeconds 3.14))
          (Ch.Alt.take ch_2)
          (Alt.never ())
          (Ch.give ch_3)
          (fun _ -> Job.unit ())
    |> Job.forever |> Job.server
// ...
```

The above initialization code sketch first creates shared channels and
variables.  Then the desired building block jobs are created, passing them
appropriate input alternatives and output actions, and started as server jobs
that loop indefinitely.

Now, games often have their own specific notion of time, different from
wall-clock time, which means that for programming games, the sketched
implementation of `Delay` would not give the desired meaning of time.  (But you
can certainly implement a notion of time more suitable for games on top of
Hopac.)  Also, the way variables are represented as mutable ref cells is a bit
naive.  Unrealistic as it may be, this sketch has hopefully given you something
interesting to think about!

Starting and Waiting for Jobs
-----------------------------

After running through the introductory examples, let's take a step back and just
play a bit with jobs.  Here is a simple job that has a loop that first sleeps
for a second and then prints a given message:

```fsharp
let hello what = job {
  for i=1 to 3 do
    do! Timer.Global.sleep (TimeSpan.FromSeconds 1.0)
    do printfn "%s" what
}
```

Let's then start two such jobs roughly half a second a part:

```fsharp
> run <| job {
  do! Job.start (hello "Hello, from a job!")
  do! Timer.Global.sleep (TimeSpan.FromSeconds 0.5)
  do! Job.start (hello "Hello, from another job!")
} ;;
val it : unit = ()
> Hello, from a job!
Hello, from another job!
Hello, from a job!
Hello, from another job!
Hello, from a job!
Hello, from another job!
```

One unfortunate thing in the above example is that the program returns
immediately and the two jobs keep running in the background.  The
`Job.start`[*](http://hopac.github.io/Hopac/Hopac.html#def:val%20Hopac.Job.start)
primitive doesn't implicitly provide for any way to wait for the started job to
finish.  This is intentional, because it is quite common to start jobs that
don't need to return.  A
`Promise`[*](http://hopac.github.io/Hopac/Hopac.html#def:type%20Hopac.Promise)
allows a parent job to wait for a child job:

```fsharp
> run <| job {
  let! j1 = Promise.start (hello "Hello, from a job!")
  do! Timer.Global.sleep (TimeSpan.FromSeconds 0.5)
  let! j2 = Promise.start (hello "Hello, from another job!")
  do! Promise.read j1
  do! Promise.read j2
} ;;
Hello, from a job!
Hello, from another job!
Hello, from a job!
Hello, from another job!
Hello, from a job!
Hello, from another job!
val it : unit = ()
>
```

Now the program explicitly waits for the children to finish and the output is
clearer.  There is one more unfortunate thing in the above program.  The two
promises are read in a specific order.  In this program it doesn't really
matter, but it is a good demonstration of the flexibility of Hopac to show that
we can indeed avoid this order dependency by using
selective[*](http://hopac.github.io/Hopac/Hopac.html#def:val%20Hopac.Alt.select)
communication offered by the
alternative[*](http://hopac.github.io/Hopac/Hopac.html#def:type%20Hopac.Alt)
mechanism:

```fsharp
> run <| job {
  let! j1 = Promise.start (hello "Hello, from a job!")
  do! Timer.Global.sleep (TimeSpan.FromSeconds 0.5)
  let! j2 = Promise.start (hello "Hello, from another job!")
  do! Alt.select
       [Promise.Alt.read j1 >>=? fun () ->
          printfn "First job finished first."
          Promise.read j2
        Promise.Alt.read j2 >>=? fun () ->
          printfn "Second job finished first."
          Promise.read j1]
} ;;
Hello, from a job!
Hello, from another job!
Hello, from a job!
Hello, from another job!
Hello, from a job!
First job finished first.
Hello, from another job!
val it : unit = ()
```

When you run the above program, you will notice that the message `First job
finished first.` is printed about half a second before the last `Hello, from
another job!` message after which the program is finished and F# interactive
prints the inferred type.

Working with many jobs at this level would be rather burdensome.  Hopac also
provides functions such as
`Job.conCollect`[*](http://hopac.github.io/Hopac/Hopac.html#def:val%20Hopac.Job.conCollect)
and
`Job.conIgnore`[*](http://hopac.github.io/Hopac/Hopac.html#def:val%20Hopac.Job.conIgnore)
for starting and waiting for a sequence of jobs.  In this case we don't care
about the results of the jobs, so `Job.conIgnore` is what we use:

```fsharp
> [Timer.Global.sleep (TimeSpan.FromSeconds 0.0) >>. hello "Hello, from first job!" ;
   Timer.Global.sleep (TimeSpan.FromSeconds 0.3) >>. hello "Hello, from second job!" ;
   Timer.Global.sleep (TimeSpan.FromSeconds 0.6) >>. hello "Hello, from third job"]
|> Job.conIgnore |> run ;;
Hello, from first job!
Hello, from second job!
Hello, from third job
Hello, from first job!
Hello, from second job!
Hello, from third job
Hello, from first job!
Hello, from second job!
Hello, from third job
val it : unit = ()
>
```

The above program starts three concurrent jobs, that print messages roughly 0.3
seconds apart from each other, and waits for all three of the jobs to finish.

### Fork-Join Parallelism

Above we saw various ways of starting and waiting for jobs.  There isn't really
any communication between the individual jobs in these examples.  Indeed, the
strength of Hopac is in that it provides high-level primitives for such
communication among concurrent jobs.  Nevertheless, the style of programming
that consists of starting and joining with threads is also known as *fork-join
parallelism* and is a convenient paradigm for expressing many parallel
algorithms.

One of the goals for Hopac is to be able to achieve speedups on multicore
machines.  The primitives, such as channels, jobs (threads in CML) and
alternatives (events in CML) inspired by Concurrent ML are primarily designed
for concurrent programming involving separate threads of execution.  For
achieving speedups from parallelism, such independent threads of execution may
not be essential.  Sometimes it may be more efficient to avoid creating a new
thread of execution for every individual job, while some jobs are still being
executed in parallel.

#### The Fibonacci Function and Optional Parallelism

Consider the following naive implementation of the Fibonacci function as a job:

```fsharp
let rec fib n = Job.delay <| fun () ->
  if n < 2L then
    Job.result n
  else
    fib (n-2L) <&> fib (n-1L) |>> fun (x, y) ->
    x + y
```

The above implementation makes use of the combinators
`<&>`[*](http://hopac.github.io/Hopac/Hopac.html#def:val%20Hopac.Job.Infixes.%3C&%3E)
and
`|>>`[*](http://hopac.github.io/Hopac/Hopac.html#def:val%20Hopac.Job.Infixes.|%3E%3E)
whose meanings can be specified in terms of
`result`[*](http://hopac.github.io/Hopac/Hopac.html#def:val%20Hopac.Job.result)
and
`>>=`[*](http://hopac.github.io/Hopac/Hopac.html#def:val%20Hopac.Job.Infixes.%3E%3E=)
as follows:

```fsharp
let (<&>) xJ yJ = xJ >>= fun x -> yJ >>= fun y -> result (x, y)
let (|>>) xJ x2y = xJ >>= fun x -> result (x2y x)
```

Note that the semantics of `<&>` are entirely sequential and as a whole the
above `fib` job doesn't use any parallelism.

After evaluating the above definition of `fib` in the F# interactive, we can run
it as follows:

```fsharp
> run (fib 38L) ;;
val it : int64 = 39088169L
```

If you ran the above code, you noticed that it took some time for the result to
appear.  Indeed, this is an extremely inefficient exponential time algorithm for
computing Fibonacci numbers.

Let's make a small change, namely, let's change from the sequential pair
combinator
`<&>`[*](http://hopac.github.io/Hopac/Hopac.html#def:val%20Hopac.Job.Infixes.%3C&%3E)
to the parallel pair combinator
`<*>`[*](http://hopac.github.io/Hopac/Hopac.html#def:val%20Hopac.Job.Infixes.%3C*%3E):

```fsharp
let rec fib n = Job.delay <| fun () ->
  if n < 2L then
    Job.result n
  else
    fib (n-2L) <*> fib (n-1L) |>> fun (x, y) ->
    x + y
```

The parallel pair combinator `<*>` makes it so that the two jobs given to it are
either executed sequentially, just like `<&>`, or if it seems like a good thing
to do, then the two jobs are executed in two separate jobs that may eventually
run in parallel.  For this to be safe, the jobs must be safe to run *both* in
parallel and in sequence.  In this case those conditions both apply, but, for
example, the following job might deadlock:

```fsharp
let notSafe = Job.delay <| fun () ->
  let c = Ch.Now.create ()
  Ch.take c <*> Ch.give c ()
```

The problem in the above job is that both the
`take`[*](http://hopac.github.io/Hopac/Hopac.html#def:val%20Hopac.Ch.take)
and the
`give`[*](http://hopac.github.io/Hopac/Hopac.html#def:val%20Hopac.Ch.give)
operations are not guaranteed to be executed in two separate jobs and a single
job cannot communicate with itself using `take` and `give` operations on
channels.  Whichever operation happens to be executed first will block waiting
for the other pair of the communication that never appears.

##### Speedup?

Did you already try to run the parallel version of the naive Fibonacci function
in the F# interactive?  If you did, the behavior may have not been what you'd
expect&mdash;that the parallel version would run about `N` times faster than the
sequential version where `N` is the number of processor cores your machine has.
Now, there are a number of reasons for this and one of the possible reasons is
that, by default, .Net uses single-threaded workstation garbage collection.  If
garbage collection is single-threaded, it becomes a sequential bottleneck and an
application cannot possibly scale.  So, you need to make sure that you are using
multi-threaded server garbage collection.  See
[&lt;gcServer&gt; Element](http://msdn.microsoft.com/en-us/library/ms229357%28v=vs.110%29.aspx)
for some details.  I have modified the configuration files of the F# tools on my
machine to use the server garbage collection.  I also use the 64-bit version of
F# interactive and run on 64-bit machines.  Once you've made the necessary
adjustments to the tool configurations, you should see the expected speedup from
the parallel version.

##### About the Fibonacci Example

This example is inspired by the parallel Fibonacci function used traditionally
as a Cilk programming example.  See
[The Implementation of the Cilk-5 Multithreaded Language](http://supertech.csail.mit.edu/papers/cilk5.pdf)
for a representative example.  Basically, the naive, recursive, exponential time
Fibonacci algorithm is used.  Parallelized versions simply run recursive calls
in parallel.

Like is often the case with cute programming examples, this is actually an
extremely inefficient algorithm for computing Fibonacci numbers and that seems
to be a recurring source of confusion.  Indeed, the naively parallelized version
of the Fibonacci function is still hopelessly inefficient, because the amount of
work done in each parallel job is an order of magnitude smaller than the
overhead costs of starting parallel jobs.

The main reason for using the Fibonacci function as an example is that it is a
simple example for introducing the concept of optional parallel execution, which
is employed by the `<*>` combinator.  The parallel Fibonacci function is also
useful and instructive as a benchmark for measuring the overhead costs of
starting, running and retrieving the results of parallel jobs.  Indeed, there is
a
[benchmark program](https://github.com/Hopac/Hopac/tree/master/Benchmarks/Fibonacci)
based on the parallel Fibonacci function.

**Exercise:** Write a basic sequential Fibonacci function (not a job) and time
it.  Then change the parallelized version of the Fibonacci function to call the
sequential function when the **n** is smaller than some constant.  Try to find a
constant after which the new parallelized version actually gives a speedup on
the order of the number of cores on your machine.

#### Parallel Merge Sort

Let's consider a bit more realistic example of fork-join parallelism: a parallel
[merge sort](http://en.wikipedia.org/wiki/Merge_sort).  This example is still a
bit of toy, because the idea here isn't to show how to make the fastest merge
sort, but rather to demonstrate fork-join parallelism.

The two building blocks of merge sort are the functions `split` and `merge`.
The `split` function simply splits the given input sequence into two halves.
The `merge` function, on the other hand, merges two sequences into a new sorted
sequence containing the elements of both of the given sequences.

Here is a simple implementation of `split`:

```fsharp
let split xs =
  let rec loop xs ys zs =
    match xs with
     | []    -> (ys, zs)
     | x::xs -> loop xs (x::zs) ys
  loop xs [] []
```

And here is a simple implementation of `merge`:

```fsharp
let merge xs ys =
  let rec loop xs ys zs =
    match (xs, ys) with
     | ([], ys)       -> List.rev zs @ ys
     | (xs, [])       -> List.rev zs @ xs
     | (x::xs, y::ys) ->
       if x <= y
       then loop xs (y::ys) (x::zs)
       else loop (x::xs) ys (y::zs)
  loop xs ys []
```

It is left as an exercise for the reader to implement `merge` in a more
efficient form.

Merge sort then simply recursively splits, sorts and then merges the lists:

```fsharp
let rec mergeSort xs =
  match split xs with
   | ([], ys) -> ys
   | (xs, []) -> xs
   | (xs, ys) -> merge (mergeSort xs) (mergeSort ys)
```

We can now test that our `mergeSort` works:

```fsharp
> mergeSort [3; 1; 4; 1; 5; 9; 2] ;;
val it : int list = [1; 1; 2; 3; 4; 5; 9]
```

This is not a very good implementation of merge sort, but it should be easy
enough to understand&mdash;perhaps even without having seen merge sort before.
One particular problem with this implementation is that it is not
[stable](http://en.wikipedia.org/wiki/Sorting_algorithm#Stability).  We'll leave
it as an exercise for the reader to tune this example into something more
realistic.

Let's then write a fork-join parallel version of merge sort:

```fsharp
let rec mergeSortJob xs = Job.delay <| fun () ->
  match split xs with
   | ([], ys) -> Job.result ys
   | (xs, []) -> Job.result xs
   | (xs, ys) ->
     mergeSortJob xs <*> mergeSortJob ys |>> fun (xs, ys) ->
     merge xs ys
```

We can also test this version:

```fsharp
> run (mergeSortJob [3; 1; 4; 1; 5; 9; 2]) ;;
val it : int list = [1; 1; 2; 3; 4; 5; 9]
```

Like suggested in an exercise in the previous section, to actually get
speed-ups, the work done in each parallel job needs to be significant compared
to the cost of starting a parallel job.  One way to do this is to use the
sequential version of merge sort when the length of the list becomes shorter
than some threshold.  That threshold then needs to be chosen in such a way that
the work required to sort a list shorter than the threshold is significant
compared to the cost of starting parallel jobs.  In practice, this often means
that you run a few experiments to find a good threshold.  Here is a modified
version of `mergeSortJob` that uses a given threshold:

```fsharp
let mergeSortJob threshold xs = Job.delay <| fun () ->
  assert (threshold > 0)
  let rec mergeSortJob n xs = Job.delay <| fun () ->
    if n < threshold then
      Job.result (mergeSort xs)
    else
      let (xs, ys) = split xs
      let n = n/2
      mergeSortJob n xs <*> mergeSortJob n ys |>> fun (xs, ys) ->
      merge xs ys
  mergeSortJob (List.length xs) xs
```

For simplicity, the above computes the length of the input list just once and
then approximates the lengths of the sub-lists resulting from the split.  It
also assumes that `threshold` is greater than zero.  Using a function like the
above you can experiment, perhaps by writing a simple driver program, to find a
threshold that gives the best speed-ups.

Programming with Alternatives
-----------------------------

The alternative mechanism (events in CML) allows the definition of first-class
synchronous operations.  In previous sections we have already seen some simple
uses of alternatives.  In this section we'll take a closer look at alternatives.

### Just what is an alternative?

There are many ways to characterize alternatives.  Here is one.  An alternative,
`Alt<'x>`, represents the possibility of communicating a value of type `'x` from
one concurrent entity to another.  How that value is computed and when that
value is available are details encapsulated by the alternative.  Alternatives
can be created and combined in many ways allowing alternatives to encapsulate
complex communication protocols.

### Primitive Alternatives

The basic building blocks of alternatives are primitive alternatives provided by
message passing primitives like channels.  For example, the channel primitive
provides the following primitive alternatives:

```fsharp
module Ch =
  module Alt =
    val give: Ch<'x> -> 'x -> Alt<unit>
    val take: Ch<'x> -> Alt<'x>
```

The `Ch.Alt.give`
[*](http://hopac.github.io/Hopac/Hopac.html#def:val%20Hopac.Ch.Alt.give)
alternative represents the possibility of giving a value on a channel to another
concurrent job and the `Ch.Alt.take`
[*](http://hopac.github.io/Hopac/Hopac.html#def:val%20Hopac.Ch.Alt.take)
alternative represents the possibility of taking a value from another concurrent
job on a channel.

It is important that primitive alternatives such as these only represent the
*possibility* of performing the operations.  As we will see shortly, we can form
a disjunction of alternatives, whether primitive or complex, and commit to
perform exactly one of those alternatives.

### Picking an Alternative

To actually perform an operation made possible by an alternative, the `Alt.pick`
[*](http://hopac.github.io/Hopac/Hopac.html#def:val%20Hopac.Alt.pick)
operation is used:

```fsharp
val pick: Alt<'x> -> Job<'x>
```

So, for example, to indeed offer to give a value on a channel to another job, a
job might run the following code:

```fsharp
do! Alt.pick (Ch.Alt.give aChannel aValue)
```

Likewise, to offer to take a value from another job, the following code could be
run:

```fsharp
let! aValue = Alt.pick (Ch.Alt.take aChannel)
```

Conceptually, the `Alt.pick` operation *instantiates* the given alternative,
*waits until* the alternative becomes *available* for picking and then *commits*
to the alternative and returns the value communicated by the alternative.  In
the instantiation phase the computation encapsulated by the alternative is
started.  In the case of the `Ch.Alt.give` operation, for example, it means that
the job basically registers an offer to give a value on a channel.  If the
alternative cannot be performed immediately, e.g. no other job has offered to
take a value on the channel, the job is blocked until the alternative becomes
available.

### Primitive Alternatives vs Immediate Operations

Before continuing, let's take a moment to consider an aspect of the efficiency
implications of alternatives.  The channel interface also provides simpler
immediate versions of the give and take operations:

```fsharp
val give: Ch<'x> -> 'x -> Job<unit>
val take: Ch<'x> -> Job<'x>
```

Contrast the use of alternatives:

```fsharp
let! aValue = Alt.pick (Ch.Alt.take aChannel)
do! Alt.pick (Ch.Alt.give aChannel aValue)
```

and the use of immediate operations:

```fsharp
let! aValue = Ch.take aChannel
do! Ch.give aChannel aValue
```

Both of the above snippets have the same semantics.  Which version is faster?

In Hopac the answer is that both snippets compile to the exact same code.
(Assuming that the F# compiler can inline a non-virtual NOP function marked as
an inline function.)  In fact, the same holds for as long as the alternative
represents no selective operations.  Now, obviously, when more complex
alternatives are formed, the selective alternative mechanism must incur some
overhead compared to non-selective immediate operations.  But in the case only
primitive alternatives are used, there is no extra overhead.  This is a
fortunate feature of Hopac as it makes it more appealing to provide interfaces
to concurrent program modules using abstract composable alternatives rather than
non-composable immediate operations.

### Choose and Wrap

If all we had was primitive alternatives there would be no point in the whole
mechanism.  What makes alternatives useful is that they can be composed in
various ways.

Let's motivate the introduction of selective communication with a sketch of a
simplistic GUI system.  Suppose our GUI framework represents buttons as simple
concurrent objects that communicate using alternatives:

```fsharp
type Button =
  val Pressed: Alt<unit>
  // ...
```

A simple Yes/No -dialog could then contain two such buttons:

```fsharp
type YesNoDialog =
  val Yes: Button
  val No: Button
  val Show: Job<unit>
  // ...
```

A job could then have a dialogue with the user using a `YesNoDialog` and code
such as:

```fsharp
do! dialog.Show
let! answer = Alt.pick <| Alt.choose [
       dialog.Yes.Pressed >>=? fun () -> Job.result true
       dialog.No.Pressed  >>=? fun () -> Job.result false
     ]
if answer then
  // Perform action on Yes.
else
  // Perform action on No.
```

The operations `Alt.choose` and `>>=?`, also known as *wrap*, have the following
signatures:

```fsharp
val choose: seq<Alt<'x>> -> Alt<'x>
val (>>=?): Alt<'x> -> ('x -> Job<'y>) -> Alt<'y>
```

The `Alt.choose`
[*](http://hopac.github.io/Hopac/Hopac.html#def:val%20Hopac.Alt.choose)
operation forms a disjunction of the sequence of alternatives given to it.  When
such a disjunction is picked, the alternatives involved in the disjunction are
instantiated one-by-one.  Assuming no alternative is immediately available, the
job is blocked, waiting for any one of the alternatives to become available.
When one of the alternatives in the disjunction becomes available, the
alternative is picked and committed to and the other alternatives are canceled.

The wrap `>>=?`
[*](http://hopac.github.io/Hopac/Hopac.html#def:val%20Hopac.Alt.Infixes.%3E%3E=?)
operation is similar to the bind `>>=`
[*](http://hopac.github.io/Hopac/Hopac.html#def:val%20Hopac.Job.Infixes.%3E%3E=)
operation on jobs and allows one to extend an alternative so that further
operations are performed after the alternative has been committed to.  Similarly
to corresponding operations on jobs, several shortcut operators, such as `|>>?`
[*](http://hopac.github.io/Hopac/Hopac.html#def:val%20Hopac.Alt.Infixes.|%3E%3E?)
and `>>%?`
[*](http://hopac.github.io/Hopac/Hopac.html#def:val%20Hopac.Alt.Infixes.%3E%3E%?),
are provided in addition to `>>=?` on alternatives.

In this case we use the ability to simply map the button messages to a boolean
value for further processing.  We could also just continue processing in the
wrapper:

```fsharp
do! Alt.pick <| Alt.choose [
      dialog.Yes.Pressed >>=? fun () ->
        // Perform action on Yes.
      dialog.No.Pressed  >>=? fun () ->
        // Perform action on No.
    ]
```

Using selective communication in this way feels and works much like using
ordinary conditional statements.

A key point in the types of the `choose` and `>>=?` operations is that they
create new alternatives and those alternatives are first-class values just like
the primitive `give` and `take` alternatives on channels.  For the common cases
of simply picking from a choice of alternatives or combining just two
alternatives the operations `Alt.select`
[*](http://hopac.github.io/Hopac/Hopac.html#def:val%20Hopac.Alt.select)
and `<|>`
[*](http://hopac.github.io/Hopac/Hopac.html#def:val%20Hopac.Alt.Infixes.%3C|%3E)
are provided.  Their semantics can be described as follows:

```fsharp
let select alts = pick (choose alts)
let (<|>) a1 a2 = choose [a1; a2]
```

In fact, the above definition of `select` is essentially how the operation is
internally implemented.  The binary choice `<|>` operation can be, and is,
implemented internally as a slightly more efficient special case (avoiding the
construction of the sequence).

It is also worth pointing out that `choose` allows synchronizing on a sequence
of alternatives that is computed dynamically.  Languages that have a special
purpose `select` statement typically only allow the program to synchronize on a
set of events that is specified statically in the program text.

### Guards

The wrap combinator `>>=?`
[*](http://hopac.github.io/Hopac/Hopac.html#def:val%20Hopac.Alt.Infixes.%3E%3E=?)
allows post-commit actions to be added to an alternative.  Hopac also provides
the `guard`
[*](http://hopac.github.io/Hopac/Hopac.html#def:val%20Hopac.Alt.guard)
combinator that allows an alternative to be computed at instantiation time.

```fsharp
val guard: Job<Alt<'x>> -> Alt<'x>
```

The idea of the `guard` combinator is that it allows one to encapsulate a
protocol for interacting with a concurrent server as an abstract selective
operation.  The way a client and a server typically interact is that the client
sends the server a message and then waits for a reply from the server.  What is
necessary is that the guard combinator allows one to package the operations of
constructing the message, sending it to the server and then waiting for the
reply in a form that can then be invoked an arbitrary number of times.

Recall in the Kismet sketch it was mentioned that simulations like games often
have their own notion of time and that the wall-clock time provided by
`Timer.Global.timeOut`
[*](http://hopac.github.io/Hopac/Hopac.html#def:val%20Hopac.Timer.Global.timeOut)
probably doesn't provide the desired semantics.  A simple game might be designed
to update the simulation of the game world 60 times per second to match with a
60Hz display devices.  Rather than complicate all the calculations done in the
simulation with a variable time step, such a simulation could be advanced in
fixed length time steps or *ticks*.  Simplifying things to a minimum, the main
loop of a game could then look roughly like this:

```fsharp
while !runGame do
  tick ()   // Advance simulation
  render () // Render new view to back buffer
  flip ()   // Wait for display refresh and switch front and back buffers
```

Now, the idea is that the `tick ()` call runs all the simulation logic for one
step and that the simulation is implemented using Hopac jobs.  More specifically
we don't want any simulation code to run after the `tick ()` call returns.  This
is so that the `render ()` call has one consistent view of the world.

A clean way to achieve this is to create a *local scheduler* for running the
Hopac jobs that implement the game logic.  This way, after we've triggered all
the jobs waiting for the next game tick, we can simply wait until the local
scheduler becomes idle.  This means that all the jobs that run under the local
scheduler have become blocked waiting for something&mdash;possibly waiting for a
future game tick.

Enough with the motivation.  We'll represent time using a 64-bit integer type.

```fsharp
type Ticks = int64
```

And we have a variable that holds the current time.


```fsharp
let mutable currentTime : Ticks = 0L
```

Now, to integrate this concept of time with Hopac, we'll have a time server with
which we communicate through a timer request channel.

```fsharp
let timerReqCh : Ch<Ticks * Ch<unit>> = Ch.Now.create ()
```

Via the channel, a client can send a request to the server to send back a
message on a channel allocated for the request at the specified time.  To send
the request and allocate a new channel for the server's reply, we'll use the
`guard` combinator.  The following `atTime` function creates an alternative
that *encapsulates the whole protocol* for interacting with the time server:

```fsharp
let atTime (atTime: Ticks) : Alt<unit> =
  Alt.guard << Job.delay <| fun () ->
  let replyCh : Ch<unit> = Ch.Now.create ()
  Ch.send timerReqCh (atTime, replyCh) >>%
  Ch.Alt.take replyCh
```

A detail worth pointing out above is the use of the `Ch.send`
[*](http://hopac.github.io/Hopac/Hopac.html#def:val%20Hopac.Ch.send)
operation to send requests to the server asynchronously.  We already have the
client synchronously taking a reply from the server, so there is no need to have
the client synchronously waiting for the time server to take the request.  Using
`atTime` we can implement the `timeOut` alternative constructor used in the
earlier Kismet example:

```fsharp
let timeOut (afterTicks: Ticks) : Alt<unit> =
  assert (0L <= afterTicks)
  Alt.delay <| fun () ->
  atTime (currentTime + afterTicks)
```

What remains is to implement the time server itself.  Taking advantage of
existing data structures, we'll use a simple dictionary that maps ticks to lists
of reply channels to represent the queue of pending requests:

```fsharp
let requests = Dictionary<Ticks, ResizeArray<Ch<unit>>> ()
```

The time request server takes messages from the request channel and deals with
them, either responding to them immediately or adding them to the queue of
pending requests:

```fsharp
let timeReqServer =
  Ch.take timerReqCh >>= fun (atTime, replyCh) ->
  if currentTime <= atTime then
    Ch.send replyCh ()
  else
    let replyChs =
      match requests.TryGetValue atTime with
       | (true, replyChs) -> replyChs
       | _ ->
         let replyChs = ResizeArray<_>()
         requests.Add (atTime, replyChs)
         replyChs
    replyIvs.Add replyCh
    Job.unit ()
```

The time request server also uses an asynchronous send to reply to requests.
This time it is not only an optimization, but required, because it is possible
that within a selective communication the timer request is abandoned and there
is no client waiting for the request.  If the server would try to give the reply
synchronously, the server would be blocked indefinitely.

The above only implements a single iteration of the time request server.  We
need to start the server after we have created the local scheduler:

```fsharp
do! Job.server (Job.forever timeReqServer)
```

One final part of the implementation of time is a routine to advance time.  The
following `tick` job increments the current time and then sends replies to all
the requests at that time:

```fsharp
let tick = Job.delay <| fun () ->
  currentTime <- currentTime + 1L
  match requests.TryGetValue currentTime with
   | (true, replyChs) ->
     requests.Remove currentTime |> ignore
     replyChs
     |> Seq.iterJob (fun replyCh -> Ch.send replyCh ())
   | _ ->
     Job.unit ()
```

That concludes the implementation of the time server itself.

### Negative Acknowledgments

In the previous section the `guard`
[*](http://hopac.github.io/Hopac/Hopac.html#def:val%20Hopac.Alt.guard)
combinator was used to encapsulate the protocol for interacting with the custom
timer server.  This worked because the service provided by the time server is
idempotent.  If a client makes a request to the time server and later aborts the
request, that is, doesn't wait for the server's reply, it causes no harm.
Sometimes things are not that simple and a server needs to know whether client
actually committed to a transaction.  Hopac, like CML, supports this via the
`withNack`
[*](http://hopac.github.io/Hopac/Hopac.html#def:val%20Hopac.Alt.withNack)
combinator:

```fsharp
val withNack: (Alt<unit> -> Job<Alt<'x>>) -> Alt<'x>
```

The `withNack` combinator is like `guard` in that it allows an alternative to be
computed at instantiation time.  Additionally, `withNack` creates a *negative
acknowledgment alternative* that it gives to the encapsulated alternative
constructor.  If the constructed alternative is ultimately not committed to, the
negative acknowledgment alternative becomes available.  Consider the following
example:

```fsharp
let verbose alt = Alt.withNack <| fun nack ->
  printf "Instantiated and "
  Job.start (Alt.pick nack |>> fun () -> printfn "aborted.") >>%
  (alt |>>? fun x -> printfn "committed to." ; x)
```

The above implements an alternative constructor that simply prints out what
happens.  Let's consider three interactions using a `verbose` alternative.

```fsharp
> run (Alt.select [verbose (Alt.always 1); Alt.always 2]) ;;
Instantiated and committed to.
val it : int = 1
```

In the first case above, a verbose alternative is instantiated and committed to.
The negative acknowledgment is created, but does not become enabled.

```fsharp
> run (Alt.select [verbose (Alt.never ()); Alt.always 2]) ;;
Instantiated and aborted.
val it : int = 2
```

In the second case above, a verbose alternative is instantiated and aborted as
the second alternative is committed to.

```fsharp
> run (Alt.select [Alt.always 1; verbose (Alt.always 2)]) ;;
val it : int = 1
```

In the third case above, the first alternative is immediately committed to and
no verbose alternative is instantiated.  No code within the verbose alternative
constructor was executed and no negative acknowledgment alternative was created.

Negative acknowledgments can be useful as both as a mechanism that allows one to
cancel expensive operations for performance reasons (when the request is
idempotent) and also as a mechanism that allows one to encapsulate protocols
that otherwise couldn't be properly encapsulated as alternatives.

#### Example: Lock Server

An example that illustrates how `withNack`
[*](http://hopac.github.io/Hopac/Hopac.html#def:val%20Hopac.Alt.withNack)
can be used to encapsulate a non-idempotent request as an alternative is the
implementation of a *lock server*.  Here is a signature of a lock server:

```fsharp
type Server
type Lock

val start: Job<Server>
val createLock: Server -> Lock
val acquire: Server -> Lock -> Alt<unit>
val release: Server -> Lock -> Job<unit>
```

The idea is that a lock server allows a client to acquire a lock as an
alternative within a selective communication.  A client could, for example, try
to obtain one of several locks and proceed accordingly:

```fsharp
Alt.select [acquire server lockA >>=? fun () ->
              (* critical section A *)
              release server lockA
            acquire server lockB >>=? fun () ->
              (* critical section B *)
              release server lockB]
```

Or a client could use a timeout to avoid waiting indefinitely for a lock:

```fsharp
Alt.select [acquire server lock >>=? (* critical section *)
            Timer.Global.timeOut duration >>=? (* do something else *)]
```

What is important here is that the `acquire` alternative must work correctly
even in case that the operation is ultimately abandoned by the client.

Let's then describe the lock server example implementation.  We represent a lock
using a unique integer:

```fsharp
type Lock = Lock of int64
```

A request for the lock server is either an `Acquire` or a `Release`:

```fsharp
type Req =
 | Acquire of lock: int64 * replyCh: Ch<unit> * abortAlt: Alt<unit>
 | Release of lock: int64
```

An `Acquire` request passes both a reply channel and an abort alternative for
the server.  The server record just contains an integer for generating new locks
and the request channel:

```fsharp
type Server = {
  mutable unique: int64
  reqCh: Ch<Req>
}
```

The `release`

```fsharp
let release s (Lock lock) = Ch.give s.reqCh (Release lock)
```

and `createLock`

```fsharp
let createLock s =
  Lock (Interlocked.Increment &s.unique)
```

operations are entirely straightforward.  Note that in this example we simply
use an interlocked increment to allocate locks.

Note that this example implementation is not entirely type safe, because two
different lock servers might have locks by the same integer value.  The reason
for leaving the server exposed like this is that it is now easier to run the
code snippets of this example in an interactive session.  As this type safety
issue is not an essential aspect of the example, we leave it as an exercise for
the reader to consider how to plug this typing hole.

The `acquire` operation is where we'll use `withNack`:

```fsharp
let acquire s (Lock lock) = Alt.withNack <| fun abortAlt ->
  let replyCh = Ch.Now.create ()
  Ch.send s.reqCh (Acquire (lock, replyCh, abortAlt)) >>%
  Ch.Alt.take replyCh
```

Using `withNack` a negative acknowledgment alternative, `abortAlt`, is created
and then a reply channel, `replyCh`, is allocated and a request is created and
sent to the lock server `s`.  An asynchronous `send`
[*](http://hopac.github.io/Hopac/Hopac.html#def:val%20Hopac.Ch.send)
operation is used as there is no point in waiting for the server at this point.
Finally the alternative of taking the server's reply is returned.

Note that a new pair of a negative acknowledgment alternative and reply channel
is created each time an alternative constructed with `acquire` is instantiated.
This means that one can even try to acquire the same lock multiple times with
the same alternative and it will work correctly:

```fsharp
let acq = acquire s l
do! Alt.select [acq >>=? /* ... */
                acq >>=? /* ... */]
```

What remains is the implementation of the server itself.  We again make use of
readily available data structures to hold the state, that is pending requests to
active locks, of the lock server.

```fsharp
let start = Job.delay <| fun () ->
  let locks = Dictionary<int64, Queue<Ch<unit> * Alt<unit>>>()
  let s = {unique = 0L; reqCh = Ch.Now.create ()}
  (Job.server << Job.forever)
   (Ch.take s.reqCh >>= function
     | Acquire (lock, replyCh, abortAlt) ->
       match locks.TryGetValue lock with
        | (true, pending) ->
          pending.Enqueue (replyCh, abortAlt)
          Job.unit ()
        | _ ->
          Alt.select [Ch.Alt.give replyCh () |>>? fun () ->
                        locks.Add (lock, Queue<_>())
                      abortAlt]
     | Release lock ->
       match locks.TryGetValue lock with
        | (true, pending) ->
          let rec assign () =
            if 0 = pending.Count then
              locks.Remove lock |> ignore
              Job.unit ()
            else
              let (replyCh, abortAlt) = pending.Dequeue ()
              Alt.select [Ch.Alt.give replyCh ()
                          abortAlt >>=? assign]
          assign ()
        | _ ->
          // We just ignore the erroneous release request
          Job.unit ()) >>% s
```

As usual, the above server is implemented as a job that loops indefinitely
taking requests from the server's request channel.  The crucial bits in the
above implementation are the uses of `select`.  In both cases, the server
selects between giving the lock to the client and aborting the transaction using
the reply channel and the abort alternative, which was implemented by the client
using a negative acknowledgment alternative created by the `withNack`
combinator.

You probably noticed the comment in the above server implementation in the case
of an unmatched release operation.  We could also have a combinator that
acquires a lock, executes some job and then releases the lock:

```fsharp
let withLock (s: Server) (l: Lock) (xJ: Job<'x>) : Alt<'x> =
  acquire s l >>=? fun () ->
  Job.tryFinallyJob xJ (release s l)
```

This ensures that an acquire is properly matched by a release.

### On the Semantics of Alternatives

The alternatives of Hopac are heavily inspired by the events of Concurrent ML,
but the two are not precisely the same.  Whether or not you are familiar with
the semantics of CML, it is important to understand how alternatives are
evaluated in Hopac.  If you are not familiar with CML, you can ignore the
comparison made to CML in this section and just concentrate on the description
of how alternatives in Hopac behave.

The semantics of Concurrent ML events and Hopac alternatives are slightly
different.  Concurrent ML emphasizes *fairness* and *non-determinism*, while
Hopac emphasizes *performance* and *co-operation*.  In CML, when two or more
events are immediately available, the choice between them is made in a
*non-deterministic* fashion.  In Hopac, the first alternative that is available
will be chosen *deterministically*.  Consider the following expression:

```fsharp
Alt.choose
 [Alt.always 1
  Alt.always 2]
```

In Hopac, the above alternative will *deterministically* evaluate to 1, because
it is the first available alternative.  In CML, the similar event would
*non-deterministically* choose between the two events.  In this case, we could
get the same behavior in Hopac given a function `shuffle` that would reorder the
elements of a sequence randomly:

```fsharp
Alt.delay <| fun () ->
 Alt.choose
  (shuffle
    [Alt.always 1
     Alt.always 2])
```

The choice of the simpler deterministic semantics in the case of multiple
immediately available alternatives is motivated by performance considerations.
In order to provide the non-determinism, considerably more processing would need
to be performed.  Consider the following example:

```fsharp
Alt.choose
 [Alt.delay <| fun () -> printfn "A" ; Alt.always 1
  Alt.delay <| fun () -> printfn "B" ; Alt.always 2]
```

In Hopac, picking the above alternative prints `A` and nothing else.  In CML,
the similar event would print both `A` and `B`.  In other words, in the initial
phase, Hopac evaluates alternatives *lazily*, while CML evaluates events
*eagerly*.  Hopac can therefore run more efficiently in cases where an
alternative happens to be immediately available.

The above examples are contrived.  In real programs, choices are made over
*communications between separate jobs* that may run in parallel.  This means
that in many cases the initial lazy and deterministic evaluation of alternatives
makes no difference except for performance.  In cases where none of the
alternatives is immediately available, the behavior of Hopac and CML is
essentially the same.  However, it is obviously possible to write programs that
rely on either the Hopac style lazy and deterministic or the CML style eager and
non-deterministic initial choice.

Channels, Mailboxes, IVars, MVars, ...
--------------------------------------

In this document we have mostly used channels in our examples.  The Hopac
library, like CML, also directly provides other communication primitives such as
`Mailbox`[*](http://hopac.github.io/Hopac/Hopac.html#def:type%20Hopac.Mailbox),
`IVar`[*](http://hopac.github.io/Hopac/Hopac.html#def:type%20Hopac.IVar),
`MVar`[*](http://hopac.github.io/Hopac/Hopac.html#def:type%20Hopac.MVar)
and
`Lock`[*](http://hopac.github.io/Hopac/Hopac.html#def:type%20Hopac.Lock).
These other primitives are optimized for the particular communication patterns
they support, but most of them could be implemented using only jobs and channels
as shown in the book
[Concurrent Programming in ML](http://www.cambridge.org/us/academic/subjects/computer-science/distributed-networked-and-mobile-computing/concurrent-programming-ml),
for example.  When programming with Hopac, it, of course, makes sense to use the
optimized primitives where possible.  So, for example, rather than allocating a
channel and starting a job for a one-shot communication, it makes sense to use
an
`IVar`[*](http://hopac.github.io/Hopac/Hopac.html#def:type%20Hopac.IVar),
which implements the desired semantics more efficiently.  On the other hand, it
is reassuring that these optimized primitives, and many others, can be
implemented using only jobs and channels.  This means that there is no need for
the Hopac library to be continuously extended with new communication primitives.

Going Further
-------------

For learning more about Concurrent ML style programming, I highly recommend
[John Reppy](http://people.cs.uchicago.edu/~jhr/)'s book
[Concurrent Programming in ML](http://www.cambridge.org/us/academic/subjects/computer-science/distributed-networked-and-mobile-computing/concurrent-programming-ml).

The wiki also has the page
[Questions and Answers](https://github.com/Hopac/Hopac/wiki/Questions-and-Answers).
Feel free to add questions there.
