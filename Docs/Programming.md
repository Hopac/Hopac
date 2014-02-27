Programming in Hopac
====================

Hopac provides a programming model that is heavily inspired by
[John Reppy](http://people.cs.uchicago.edu/~jhr/)'s *Concurrent ML* language.
The book
[Concurrent Programming in ML](http://www.cambridge.org/us/academic/subjects/computer-science/distributed-networked-and-mobile-computing/concurrent-programming-ml)
is the most comprehensive introduction to Concurrent ML style programming.  This
document contains some discussion and examples on Hopac programming techniques.
In the future, this document might grow to a proper introduction to Hopac.

The Hopac Programming Model
---------------------------

There are two central aspects of Hopac that shape the programming model.

The first aspect is that, threads, which are called *jobs*, in Hopac are
extremely lightweight.  On modern machines you can spawn tens of millions of new
jobs in a second.  Because a job takes only a very small amount of memory,
starting from tens of bytes, a program may have millions of jobs on a modern
machine at any moment.  (Of course, at any moment, most of those jobs are
suspended, because modern machines still only have a few, or at most a few
dozen, processor cores.)  When programming in Hopac, one can therefore start new
jobs in situations where it would simply be unthinkable when using heavyweight
threads.

The other aspect is that Hopac provides first-class, higher-order, selective,
synchronous, lightweight, message passing primitives in the form of channels
(Ch) and alternatives (Alt) for coordinating and communicating between jobs.
That is a mouthful!  Let's open it up a bit.

* **First-class** means that channels and alternatives are ordinary values.
  They can be bound to variables, passed to and returned from functions and can
  even be sent from one job to another.
* **Higher-order** means that primitive alternatives can be combined and
  extended with user defined procedures to build more complex alternatives.
* **Selective** means that a form of choice or disjunction between alternatives
  is supported.  An alternative can be constructed that, for example, offers to
  give a message to another job *or* take a message from another job.  The
  choice of which operation is performed then depends on whichever alternative
  becomes available at runtime.
* **Synchronous** means that rather than building up a queue of messages for
  another job to examine, jobs can communicate via rendezvous.  Two jobs can
  meet so that one job can give a message to another job that takes the message.
* **Lightweight** means that creating a new synchronous channel takes very
  little time (a single memory allocation) and a channel takes very little
  memory on its own.

What this all boils down to is that Hopac basically provides a kind of language
for expressing concurrent control flow.

On Memory Usage
---------------

An important property of Hopac jobs and synchronous channels is that a system
that consist of **m** jobs that communicate with each other using **n**
synchronous channels (and no other primitives) requires **Theta(m + n)** space
for the jobs and channels.

That may sound obvious, but many concurrent systems,
e.g. [Erlang](http://www.erlang.org/) and F#'s
[MailboxProcessor](http://msdn.microsoft.com/en-us/library/ee370357.aspx), are
built upon asynchronous message passing primitives and in such systems message
queues can collect arbitrary numbers of messages when there are differences in
speed between producer and consumer threads.  Synchronous channels do not work
like that.  A synchronous channel doesn't hold a buffer of messages.  When a
producer job tries to give a message to a consumer job via synchronous channels,
the producer is suspended until a consumer job is ready to take the message.  A
synchronous channel provides something that is much more like a control flow
mechanism, like a procedure call, rather than a passive buffer for passing data
between threads.  This property can make it easier to understand the behaviour
of concurrent programs.

Of course, the bound **Theta(m + n)** does not take into account space that the
jobs otherwise accumulate in the form of data structures other than the
synchronous channels.

### Garbage Collection

Another aspect that is important to understand is that Hopac jobs and channels
are basic simple .Net objects and can be garbage collected.  Specifically, jobs
and channels do not inherently hold onto disposable system resources.  (This is
unlike the
[MailboxProcessor](http://msdn.microsoft.com/en-us/library/ee370357.aspx), for
example, which is disposable.)  What this means in practise is that most jobs do
not necessarily need to implement any special kill protocol.  A job that is
blocked waiting for communication on a channel that is no longer reachable can
(and will) be garbage collected.  Only jobs that explicitly hold onto some
resource that needs to be disposed must implement a kill protocol to explicitly
make sure that the resource gets properly disposed.

Example: Updatable Storage Cells
--------------------------------

In the book
[Concurrent Programming in ML](http://www.cambridge.org/us/academic/subjects/computer-science/distributed-networked-and-mobile-computing/concurrent-programming-ml),
[John Reppy](http://people.cs.uchicago.edu/~jhr/) presents as the first
programming example an implementation of updatable storage cells using
Concurrent ML channels and threads.  While this example is not exactly something
that one would do in practise, because F# lready provides ref cells, it does a
fairly nice job of illustrating some core aspects of Concurrent ML.  So, let's
reproduce the same example with Hopac.

Here is the signature for our updatable storage cells:

```fsharp
type Cell<'a>
val cell: 'a -> Job<Cell<'a>>
val get: Cell<'a> -> Job<'a>
val put: Cell<'a> -> 'a -> Job<unit>
```

The **cell** function creates a job that creates a new storage cell.  The
**get** function creates a job that returns the contents of the cell and the
**put** function creates a job that updates the contents of the cell.

The basic idea behind the implementation is that the cell is a concurrent
*server* that responds to **Get** and **Put** request.  We represent the
requests using the **Request** discriminated union type:

```fsharp
type Request<'a> =
 | Get
 | Put of 'a
```

To communicate with the outside world, the server presents two channels: one
channel for requests and another channel for replies required by the get
operation.  The **Cell** type is a record of those two channels:

```fsharp
type Cell<'a> = {
  reqCh: Ch<Request<'a>>
  replyCh: Ch<'a>
}
```

The **put** operation simply gives the **Put** request to the server via the
request channel:

```fsharp
let put (c: Cell<'a>) (x: 'a) : Job<unit> =
  Ch.give c.reqCh (Put x)
```

The **get** operation gives the **Get** request to the server via the request
channel and then takes the server's reply from the reply channel:

```fsharp
let get (c: Cell<'a>) : Job<'a> = job {
  do! Ch.give c.reqCh Get
  return! Ch.take c.replyCh
}
```

Finally, the **cell** operation actually creates the channels and starts the
concurrent server job:

```fsharp
let cell (x: 'a) : Job<Cell<'a>> = job {
  let reqCh = Ch.Now.create ()
  let replyCh = Ch.Now.create ()
  let rec server x = job {
        let! req = Ch.take reqCh
        match req with
         | Get ->
           do! Ch.give replyCh x
           return! server x
         | Put x ->
           return! server x
      }
  do! Job.start (server x)
  return {reqCh = reqCh; replyCh = replyCh}
}
```

The concurrent server is a job that loops indefinitely taking requests from the
request channel.  When the server receives a **Get** request, it gives the
current value of the cell on the reply channel and then loops to take another
request.  When the server receives a **Put** request, the server loops with the
new value to take another request.

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

Inspired by this example there is benchmark program, named
[Cell](https://github.com/VesaKarvonen/Hopac/tree/master/Benchmarks/Cell), that
creates large numbers of cells and large numbers of jobs running in parallel
that perform updates on randomly chosen cells.  While the benchmark program is
not terribly exciting, it nicely substantiates the claims made in the first
section about the lightweight nature of Hopac jobs and channels.

Example: Kismet
---------------

The updatable storage cell example in the previous section may have seemed
rather unrealistic.  The server job of a storage cell doesn't do much and it
probably doesn't seem like something for which you'd even consider starting a
separate thread, no matter how lightweight such a thread would be.  In this
section we'll sketch an example that might be a bit more compelling, although in
a way it is also quite unreal.

[UnrealScript](http://en.wikipedia.org/wiki/UnrealScript) is the scripting
language of the [Unreal Engine](http://en.wikipedia.org/wiki/Unreal_Engine) and
is used for making games.
[Kismet](http://en.wikipedia.org/wiki/UnrealEd#Kismet) is a tool that enables
artists to create scripts in UnrealScript using a visual interface.  Working
with Kismet, artists can basically create games by combining building blocks
created by programmers.  Those building blocks can be seen as black boxes that
have some inputs, outputs and have some interesting behaviour mapping the inputs
to outputs.

On the Wikipedia page on [UnrealEd](http://en.wikipedia.org/wiki/UnrealEd) there
is a screenshot of a simple system built using Kismet.  Take a moment to look at
the screenshot:
[Roboblitz](http://upload.wikimedia.org/wikipedia/en/e/e6/Kismet_Roboblitz.PNG).
As you can see, there are basic reusable blocks like **Bool**, **Compare Bool**,
**Delay**, and **Matinee** that have some inputs, outputs and some behaviour.

Kismet, UnrealScript and Unreal Engine, in general, have semantics that have
been designed for making games.  In fact, I've never actually programmed in
UnrealScript or used Kismet, but a curious mind might wonder how could black
boxes like that be implemented?  Could we build something similar using Hopac?

Let's first consider the **Compare Bool** box.  Looking at the screenshot and
making an educated guess, it seems to define an input event **In** and two
output events **True** and **False** and it also seems to read a **Bool**
variable.  It would seem that the idea is that when the box receives the **In**
event, it signals either the **True** or the **False** event depending on the
current value of the **Bool** variable.  Something like that can be quite
concisely expressed a Hopac job:

```fsharp
let CompareBool (comparand: ref<bool>)
                (inCh: Ch<'a>)
                (onTrueCh: Ch<'a>)
                (onFalseCh: Ch<'a>) : Job<_> =
  Job.forever
   (Ch.take inCh >>= fun x ->
    Ch.give (if !comparand then onTrueCh else onFalseCh) x)
```

The **CompareBool** function creates a job that loops indefinitely taking
messages from the given **inCh** channel and then giving those messages away on
either the **onTrueCh** or the **onFalseCh** channel depending on the value held
by the **comparand** ref cell.  As you can see, the above **CompareBool** job
doesn't care about the type of the signals being sent on the channels.  It just
copies the received value **x** to the chosen output.

Let's then consider the **Delay** box.  Making another educated guess and
simplifying a bit, it takes as input events **Start** and **Stop** (I leave
**Pause** as an exercise for the reader), output events **Finished** and
**Aborted** and also a time value **Duration**.  It would seem that the idea is
that when the box receives the **Start** event, it starts a timer that counts
down for the specified **Duration** after which the **Finished** event is
signaled.  Also, if during the countdown a **Stop** signal is received then the
**Aborted** signal is signaled instead.  Here is how something like that could
be expressed as a Hopac job:

```fsharp
let Delay (duration: ref<TimeSpan>)
          (startCh: Ch<'a>)
          (stopCh: Ch<'b>)
          (finishedCh: Ch<'a>)
          (abortedCh: Ch<'b>) : Job<_> =
  let rec initial () = Ch.take startCh >>= started
  and started x =
    Alt.pick (Ch.Alt.take stopCh      >=> Ch.give abortedCh
          <|> Alt.timeOut (!duration) >=> fun () -> Ch.give finishedCh x) >>= initial
  initial ()
```

The **Delay** function creates a job that, using a pair of mutually recursive
functions, representing two different states of the Delay job, loops
indefinitely.  In the **initial** state it takes an input from the **startCh**
channel and then transfers to the **started** state.  In the **started** state
it sets up two alternatives.  The first alternative takes a messages from the
**stopCh** channel and then gives the message on the **abortedCh** channel.  The
second alternative starts a **timeout** alternative for the current value of
**duration** and then gives the message taken in the **initial** state on the
**finishedCh** channel.  Whichever of those alternatives becomes enabled first
will then be committed to during runtime and the other will be discarded.

Those previous snippets are just two of the necessary building blocks.  Assuming
we would have all of the building blocks packaged in similar style, what remains
is translation of the configuration specified in the screenshot into code.  Here
is a small snippet of a sketch of how what the end result could look like:

```fsharp
let ch_1 = Ch.Now.create ()
let ch_2 = Ch.Now.create ()
let ch_3 = Ch.Now.create ()
// ...
let bMoved = ref false
// ...
do! Job.start <| CompareBool bMoved ch_1 ch_2 ch_3
do! Job.start <| Sink ch_2
do! Job.start <| Delay (ref (TimeSpan.FromSeconds 3.14)) ch_3 ch_4 ch_5 ch_6
// ...
```

The above initialization code sketch first creates shared channels and
variables.  Then the desired jobs are created and started passing to them the
previously created channels.  One more thing in the above is the use of a
**Sink** job to take messages from output channels that are not explicitly
connected to any other box.  The **Sink** could be implemented like this:

```fsharp
let Sink inCh = Job.forever (Ch.take inCh)
```

Now, games often have their own specific notion of time, different from
wall-clock time, which means that for programming games, the sketched
implementation of **Delay** would not give the desired meaning of time.  (But
you can certainly implement a notion of time more suitable for games on top of
Hopac.)  Also, the way variables are represented as mutable ref cells is a bit
naive.  In a real system, one would probably also want to specify boxes using
[directional channels](https://github.com/VesaKarvonen/Hopac/blob/master/Libs/Hopac.Extra/DirCh.fsi),
which would allow an editor to automatically understand input-output
relationships.  Unrealistic as it may be, this sketch has hopefully given you
something interesting to think about!
