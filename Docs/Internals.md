Notes on the Internal Implementation of Hopac
=============================================

This document describes some aspects of the internal implementation of Hopac.
This is not meant to be something that a user of Hopac would need to understand,
but potential users might find this interesting nevertheless.  The code snippets
in here are illustrative and not necessarily valid F# or C# code.  The actual
definitions corresponding to a snippet of F# may be written in C# and contain
implementation details not discussed here.  Also note that these are internal
implementation details and subject to change.  The internal design of Hopac has
evolved considerably during the course of developing it.  This document makes
mention of "tricks" or techniques used in the implementation.  The possibility
of using many of these techniques in the implementation is something that was
not a priori obvious when I started working on the implementation and the
discovery and subsequent incorporation of some of these techniques into the
implementation has improved the performance of Hopac significantly.

Scheduler and Workers
---------------------

At the core of Hopac is a work distributing scheduler.  In the current
implementation, the scheduler is only a conceptual entity.  In other words,
there is no single class or module that would implement the whole of the
scheduler.  However, it is likely that the internals will be refactored to make
the scheduler into something that can be largely replaced and/or configured to
better suit the needs of particular applications.

The scheduler basically consists of a bit of global state, defined in the
**Scheduler** class, and a number of worker threads and their associated internal
data structures (**Worker** struct).  One worker thread per hardware thread
(Environment.ProcessorCount) is used.  The worker threads execute work items,
described in the next section, very much like the .Net thread pool does, and
collaborate to distribute those work items among the worker threads.

This is not all, however, because other components of Hopac can also
*effectively* make scheduling decisions.  For example, when a new lightweight
thread is spawned, there are multiple alternatives:

* The new thread is queued via the scheduler and the current thread is
  continued.
* The current thread is queued via the scheduler and the new thread is started
  immediately.
* Both the new and current threads are queued and some previously queued thread
  is started instead.
* ...

Choices like these are not made by a single specific component in Hopac.
Instead, these decisions are made on a case by case basis in the various
primitives of Hopac.

Work items and Worker threads
-----------------------------

The **Work** class represents a work item.

```fsharp
type Work = class
  inherit Handler
  val mutable Next: Work
  abstract DoWork: byref<Worker> -> unit
end
```

This is comparable to what the .Net thread pool calls user work items, but also
includes support for exception handling by inheriting from the **Handler**
class.

```fsharp
type Handler = class
  abstract DoHandle: byref<Worker> * exn -> unit
end
```

Work items are executed by worker threads.  A worker thread is represented by
the **Worker** struct.

```fsharp
type Worker = struct
  val mutable WorkStack: Work
  val mutable Handler: Handler
end
```

A worker thread executes a loop that runs work items that are obtained by
popping them from the work item stack, **WorkStack**.  When a work item is
executed, by calling its **DoWork** method, it is passed a reference to the
**Worker** struct of the worker thread that calls it.  The main purpose of this
convention is to make it possible to access the work item stack of the worker
thread as efficiently as possible.  By pushing work items to the work item
stack of a worker thread, a work item can effectively spawn new lightweight
threads.  The **Work** class also contains a **Next** field (mainly) for this
purpose as the work item stack is implemented as a linked list.

The loop in a worker thread is responsible for catching any unhandled
exceptions raised by the execution of a work item and passing them to the
current exception handler.  This way individual work items do not generally
need to implement try-catch blocks (and there is a little less overhead to
executing a work item).  The current handler is referenced by the **Handler**
field of the **Worker** struct and is updated by both the worker thread loop
and individual work items that specifically want to install a new handler.

Lightweight threads: Job and Cont objects
-----------------------------------------

While the **Work** class has support for exception handling, it does not have
support for success continuations.  That is the domain of the **Job** and
**Cont** classes, which allow one to express lightweight threads.

The **Job** class represents an operation that may block.

```fsharp
type Job<'a> = class
  abstract DoJob: byref<Worker> * Cont<'a> -> unit
end
```

It contains a single method **DoJob** that is passed both a reference to the
current worker, and a **Cont** object, whose **DoCont** method the job may (or
may not) ultimately arrange to be called with the result computed by the job.

Note that throughout the documentation the term job is used, depending on
context, to refer to either a class that inherits from **Job** or to the kind
of lightweight threads (runtime objects) that one can define using **Job** and
**Cont** objects.

The **Cont** class represents a continuation.

```fsharp
type Cont<'a> = class
  inherit Work
  val mutable Value: 'a
  abstract DoCont: byref<Worker> * 'a -> unit
end
```

It inherits the **Work** class, and via **Work**, the **Handler** class.  So,
if we ignore the selective communication mechanism for now, then, in fact,
**Cont** represents:

* a success continuation (**DoCont**),
* a failure continuation (**DoHandle**), and
* a work item or lightweight thread (**DoWork**).

This is one of many "tricks" used in Hopac to reduce allocations and copying of
data and improve performance.  So, when a **DoJob** method is passed a **Cont**
object, it actually receives all three of those "capabilities".  In case of
success, the job can call **DoCont**, or, in case of failure, it can call
**DoHandle**, or, in case the job needs to block, it can add the continuation,
as a **Work** item (remember the **Next** field), into a relevant queue and
return, which returns control to the worker thread loop, which then continues
with another work item.

In order to make the work item aspect really useful, the **Cont** class also
contains the **Value** field.  The **Value** field is there so that when a
blocked job is finally resumed, the value that the continuation is expecting
can be written to the **Value** field, and the continuation object can be
pushed to a work item stack.  And this can be done without having to allocate
a new **Work** object.  (On the other hand, this means that continuations are
one-shot or single threaded - one must be careful not to try to queue a single
continuation object into multiple queues at the same time.)

The Job Monad
-------------

To program lightweight threads directly in terms of **Job**, **Cont**,
**Handler** and **Work** objects as well as worker threads would be rather
cumbersome.  Fortunately the underlying mechanisms can be abstract away in the
form of a **Job** monad with the usual **result** and **>>=** (or bind)
operations.

The **result** operation is quite straightforward:

```fsharp
let result (x: 'x) =
  {new Job<'x> () with
    override xJ.DoJob (wr, xK) =
     xK.DoCont (&wr, x)}
```

**result x** is a job **xJ** that simply calls the **DoCont** method of the
continuation **xK** with value **x** and the worker reference **wr** it
was given.

The **>>=** (or bind) operation, is considerably more complicated:

```fsharp
let (>>=) (xJ: Job<'x>) (x2yJ: 'x -> Job<'y>) : Job<'y> =
  {new Job<'y>() with
    override yJ.DoJob (wr, yK) =
      xJ.DoJob (&wr, {new Cont<'x>() with
       override xK.DoHandle (wr, e) =
        yK.DoHandle (&wr, e)
       override xK.DoWork (wr) =
        xK.DoCont (&wr, xK.Value)
       override xK.DoCont (wr, x) =
        let yJ' = x2yJ x
        yJ'.DoJob (&wr, yK)})}
```

**xJ >>= x2yJ** is a job **yJ** that first executes the **xJ** job.  In order to
do that, it needs to allocate a new continuation object **xK**, which it passes
to the **DoJob** method of **xJ**.  That continuation object needs to properly
implement all that the **Cont** class represents.  In particular,

* the **DoHandle** method implements the failure (or exception) continuation,
* the **DoCont** method implements the success continuation, and, interestingly,
* the **DoWork** method *also* implements the success continuation for the case
  that the job was suspended, and in that case, the **Value** field of the
  continuation object **xK** contains the result of the job.

Take a look at the **DoCont** method above.  It simply calls **x2yJ** with
**x**, which is the result of the **xJ** job and gets a new job **yJ'** as
the result.  It then calls the **DoJob** method of **yJ'**.  There is no need to
wrap the call with an exception handling block.  If the user defined code
evaluted with the expression **x2yJ x** throws an exception, it will be handled
by the worker thread and the exception will be passed to the current handler.

Take a look at the **DoHandle** method above.  It simply forwards the exception
**e** to the handler of **yK**.  This is another trick that basically makes it
zero cost, **O(0)**, to pass around the exception handler with the tradeoff that
every success continuation must also implement such a forwarding failure
continuation and that, due to the forwarding, calling the exception handler is
then an **O(n)** operation, where **n** is the depth of the success continuation
(or number of stack of frames in a more traditional implementation).

Non selective, synchronous message passing primitives
-----------------------------------------------------

Let's then consider a somewhat simplified implementation of a write once
variable or **IVar**.  While this implementation is simplified, it makes use
of implementation tricks that are used in the actual Hopac implementation.  The
signature for IVars could look like roughly like this:

```fsharp
type IVar<'x>
val create: unit -> IVar<'x>
val fill: IVar<'x> -> 'x -> Job<unit>
val read: IVar<'x> -> Job<'x>
```

Briefly, the idea is that an IVar is initially created as empty and the program
is organized so that only the owner of the IVar is allowed to **fill** the IVar
with a value.  An attempt to **read** an empty IVar blocks until the IVar is
filled.

An important detail in the above signature is that the **fill** operation is
implemented as a job.  Remember that a job is an operation that may block.  In
this case, a fill operation actually never blocks, but it may *unblock* other
jobs that attempted to read the IVar earlier.  In order to efficiently unblock
jobs, the fill operation needs access to a work item stack into which the
blocked jobs can then be pushed.

Another important detail is the type of the **read** operation.  It is
essentially a function that converts an **IVar** into a **Job**.  Operations
with a type like this can be found in most Hopac communication primitives.
A naive implementation would actually need to allocate a new object, but we can
avoid that by simply inheriting **IVar** from **Job**.

The **read** and **create** functions are therefore entirely trivial:

```fsharp
let read (xV: IVar<'x>) : Job<'x> = xV :> Job<'x>
let create () = IVar<'x> ()
```

The implementation of the read operation is in the **DoJob** method that needs
to be implemented in the IVar class:

```fsharp
type IVar<'x> () = class
  inherit Job<'x>
  val volatile IsFull: bool
  val mutable Value: 'x
  val mutable Readers: Cont<'x>
  override xV.DoJob (wr, xK) =
    if xV.IsFull then
      xK.DoCont (&wr, xV.Value)
    else
      Monitor.Enter xV
      if xV.IsFull then
        Monitor.Exit xV
        xK.DoCont (&wr, xV.Value)
      else
        xK.Next <- xV.Readers
        xV.Readers <- xK
        Monitor.Exit xV
end
```

For efficiency, the above illustrative implementation uses double checked
locking.  As you can see, the entire operation can be implemented without
additional allocations; the caller of the read operation has already allocated
the continuation object, which also includes the **Next** field that we use to
implement a linked list.  The lock is held for the duration of only a few
machine instructions.  In a non-contrived application, the scope for lock
contention is extremely small.  The same goes for the actual implementation of
channels, for example.

The **fill** operation is not much more complex.  The illustrative code below
omits any error checking (trying to fill an IVar twice would be bad).  It also
skips the proper protocol for pushing items to the work item stack.  This is
done in order to illustrate how efficiently these operations can be
implemented.  (Compare this to what a combination of .Net thread pool, tasks
and the F# async framework would need to do.)

```fsharp
let fill (xV: IVar<'x>) (x: 'x) : Job<unit> =
  {new Job<unit> () with
    override xF.DoJob (wr, uK) =
     xV.Value <- x
     xV.IsFull <- true

     Monitor.Enter xV
     let readers = xV.Readers :> Work
     Monitor.Exit xV

     xV.Readers <- null

     let rec loop readers =
       match readers with
        | null -> uK.DoCont (&wr, ())
        | reader ->
          let next = reader.Next :?> Cont<'x>
          reader.Value <- x
          reader.Next <- wr.WorkStack
          wr.WorkStack <- reader
          loop next

     loop readers}
```

As you can see, the **fill** operation also holds the lock for only a few
machine instructions.  After the state is updated and the lock released,
subsequent read operations finish immediately.  The read operations that
have executed before have been pushed to the Readers stack, which must now
be cleared by the fill operation.  The clearing loop makes use of the **Value**
field to store the result of the *read* operation.  That was not a slip.
The **Value** field is written the result of the *read* operation that was
blocked in the **Readers** stack.  The continuation of the read operation is
then pushed to the work item stack of the worker thread.  Note that there is
no need to use locks at this point.  The work item stack is owned by the thread
and can be accessed without locking.  Finally, the **fill** operation is
complete and the continuation **uK** is invoked.  Note that aside from the Job
object that represents the fill operation itself, no other objects need to be
allocated.  All the blocked lightweight threads can be resumed without any
additional allocations using only a few machine instructions.

The actual implementions of these operations in Hopac are somewhat more
complex due to the support for selective communication and use of further low
level optimizations such as taking advantage of volatile reads and writes and
interlocked operations.

Tail calls and Trampolining
---------------------------

As should be obvious from the previous sections, the continuation mechanism of
Hopac requires tail call optimization.  Unfortunately, the state of tail call
optimization support in .Net is a mixed bag.  Current C# compilers do not insert
**tail** prefixes on call instructions at tail positions.  The current F#
compiler (3.1) inserts tail prefixes on some calls, but not all tail
calls&mdash;even with the tail call optimization enabled.  (I seem to recall
that an earlier version of the F# compiler was more aggressive with the
prefixes.)  On the other hand, Microsoft's current 64-bit JIT does a decent job
of implementing tail calls&mdash;even without tail prefixes.  Unfortunately, the
same cannot be said of all .Net JITs.

In the beginning, Hopac used a simple ad hoc tool to add tail prefixes to
virtual tail calls in the Hopac libraries (basically all the places where Hopac
relies on tail calls).  Unfortunately, even with the tail prefixes some tail
calls simply won't be optimized properly.  Furthermore, adding tail prefixes
sometimes makes performance worse.

Without the tool, Hopac would have to rely on tail call optimization done by the
JIT, which works reasonably well with the 64-bit JIT.  However, it doesn't
always work and doesn't work on other JIT compilers.  So, Hopac now implements
*trampolining* that allows Hopac code to work even without any tail call
optimizations.  The additional runtime cost of trampolining appears to be
surprisingly small.  This is because continuations in Hopac are already
allocated from the heap and pushing an item to the local work item stack of a
worker can be done very quickly as shown in previous sections.  To implement
trampolining, the **Worker** struct contains an additional field:

```csharp
struct Worker {
  // ...
  void *StackLimit;
};
```

This field is initialized using a bit of unsafe code with a pointer value that
is used as an estimate of a safe stack limit.  The stack limit could be computed
quickly from the address of the Worker struct (**ref wr**), but there is no way
to tell the .Net JIT that the Worker struct is stack allocated and it would
generate a few redundant instructions to pin the struct, so it is better to
cache the value.  Continuation invocations are then done via a helper function
that uses a bit of unsafe code to check whether the stack limit has been
reached, and either trampolines via the work item stack or calls the
continuation directly:

```csharp
static void Do<T>(Cont<T> tK, ref Worker wr, T value) {
  unsafe {
    byte stack;
    void *ptr = &stack;
    if (ptr < wr.StackLimit) {
      tK.Value = value;
      tK.Next = wr.WorkStack;
      wr.WorkStack = tK;
    } else {
      tK.DoCont(ref wr, value);
    }
  }
}
```

Carefully applying this trampolining helper only where necessary makes the
overhead surprisingly small.

Could something like Hopac be even faster?
------------------------------------------

Yes, definitely.

The previous sections basically showed what makes Hopac fast: the primitive
operations within Hopac are designed to avoid memory allocations, locks are
only held for extremely short periods and many operations, such as suspending
and resuming a lightweight thread, require only a few machine instructions
and many operations can be done without locking.  (Note that by "locks" and
"locking" I mean both actual locks and other synchronization objects as well
as lower level techniques using volatile and interlocked operations.)

Indeed, I believe that it is not possible to do much better on the .Net
platform and in the F# and C# languages as they stand today.  According to my
tests, even async state machines generated by the C# compiler run slower than
similar Hopac code - even when running only sequential code.  In Hopac, there
is no overhead from having to cross abstraction boundaries between the .Net
thread pool, the tasks parallel library and the async framework.

On the other hand, at the moment, the .Net platform seems to be extremely weak
when it comes to eliminating certain forms of abstraction penalty.  Take a look
at the IVar operations implemented in the previous section.  They would be
prime candidates for inlining.  But the current .Net runtimes do not seem to be
able to do that.  As a result, the job monad incurs an order of magnitude
overhead compared to what would be possible in an implementation that would be
able to inline operations such as the **read** and **fill** operations on
IVars.

The F# compiler has support for inline functions.  Unfortunately, inline
functions do not mix well with encapsulation.  Methods marked as **internal**
cannot be implemented inside inline functions exported outside of their module.
By exposing the internals of Hopac it would be possible to use F# inline
functions.  According to my tests that can give something like up to 30% better
performance.  That is much less than one could expect, because the optimizer of
F# is not particularly clever (e.g. partially applied functions cannot be
inlined trivially and compiler analyses by the F# compiler are not powerful
enough to help).  Nevertheless, in some cases it might make a difference and it
might make sense to provide a build of Hopac with such inline support.

Selective message passing primitives
------------------------------------

TBD
