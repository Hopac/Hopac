Notes on the Internal Implementation of Hopac
=============================================

At the moment, this document contains some quickly written random notes on the
internal implementation details of Hopac.  It will probably help if the reader
is already familiar with many of the basic ideas and issues in the
implementation of something like Hopac.

> The code snippets in here are illustrative and not necessarily valid F# code.

The actual definitions may be written in C# and contain implementation details
not discussed here.  Also note that these are internal implementation details
and subject to change.

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

While the **Work** class has support for exception handling, it does not have
support for success continuations.  That is the domain of the **Job** and
**Cont** classes.

The **Job** class represents an operation that may block.

```fsharp
type Job<'a> = class
  abstract DoJob: byref<Worker> * Cont<'a> -> unit
end
```

It contains a single method **DoJob** that is passed both a reference to the
current worker, and a **Cont** object, whose **DoCont** method the job may (or
may not) ultimately arrange to be called with the result computed by the job.

Note the throughout the documentation the term job is used, depending on
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

Let's make some of this more concrete by looking at the implementations of the
Job monad operations, **result** and **&gt;&gt;=** (or bind).  The **result**
operation is quite straightforward:

```fsharp
let result (x: 'x) =
  {new Job<'x> () with
    override xJ.DoJob (wr, xK) =
     xK.DoCont (&wr, x)}
```

**result x** is a job **xJ** that simply calls the **DoCont** method of the
continuation **xK** with value **x** and the worker reference **wr** it
was given.

The **&gt;&gt;=** (or bind) operation, is considerably more complicated:

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

**xJ &gt;&gt;= x2yJ** is a job **yJ** that first executes the **xJ** job.  In
order to do that, it needs to allocate a new continuation object **xK**, which it
passes to the **DoJob** method of **xJ**.  That continuation object needs to
properly implement all that the **Cont** class represents.  In particular,

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

Let's then consider a somewhat simplified implementation of a write once
variable or **IVar**.  While this implementation is simplified, it makes use
of implementation tricks that are used in the actual Hopac implementation.  The
signature for IVars could look like roughly like this:

```fsharp
module IVar =
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
A naive implementation would actually need to allocate a new object.  In this
case we can avoid that by simply inheriting **IVar** from **Job**.

The **read** and **create** functions are therefore entirely trivial:

```fsharp
let read (xV: IVar<'x>) : Job<'x> = xV :> Job<'x>
let create () = IVar<'x> ()
```

The implementation of the read operation is then in the **DoJob** method that
needs to be implemented in the IVar class:

```fsharp
type IVar<'x> () = class
  inherit Job<'x>
  val mutable IsFull: bool
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
the continuation object, that also includes the **Next** field that we use to
implement a linked list.  The lock is only held for the duration of a few
machine instructions.  The same goes for the actual implementation of channels,
for example.

The **fill** operation is not much more complex.  The illustrative code below
omits any error checking (trying to fill an IVar twice would be bad).  It also
skips the proper protocol for pushing items to the work item stack.

```fsharp
let fill (xV: IVar<'x>) (x: 'x) : Job<unit> =
  {new Job<unit> () with
    override xF.DoJob (wr, uK) =
     Monitor.Enter xV
     xV.Value <- x
     xV.IsFull <- true
     Monitor.Exit xV

     let readers = xV.Readers :> Work
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
machine instructions.  After the state is update and the lock released,
subsequent read operations finish immediately.  The read operations that
have executed before have been pushed to the Readers stack, which must now
be cleared by the fill operation.  The clearing loop makes use of the **Value**
field to store the result of the *read* operation.  That was not a slip.
The **Value** field is written the result of the *read* operation that was
blocked in the **Readers** stack.  The continuation of the read operation is
then pushed to the work item stack of the worker thread.  Finally, the **fill**
operation is complete and the continuation **uK** is invoked.

The actual implementions of these operations in Hopac are somewhat more complex,
due to the support for selective communication, and use further low level
optimizations such as taking advantage of volatile reads and write and
interlocked primitives.
