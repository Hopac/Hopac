Notes on the Internal Implementation of Hopac
=============================================

At the moment, this document contains some quickly written random notes on the
internal implementation details of Hopac.  It will probably help if the reader
is already familiar with many of the basic ideas and issues in the
implementation of something like Hopac.  Many of the code snippets in here are
illustrative and might not be valid F# code.  The actual definitions may be
written in C# and contain implementation details not discussed here.  Also note
that these are internal implementation details and subject to change.

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
executed, by calling its **DoWork** method it is passed a reference to the
**Worker** struct of the worker thread that calls it.  The main purpose of this
convention is to make it possible to access the work item stack of the worker
thread as efficiently as possible.  By pushing work items to the work item
stack of a worker thread, a work item can effectively spawn new lightweight
threads.  The **Work** class also contains a **Next** field (mainly) for this
purpose as the work item stack is implemented as a linked list.

The worker thread is responsible for catching any unhandled exceptions raised
by the execution of a work item and passing them to the current exception
handler.  This way individual work items do not generally need to implement
try-catch blocks (and there is a little less overhead to executing a work
item).  The current handler is referenced by the **Handler** field of the
**Worker** struct and is updated by both the worker thread loop and individual
work items that specifically want to install a new handler.

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

This is one of many tricks used in Hopac to reduce allocations and copying of
data and improve performance.  So, when a **DoJob** method is passed a **Cont**
object, it actually receives all three of those "properties".  In case of
success, the job can then immediately call **DoCont**, or, in case of failure,
it can immediately call **DoHandle**, or, in case the job needs to block, it
can efficiently add the continuation, as a **Work** item (remember the **Next**
field), into a relevant queue.

In order to make the work item aspect really useful, the **Cont** class also
contains the **Value** field.  The **Value** field is there so that when a
blocked job is finally resumed, the value that the continuation is expecting
can be written to the **Value** field, and the continuation object can be
pushed to a work item stack.  And all of this can be done without having to
allocate a new **Work** object.  (On the other hand, this means that
continuations are single threaded - one must be careful not to queue a single
continuation object into multiple queues at the same time.)

Let's make some of this more concrete by looking at the implementations of the
Job monad operations, **result** and **&gt;&gt;=** (or bind).  The **result**
operation is quite straightforward:

```fsharp
let result (x: 'x) =
  {new Job<'x> () with
    override self.DoJob (wr, xK) =
     xK.DoCont (&wr, x)}
```

**result x** is a job that simply calls the **DoCont** method of the
continuation **xK** with value **x** and the worker reference **wr** it
was given.

The **&gt;&gt;=** (or bind) operation, is considerably more complicated:

```fsharp
let (>>=) (aJ: Job<'a>) (a2bJ: 'a -> Job<'b>) : Job<'b> =
  {new Job<_>() with
    override self.DoJob (wr, bK) =
      aJ.DoJob (&wr, {new Cont<_>() with
      override self.DoHandle (wr, e) = bK.DoHandle (&wr, e)
      override self.DoWork (wr) = (a2bJ self.Value).DoJob (&wr, bK)
      override self.DoCont (wr, a) = (a2bJ a).DoJob (&wr, bK)})}
```

**aJ &gt;&gt;= a2bJ** is a job that first executes the **aJ** job.  In order
to do that, it needs to allocate a new continuation object, which it passes to
the **DoJob** method of **aJ**.  That continuation object needs to properly
implement all that the **Cont** class represents.  In particular,

* the **DoHandle** method implements the failure (or exception) continuation,
* the **DoCont** method implements the success continuation, and, interestingly,
* the **DoWork** method *also* implements the success continuation for the case
  that the job was suspended, and in that case, the **Value** field of the
  continuation object contains the result of the job.

Take a look at the **DoCont** method above.  It simply calls **a2bJ** with
**a**, which is the result of the **aJ** job and then the **DoJob** method of
the resulting object.  There is no need to wrap the call with an exception
handling block.  If the expression **a2bJ a** throws an exception, it will be
handled by the worker thread and the exception will be passed to the current
handler.

Take a look at the **DoHandle** method above.  It simply forwards the exception
**e** to the handler of **bK**.  This is another trick that basically makes it
zero cost, **O(0)**, to pass around the exception handler with the tradeoff that
every success continuation must also implement such a forwarding failure
continuation and that, due to the forwarding, calling the exception handler is
then an **O(n)** operation, where **n** is the size of the success continuation
(or stack of frames in a more traditional implementation).
