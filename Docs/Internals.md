Notes on the Internal Implementation of Hopac
=============================================

At the moment, this document contains some quickly written random notes on the
internal implementation details of Hopac.  It will probably help if the reader
is already familiar with many of the basic ideas and issues in the
implementation of something like Hopac.

The **Work** class represents a work item.  This is comparable to what the .Net
thread pool calls user work items, but also includes a mechanism, by inheriting
from the **Handler** class, for exception handling.

Work items are executed by worker threads.  A worker thread is represented by
the **Worker** struct.  When a work item is executed, by calling its **DoWork**
method it is passed a reference to the **Worker** struct of the worker thread
that calls it.  The main purpose of this convention is to make it possible to
access the **WorkStack** of the worker thread as efficiently as possible.  By
pushing work items to the **WorkStack** of a worker thread, a work item can
effectively spawn new lightweight threads.  The **Work** class also contains a
**Next** field partly for this purpose.

The worker thread is responsible for catching any unhandled exceptions raised
by the execution of a work item and passing them to the current **Handler**
(see the **Worker** struct).  This way individual work items do not generally
need to implement try-catch blocks.

While the **Work** class has support for exception handling, it does not have
support for other kind of control flow or continuations.  That is the domain of
the **Job** and **Cont** classes.

The **Job** class represents an operation that may block.  It contains a single
method **DoJob** that is passed both a reference to the current worker, and a
**Cont** object, whose **DoCont** method the job (may or may not) ultimately
arrange to be called with the result computed by the job.

The **Cont** class represents a continuation.  It also inherits the **Work**
class, and via **Work**, the **Handler** class.  So, if we ignore the selective
communication mechanism for now, then, in fact, **Cont** represents:

* a success continuation (**DoCont**),
* a failure continuation (**DoHandle**), and
* a work item (**DoWork**).

This is one of many tricks used in Hopac to reduce allocations and copying of
data and improve performance.  So, when the **DoJob** method is passed a
**Cont** object, it actually receives all three of those "properties".  In case
of success, the job can then immediately call **DoCont**, or, in case of
failure, it can immediately call **DoHandle**, or, in case the job needs to
block, it can efficiently add the continuation, as a **Work** item (remember
the **Next** field), into a relevant queue.
