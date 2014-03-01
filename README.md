Hopac - Higher-Order, Parallel, Asynchronous and Concurrent
===========================================================

Hopac is a library for F# with the aim of making it easier to write efficient
parallel, asynchronous and concurrent programs.  The design of Hopac draws
inspiration from Concurrent ML.  Similar to Concurrent ML, Hopac provides
message passing primitives and supports the construction of first-class
synchronous abstractions.  Parallel jobs (light-weight threads) in Hopac are
created using techniques similar to the F# Async framework.  Hopac runs parallel
jobs using a work distributing scheduler in a non-preemptive fashion.

Licensing
---------

Hopac is licensed under a MIT-style license.  See
[LICENSE.md](https://github.com/VesaKarvonen/Hopac/blob/master/LICENSE.md) for
details.

Status
------

The Hopac library has been in development and in internal use for some time now.
Currently it is being prepared for tagging a first release.  Documentation is
being written and the interfaces polished (e.g. naming orthogonalized and some
missing primitives being added).

Performance
-----------

One of the main reasons Hopac was developed is that existing solutions provided
by the .Net framework made it difficult to meet performance goals.  An extra
minute of delay and not being able to run an application within typical
workstation memory limits sometimes makes a big difference.  So, how fast is
Hopac?  How well does it scale to multicore machines?  How much memory do Hopac
primitives take?  In order to measure and improve the performance of Hopac,
several
[benchmark programs](https://github.com/VesaKarvonen/Hopac/tree/master/Benchmarks)
have been written and some of these programs also test similar algorithms
written using the .Net thread pool, tasks and async.  Those benchmarks are
readily available in this project and they should be very easy to compile and
run on your machine.  So, why don't you try and run those benchmark programs, or
write your own benchmark, on your multicore machine and see for yourself?

Programming in Hopac
--------------------

Hopac provides a programming model that is heavily inspired by
[John Reppy](http://people.cs.uchicago.edu/~jhr/)'s *Concurrent ML* language.
The book
[Concurrent Programming in ML](http://www.cambridge.org/us/academic/subjects/computer-science/distributed-networked-and-mobile-computing/concurrent-programming-ml)
is the most comprehensive introduction to Concurrent ML style programming.  The
*Further Reading* section at the end of this document contains links to some
articles and blog posts that discuss programming in Concurrent ML or languages
inspired by it.

Documentation on Hopac is minimal at the moment, but hopefully this will be
rectified shortly.  In particular, the document
[Programming in Hopac](https://github.com/VesaKarvonen/Hopac/blob/master/Docs/Programming.md)
will hopefully grow to a proper introduction to programming in Hopac.  In
addition, the Hopac library code contains documentation comments on individual
Hopac primitives.  The
[Hopac.fsi](https://github.com/VesaKarvonen/Hopac/blob/master/Libs/Hopac/Hopac.fsi)
signature file contains most of those documentation comments.

Rationale: Why Hopac?
---------------------

The .Net framework already provides several layers of support for parallel,
asynchronous, and concurrent (PAC) programming.  Do we really need another
library?  In here, I will try to briefly highlight some problems with some of
those existing layers.

One of the most basic ways .Net enables PAC is via threads, synchronization
primitives and concurrent data structures.  Perhaps the biggest problem with
.Net threads is that .Net threads are heavy weight.  Every thread has its own
dedicated stack and other runtime structures taking up memory.  Spawning a new
thread is expensive and thread switches are expensive.  Threads are such a
scarce resource that arbitrary modules in a large program simply cannot spawn
new threads whenever an opportunity for PAC exists.  This severely limits the
modularity of programs written using just threads.

Another problem is that writing correct, let alone modular and high performance,
PAC programs using just basic locking primitives can be rather challenging as
has been widely recognized.  This is very much a separate problem, because if
threads were extremely cheap, one could easily build higher-level synchronous
abstractions on top of such threads using only the basic synchronization
primitives.

To alleviate the costs of threads, .Net provides the thread pool.  Instead of
spawning new threads, program modules can queue user work items to be processed
using a small number of worker threads managed by the thread pool.  Queuing user
work items is several orders of magnitude cheaper than spawning new threads.

One aspect of the thread pool that is not discussed often is that the thread
pool tends to process work items in [FIFO](http://en.wikipedia.org/wiki/FIFO)
order.  Suppose you have a
[DAG](http://en.wikipedia.org/wiki/Directed_acyclic_graph), like in a parallel
build system, where the vertices of the graph represent computations and edges
represent dependencies between computations.  Simple use of the thread pool to
execute those computations leads to a
[BFS](http://en.wikipedia.org/wiki/Breadth-first_search) traversal of the graph.
During the traversal, the set or queue of ready computations, or open nodes,
when using BFS is **O(width)** and **O(depth)** when using DFS.  In many
practical situations graphs are expected to be *wide* (lots of data), but
*shallow* (not many layers of data).

Other problems with the thread pool are better recognized.  For example, one
should avoid blocking within a user work item, but at the same time, the thread
pool provides very little help to coordinate multiple work items with
dependencies.  Once a work item is queued, there is no built in notification
when it starts executing, it cannot be removed from the queue and there is no
built in notication when it has run to completion.  There is no mechanism for
the thread pool to notify the module that queued a work item when the execution
of the work item terminated with an unhandled exception.

To help with parallel and asynchonous programming, the .Net framework provides
the task parallel library and (the C# and) the F# async mechanisms.  For simple
parallel programming tasks, that do not require complex communication patterns,
the task parallel library does quite well.  The task parallel library supports
continuation tasks and the async mechanisms makes working with continuations
even easier than with just tasks.  For a particular kind of communication
pattern (many-to-one actor), the F# library provides the mailbox processor
abstraction.

On the other hand, aside from supporting task continuations and the mailbox
processor abstraction, those systems still provide very little in terms of
communicating and coordinating between tasks and one must fall back to using
locking primitives to implement those when needed.  Also, it seems that the task
and async mechanism built on top of the thread pool include some significant
implementation overheads and design weaknesses.  For example, the Task&lt;T&gt;
type of the task parallel library acts both as a representation of an operation
(comparable to a first-class function) and as a container for the result of the
operation (comparable to a write-once ref cell).  Frankly, those are two
separate responsibilities and combining them into one type is questionable
design.  The F# async design recognizes this issue at the type level, while the
C# async design does not, and as a result the C# async design suffers from
"gotchas" as has been recognized.

The thread pool, the task parallel library and the async frameworks were not
initially designed as a coherent whole.  As a result, there seem to be some
significant overheads when those systems are taken as a whole.  The Task class,
for example, has significantly more fields than a Hopac job (or continuation)
and, while tasks are very light-weight when compared to native threads, as a
result, Hopac jobs are even more light-weight than async tasks.  In Hopac, jobs
are rather intimately tied to the work distributing scheduler.  This allows
operations such as suspending and resuming jobs to be done with an order of
magnitude lower overhead than what tasks and async can do at the moment.

For the kind of programming that Hopac is designed for, some of the things that
tasks and async provide are not necessary and would increase overheads.  These
include cancellation and the preservation of synchronization contexts and Hopac
has therefore eliminated those.  (Support for synchronization contexts is
somewhat in conflict with the goals of Hopac (minimize overheads and maximize
parallel performance) and it is unlikely that such support would be added to
Hopac.  If preservation of synchronization contexts is necessary, you should
stick to tasks and/or async.  Some form of support for cancellation might be
added in the future.)

Finally, where Hopac goes much further is in providing support for message
passing primitives and the construction of first-class synchronous abstractions
for coordinating and communicating between separate parallel jobs.  The goal is
that using the high-level primitives supported by Hopac it should be possible to
program a wide range of PAC architectures with good performance without
resorting to low level locking primitives.

This rationale became much longer that I hoped for.  Hopefully you got something
out of reading it. :)

Further Reading
---------------

This section contains a list of blog posts, articles, books and other resources
that may be of interest.  The most comprehensive introduction to Concurrent ML
style programming is [John Reppy](http://people.cs.uchicago.edu/~jhr/)'s book
[Concurrent Programming in ML](http://www.cambridge.org/us/academic/subjects/computer-science/distributed-networked-and-mobile-computing/concurrent-programming-ml).

* [The Art of Multiprocessor Programming](http://people.csail.mit.edu/shanir/)
* [Async in C# and F#: Asynchronous gotchas in C#](http://tomasp.net/blog/csharp-async-gotchas.aspx/)
* [Concurrent ML as a Discrete Event Simulation Language](http://www.eecs.wsu.edu/~hauser/Publications/CMLSim.pdf)
* [Concurrent ML home page](http://cml.cs.uchicago.edu/)
* [A Concurrent ML Library in Concurrent Haskell](http://www.cs.umd.edu/~avik/projects/cmllch/)
* [Concurrent Programming in ML](http://www.cambridge.org/us/academic/subjects/computer-science/distributed-networked-and-mobile-computing/concurrent-programming-ml)
* [Composable Asynchronous Events](http://multimlton.cs.purdue.edu/mML/Publications_files/pldi11.pdf)
* [Dynamic Circular Work-Stealing Deque](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.170.1097&rep=rep1&type=pdf)
* [The Implementation of the Cilk-5 Multithreaded Language](http://supertech.csail.mit.edu/papers/cilk5.pdf)
* [Higher-Order Concurrency in Java](http://erikdemaine.org/papers/WoTUG20/)
* [Kill-Safe Synchronization Abstractions](http://www.cs.utah.edu/plt/publications/pldi04-ff.pdf)
* [Lwt and Concurrent ML](http://ambassadortothecomputers.blogspot.fi/2009/05/lwt-and-concurrent-ml.html)
* [Parallel Concurrent ML](http://manticore.cs.uchicago.edu/papers/icfp09-parallel-cml.pdf)
* [Prototyping Application Models in Concurrent ML](http://privatewww.essex.ac.uk/~fleum/typeinst.pdf)
* [Scheduling Parallel Programs by Work Stealing with Private Deques](http://chargueraud.org/research/2013/ppopp/full.pdf)
* [TaskCreationOptions.PreferFairness](http://blogs.msdn.com/b/pfxteam/archive/2009/07/07/9822857.aspx)
* [Transactional Events](http://www.cs.rit.edu/~mtf/research/tx-events/ICFP06/icfp06.pdf)
