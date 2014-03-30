Parallel Programming Performance Considerations
===============================================

Probably the most important thing that affects the performance of parallel
algorithms on shared memory multiprocessors is the amount of *synchronization*,
whether intentional or unintentional, required between individual processors.
In a single processor system there is no need for such synchronization, but in a
multiprocessor system the processors need to synchronize their view of the
memory with the views of the other processors.  In a modern shared memory
multiprocessor this synchronization is the job of the so called cache coherence
protocol.  Most commonly this protocol is a variation of the
[MESI protocol](http://en.wikipedia.org/wiki/MESI_protocol).

Basically, when a processor reads a particular memory location, a small block of
memory corresponding to that memory location is fetched into the cache of that
processor and marked as shared.  That block is called a cache line and the size
of a cache line is typically 64 bytes, but it is processor dependent, and is
aligned on a boundary of the cache line size.  Multiple processors may hold a
cache line corresponding to a particular memory location in shared state.  When
a processor writes to a particular memory location, the corresponding cache line
is marked as modified and other processors must invalidate their copies of the
cache line.  When another processor then wants to read or write to the cache
line, the contents of the modified cache line must be transferred to that
processor.  Regardless of how that transfer takes place it typically costs a lot
more than an access to a valid cache line.

This need to synchronize the view of memory between processors is a cost that
does not exist as such with single processor systems, because there are no other
processors whose operations would invalidate cache lines held by the only
processor in the system.  Suppose you have two processors that simply read and
write the same location in memory.  Each write invalidates the cache line of the
other processor and the modified memory contents need to be continuously
transferred between the processors.  Total execution time is likely to be much
higher than when only a single processor does the same number of operations or
when two or more processors do the same operations, but do not touch the same
memory locations.

In order for a parallel algorithm to scale on a shared memory multiprocessor as
the number of processors is increased, the algorithm must avoid reading and
writing the same memory locations on each processor.  Obviously the processors
need to communicate at some points, and the fastest way to do that communication
is still by reading and writing shared memory locations, but the less such
communication is needed, the better.

Many modern runtime system, including .Net, provide a concurrent queue of some
kind that can be accessed concurrently by multiple threads and the performance
in such multiple-reader-writer scenarios is much better than when using a naive
lock around a basic queue.  Interestingly, however, it has been proven that
[queues just aren't scalable](http://concurrencyfreaks.blogspot.fi/2013_05_01_archive.html).
What this means is that the cost of operations on a queue, regardless of how it
is implemented, goes up as the number of processors accessing the queue is
increased.  Even a concurrent queue must have some amount of memory that is both
read and written by multiple processors and as discussed above, accesses to such
shared memory become expensive.  Of course, more relaxed, less sequential,
concurrent data structures may scale better than queues.
