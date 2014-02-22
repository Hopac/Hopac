This is a benchmark inspired by the paper [Chameneos, a Concurrency Game for
Java, Ada and Others](http://cedric.cnam.fr/PUBLIS/RC474.pdf).  Actually this
benchmark is inspired by a benchmark, inspired by the paper, on an infamous
language shootout site.  (The implementations here do not meet the requirements
of the shootout, because a custom cooperative scheduler is used.)

This is an interesting benchmark in a number of ways.  As described in the
paper the benchmark concerns "symmetric cooperation".  This is something that
is supported by the synchronous channels of Concurrent ML, and Hopac, and the
core of the Chameneos benchmark can be expressed using a kind of "swap channel"
abstraction.

On the other hand, because the number of threads used in Chameneos is low, it
is easy to make it very fast via straightforward use of native threads and
minimal scope locks or interlocked primitives.  (It is quite surprising that
there is no such C# or F# implementation on the shootout site.  It should be
easy to get very close to C++ performance.)

The program here actually contains multiple implementations utilizing different
mechanisms provided by Hopac:

* One implementation is written using locks.
* Another is written using synchronous variables.
* And another is written using a kind of swap channels.

What is important to realize is that, while these solutions have obviously
different performance characteristics, they also have different modularity and
symmetry characteristics.  The version written using swap channels is arguably
both modular and symmetric.
