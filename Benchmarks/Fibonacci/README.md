This benchmark is inspired by the parallel Fibonacci function used
traditionally as a Cilk programming example.  See [The Implementation of the
Cilk-5 Multithreaded Language](http://supertech.csail.mit.edu/papers/cilk5.pdf)
for a representative example.  Basically, the naive, recursive, exponential
time Fibonacci algorithm is used.  Parallelized versions simply run recursive
calls in parallel.

The program actually implements several sequential and parallel versions of the
benchmark.  Of particular interest is that simple use of F# Asyncs seems to
perform extremely badly (in comparison to other methods) on this benchmark.
(Do take note of process memory usage when running this benchmark program.)

Like is common with benchmark programs or cute programming examples, this is
actually an extremely inefficient algorithm for computing Fibonacci numbers and
seems to be a recurring source of confusion (see the thread [Cilk Plus
Perfomance - Overhead](http://software.intel.com/en-us/forums/topic/265700),
for example).  On the other hand, because the amount of computation performed
per recursion step is very small and the number of recursive steps is very
large this is quite an effective method for measuring the overhead costs of
starting, running and retrieving the results of parallel jobs.
