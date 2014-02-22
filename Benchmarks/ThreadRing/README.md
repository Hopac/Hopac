This benchmark is inspired by a benchmark by the same name on an infamous
language shootout site.  The benchmark creates a ring (or multiple independent
rings) of jobs that pass a message around the ring.  The Hopac based
implementations in here use a custom cooperative scheduler and do not pass the
requirements for the original benchmark.

As an aside, it is quite interesting that, at the time of writing, one could
argue that the F# implementation displayed on that site does not come even
close to passing the requirements.  In the comments of that program it is said
"Uses F# asyncs.  Asyncs are triggered by tailcall in sequence as progress is
made around the ring."  That comment is accurate.  The Haskell implementation
is also quite interesting in the sense that it explicitly pins all the threads
to a single core.  One could argue that it fundamentally defeats the purpose of
the benchmark, because it eliminates the costs of suspending and resuming
threads.  One could argue that the Haskell implementation effectively uses a
custom scheduler.  Hamming might ask whether the purpose of benchmarks is
insight or numbers?

This was one of the first benchmarks implemented while developing the library
and has been quite useful while optimizing the Hopac library implementation. 
The benchmark is quite sensitive to many implementation tradeoffs.  For
example, if the amount of memory taken by channels or jobs is increased
significantly, it will quickly show up in this benchmark (especially when you
increase the ring length), because the benchmark will run out of some cache
level.

This is also an interesting benchmark in a number of ways.  A naive
implementation using native threads is unlikely to perform well - especially
when the length of the ring is increased substantially.  The benchmark will
also very effectively expose overheads of basic operations (on message and
job queues).
