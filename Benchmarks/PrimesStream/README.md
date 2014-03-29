This benchmark implements a traditional example of programming with (lazy)
streams: the Sieve of Eratosthenes.  This is also one of the first examples
presented in the book
[Concurrent Programming in ML](http://www.cambridge.org/us/academic/subjects/computer-science/distributed-networked-and-mobile-computing/concurrent-programming-ml).
One should understand that the algorithm actually isn't the Sieve of
Eratosthenes as explained in the paper
[The Genuine Sieve of Eratosthenes](http://www.cs.hmc.edu/~oneill/papers/Sieve-JFP.pdf).
While, for the purpose of computing primes, this algorithm is quite inefficient,
this is still quite effective for the purpose of benchmarking the costs of jobs
and message passing.

There are actually multiple implementations of the sieve in this benchmark
program.  A sequential version, a version using Hopac channels, a version using
Hopac promises and a version using Async MailboxProcessors.  Note that the
version using Hopac promises actually does not start jobs in parallel.
