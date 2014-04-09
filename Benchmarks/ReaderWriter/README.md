This benchmark is inspired by the blog post
[ha4: Making Async 5x Faster](http://t0yv0.blogspot.com/2011/12/making-async-5x-faster.html).
The benchmark basically creates a pair of jobs.  One job acts as a writer (or
producer) and another acts as a reader (or consumer) of messages on a channel
shared by the jobs.  Two versions of the basic setup are actually implemented
within the program.  The first version is basically a literal translation of the
original benchmark program described in the blog post.  A literal translation is
possible, because the blog post actually motivates and describes an
implementation of synchronous channels with Concurrent ML like semantics in F#.
The other version implements the exact same algorithm, but is tweaked a bit to
avoid some (in this case) unnecessary operations.
