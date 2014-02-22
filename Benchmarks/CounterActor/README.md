This benchmark program is inspired by the blog post [Building an “actor” in F#
with higher throughput than Akka and Erlang actors](http://zbray.com/2012/12/09/building-an-actor-in-f-with-higher-throughput-than-akka-and-erlang-actors/).

The blog post can be somewhat misleading.  As a kind of technicality, the 
blog post seems to make an assertion that locks do not scale.  That is rather
debateable depending on how you define scalable.  This is particularly so,
because the blog post later seems to imply that using a shared integer
variable that is manipulated with interlocked operations from multiple threads
would be more scalable than locks.  While using interlocked increment on a
shared variable is lock-free, the fact that the variable is shared means that
the shared variable is a sequential bottleneck.  There are, in fact, locking
algorithms (e.g. MCS locks) that are basically just as scalable (require
asymptotically same amount of memory transfers) as such a shared variable.  You
can find more details in the book [The Art of Multiprocessor
Programming](http://people.csail.mit.edu/shanir/), for example.  Similarly, it
is important to understand that lock-free and wait-free algorithms are not
necessarily faster than blocking algorithms.

Much more importantly, one issue, in particular, can be very misleading.  The
blog post, as a continuation and response to several earlier blog posts by
various authors, seems to put forth the idea that a useful measure of
scalability would be the through-put of a system where multiple threads post
messages to a single actor.  This is simply wrong, and, in fact, if you read
the blog post carefully, you can see that much is said in the blog post.  What
is not said, however, is that a many-to-one communication pattern like that is
not scalable, because the single actor in such a system is, once again, a
sequential bottleneck making it fundamentally impossible for such a system to
scale.  At best, it is a useful way of measuring system behaviour in case of
contention.  While it is useful to try to minimize performance degradation in
high-contention scenarios, contention and sequential bottle-necks are precisely
what a scalable parallel architecture must avoid.
