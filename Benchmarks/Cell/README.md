This is a custom benchmark inspired by the first example of Concurrent ML
programming in [John Reppy](http://people.cs.uchicago.edu/~jhr/)'s book
[Concurrent Programming in
ML](http://www.cambridge.org/us/academic/subjects/computer-science/distributed-networked-and-mobile-computing/concurrent-programming-ml).
As a first example of Concurrent ML programming the book shows how to build
updatable storage cells (like ref cells) from threads and channels.  This
benchmark does the same using Hopac jobs and channels.  Also implemented is a
version with the F#
[MailboxProcessor](http://msdn.microsoft.com/en-us/library/ee370357.aspx) and
the Async framework.  Large numbers of lightweigth threads are being created and
messages are being processed.  Memory usage and execution time is reported.
