This program contains benchmarks inspired by the benchmarks described in the
paper [A Concurrent ML Library in Concurrent
Haskell](http://www.cs.umd.edu/~avik/projects/cmllch/).  The paper describes an
implementation, or encoding, of Concurrent ML on top of (simpler) primitives
provided by Concurrent Haskell.  Comparing the performance of Hopac and the
Concurrent ML encoding in Concurrent Haskell is not valid as such, because the
libraries have different semantics.  The implementations also have quite
different goals.  If you are just interested in the numbers, then it would
appear that Hopac is one to four orders of magnitude faster.  The swap channel
benchmark, in particular, seems to run several orders of magnitude faster under
the semantics and implementation of Hopac.
