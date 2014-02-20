Hopac - Higher-Order, Parallel, Asynchronous and Concurrent
===========================================================

Hopac is a library for F# with the aim of making it easier to write efficient
parallel, asynchronous and concurrent programs.  The design of Hopac draws
inspiration from Concurrent ML.  Similar to Concurrent ML, Hopac provides
message passing primitives and supports the construction of first-class
synchronous abstractions.  Parallel jobs (light-weight threads) in Hopac are
created using techniques similar to the F# Async framework.  Hopac runs
parallel jobs using a work distributing scheduler in a non-preemptive fashion.
