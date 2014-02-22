Here you will find several benchmark programs using the Hopac library.  See the
individual project directories for more details.

DON'T BE FOOLED!
================

The benchmarks in here are not in any way intended to be efficient solutions
to actual computational problems.  These benchmarks are intended to expose the
costs of message passing, starting jobs and synchronizing on the results of
jobs, so that one can measure, profile and ultimately reduce those costs by
optimizing the library.  In some cases, these benchmarks are intentionally
written to use some specific construct for testing purposes rather than for
efficiency.

As described in the documentation, starting a job and synchronizing on the
result of that job is cheap, but not free.  Under reasonable assumptions,
those costs are comparable to the costs of allocating, pushing and then popping
a few linked list nodes.  Most of these benchmarks do more than an order of
magnitude less processing (e.g. sum two integers) in each started job than
that.  It is therefore expected that these parallel benchmarks actually run an
order of magnitude slower than similar sequential benchmarks would.

You should also understand that many of these specific toy benchmarks could
fairly easily be programmed using native .NET threads, locks, and interlocked
primitives.  Simple logic tells that it is impossible to write a pure .Net
framework under which a benchmark implementation would run faster than a
specialized benchmark implemention written in pure .Net would.  For example, if
you know what you are doing, it is not at all difficult to manually write a
specialized program in F# that computes Fibonacci numbers using a parallelized
version of the recursive algorithm faster when using two or more cores than a
sequential function using one core.  For more complex programs than these
benchmarks, however, it quickly becomes rather difficult to write such manually
specialized code.
