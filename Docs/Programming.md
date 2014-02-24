Programming in Hopac
====================

Hopac provides a programming model that is heavily inspired by John Reppy's
*Concurrent ML* language.  The book [Concurrent Programming in ML](http://www.cambridge.org/us/academic/subjects/computer-science/distributed-networked-and-mobile-computing/concurrent-programming-ml)
is the most comprehensive introduction to Concurrent ML style programming.
This document contains some discussion and examples on Hopac programming
techniques.  In the future, this document might grow to a proper introduction
to Hopac.

The Hopac Programming Model
---------------------------

There are two central aspects of Hopac that shape the programming model.

The first aspect is that, threads, which are called *jobs*, in Hopac are
extremely lightweight.  On modern machines you can spawn tens of millions of
new jobs in a second.  Because a job takes only a very small amount of memory,
starting from tens of bytes, a program may have millions of jobs on a
modern machine at any moment.  Of course, at any moment, most of those jobs
are suspended, because modern machines still only have a few, or at most a
few dozen, processors cores.  When programming in Hopac, one can therefore
start new jobs in situations where it would simply be unthinkable when using
heavyweight threads.

The other aspect is that Hopac provides first-class, higher-order, selective,
synchronous, message passing primitives in the form of channels (Ch) and
alternatives (Alt) for coordinating and communicating between jobs.  That is a
mouthful!  Let's open it up a bit.

* First-class means that channels and alternatives are ordinary values.  They
  can be bound to variables, passed to and returned from functions and can even
  be sent from one job to another.
* Higher-order means that primitive alternatives can be combined and extended
  with user defined procedures to build more complex alternatives.
* Selective means that a form of choice or disjunction between alternatives is
  supported.  An alternative can be constructed that, for example, offers to
  give a message to another job or take a message from another job.  The choice
  of which operation is performed then depends on whichever alternative becomes
  available at runtime.
* Synchronous means that rather than building up a queue of messages for
  another job to examine, jobs can communicate via rendezvous.  Two jobs can
  meet so that one job can give a message to the another job.

What this all boils down to is that Hopac basically provides a kind of language
for expressing concurrent control flow.
