This is a simple benchmark that exposes costs associated with awaiting for
tasks.  A trivial task is started (queued to the .Net thread pool) from within
a job and then the result of that task is awaited for.  Some of the relevant
issues here are:
* How much does it cost to suspend a job.
* How much does it cost to resume the job once the result of the task is ready.
* How much does it cost to suspend and resume internal worker threads used
  to run the job(s).
* How many worker threads are kept active while running particular number of
  jobs in parallel.
