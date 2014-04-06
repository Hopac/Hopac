This benchmark is inspired by the sequential benchmark described in the book
Erlang Programming by Cesarini and Thompson in chapter 4.  This is a variation
of a process ring benchmark where the previous process spawns the next process
in the ring.  As mentioned already, this is essentially a sequential benchmark
and should expose the costs of spawning processes and the sending and receiving
of messages.  Also relevant is the number of heavyweight threads being kept
active while processing this benchmark.
