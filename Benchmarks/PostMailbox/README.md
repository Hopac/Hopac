This benchmark is inspired by the blog post
[F# – How many messages can you post to a F# agent in one second?](http://theburningmonk.com/2012/03/f-how-many-messages-can-you-post-to-a-f-agent-in-one-second/).
The benchmark in itself is very simple.  It just creates an "agent" that
receives a stream of integer messages from 1 to max and then signals when it
receives the max message.  Several versions of the benchmark are implemented
using various messaging primitives in this benchmark program.  For each run of
the benchmark two numbers are reported.  The first one tells how many messages
per second one could send to the agent and the second one tells how many
messages per second the agent could actually receive.

There are a couple of important things to learn from this benchmark regarding
the effects of buffering, latency and asynchrony.  (Do monitor the process
memory usage using ProcessExplorer, for example.)  As reported in the original
blog post, the F# MailboxProcessor agent can be sent messages quickly, but it
takes much more time for the agent to process those messages.  What happens is
that the MailboxProcessor queues the sent messages into an internal buffer.  In
a benchmark like this, that buffer can grow very large, and, as pointed out in
the blog post, that could even result in an OutOfMemoryException.

The same buffering effect can be observed with the asynchronous Hopac
primitives, called ChSend and MbSend in the benchmark.  The MbSend version uses
Hopac's Mailbox primitive, which is optimized for buffering and can achieve
similar queuing performance to MailboxProcessor.  The ChSend version uses
Hopac's Ch primitive, which is optimized for non-buffered synchronous message
passing and unsurprisingly, due to memory pressure, does not perform as well as
MbSend.  On the other hand, the ChGive version uses synchronous message passing
and does not perform any buffering.

I would suspect that there is a significant latency in between the time of
sending an idle MailboxProcessor agent a message and the time at which the agent
wakes up and starts processing those messages.  In this benchmark setting, it
would seem to help the MailboxProcessor agent to consistently get high numbers
of messages sent per second, because the sender can proceed during the latency
period and build up a queue of messages.

Hopac's Mailbox primitive is not quite so forgiving in this benchmark setting.
I would suspect that it may sometimes wake up the agent effectively immediately.
Peak send performance in this benchmark from Hopac's Mailbox seems slightly
better than with MailboxProcessor.  However, it would seem that occasionally the
agent wakes up immediately and then the sender and receiver proceed in almost
lock step fashion and peak performance is not reached.  On the other hand,
Hopac's synchronous Ch performs consistently, because the sender and agent must
proceed synchronously.
