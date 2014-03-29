This benchmark is inspired by the blog post
[50 million messages per second - on a single machine](http://letitcrash.com/post/20397701710/50-million-messages-per-second-on-a-single-machine).
There is really nothing novel in a ping-pong benchmark.  You can find ping-pong
benchmarks written for pretty much every message passing system.  On my laptop
this benchmark peaks at about 56 million messages per second, so I guess we're
competitive with Akka.  Of course, my laptop is only a 2-core / 4-thread
i5-3337U running at 1.8GHz.

Let's analyze the setup of the original benchmark.  At the time of writing this,
I've never programmed in Scala or used Akka, so I'm making some assumptions here
on the semantics of Scala and Akka based solely on my programming experience.

First the benchmark defines two messages:

```scala
case object Run
case object Msg
```

I would assume these correspond to nullary constructor definitions, so that
Scala will only allocate any potential objects related to those definitions
once.  This means that when sending those case objects as messages, no memory
allocation is needed for the message itself.

The destination actor is very simple:

```scala
class Destination extends Actor {
  def receive = {
    case Msg => sender ! Msg
  }
}
```

Upon receiving a **Msg**, it simply sends one back to the **sender**.  Here it
seems that the message passing system in Akka implicitly passes a reference to
the actor that sent the message.  In Hopac there is no semantic that would
directly correspond to this feature, because channels and jobs are separate
concepts.  In the benchmark implementation we'll simulate this semantic by
passing a reply channel as the message contents.

There is a bit more logic in the client actor:

```scala
def receive = {
  case Msg =>
    received += 1
    if (sent < repeat) {
      actor ! Msg
      sent += 1
    } else if (received >= repeat) {
      latch.countDown()
    }
  case Run =>
    for (i <- 0L until initalMessages) {
      actor ! Msg
      sent += 1
    }
}
```

The client first waits for the **Run** message to start.  It then sends a number
of messages to the destination and then starts receiving replies.  This sending
of a batch of messages to start the process basically seems like a manual
optimization to reduce otherwise significant synchronization costs.  There is no
need for this in Hopac.  Also, it is more natural to use ordinary control flow
in Hopac, so there is no need to arrange it so that the client would wait for a
**Run** message.  Because simulating either aspect shouldn't really affect
performance with Hopac, we will not simulate these aspects.  This concludes the
analysis.

The benchmark program actually implements several variants that use different
combinations of operations to pass the messages.  In Hopac, one can either send
a message to a channel asynchronously or give a message on a channel
synchronously.  When a message is given synchronously, the giver is suspended if
the other party of the communication is not present.  In this benchmark both
parties of the communication immediately start waiting for a reply message, so
using fully synchronous messaging is not necessary to avoid buffering issues.
Also implemented in this program is a version using an F# async
MailboxProcessor.
