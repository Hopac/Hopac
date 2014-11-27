## Cancellation with Negative Acknowledgments

The F# `async` mechanism supports *cancellation* in the form of having a
cancellation token carried along and checked by the async monad.  Hopac jobs do
not implicitly carry or check any form of cancellation token.  On the other
hand, the synchronous channels[]() of Hopac support *rendezvous* and the
alternative mechanism provides *negative acknowledgments* via the `withNack`
combinator.  The idea is that alternatives represent *selective synchronous
operations*.  When synchronizing on a choice of multiple alternatives, only one
alternative will be committed to.  Relying on idempotency, rendezvous, or
negative acknowledgments alternatives can be designed and implemented so that
they do not have an effect unless they are committed to.

The
[MSDN documentation on asynchronous workflows](http://msdn.microsoft.com/en-us/library/dd233250.aspx)
contains an example that downloads a number of web pages in parallel.  Let's
wrap the `fetchAsync` function from the example as a selective synchronous
operation:

```fsharp
open Microsoft.FSharp.Control.WebExtensions
open System.Net
open System.Threading

let fetchAlt (name, url: string) : Alt<string> = Alt.withNack <| fun nack ->
  let rI = ivar ()
  let tokenSource = new CancellationTokenSource ()
  Async.Start
    (async {
       try
         let uri = new System.Uri (url)
         let webClient = new WebClient ()
         let! html = webClient.AsyncDownloadString uri
         do rI <-= sprintf "Read %d characters for %s" html.Length name |> start
       with ex ->
         do rI <-=! ex |> start
     },
     cancellationToken = tokenSource.Token)
  Job.start (nack |>> fun () -> tokenSource.Cancel ()) >>%
  upcast rI
```

And here is the list of URLs used in the MSDN example:

```fsharp
let urlList = [ "Microsoft.com", "http://www.microsoft.com/" 
                "MSDN", "http://msdn.microsoft.com/" 
                "Bing", "http://www.bing.com" ]
```

Now, in addition to being able to perform `fetchAlt` operations in parallel

```fsharp
let runAll () =
  urlList
  |> Seq.map (fetchAlt >> asJob)
  |> Job.conCollect
  |> run
```

We can also use
`Alt.select`[*](http://vesakarvonen.github.io/Hopac/Hopac.html#def:val%20Hopac.Alt.select),
for example, to start multiple fetches in parallel and then cancel the rest when
the fastest one is completed:

```fsharp
let runFastest () =
  urlList
  |> Seq.map fetchAlt
  |> Alt.select
  |> run
```

If you have trouble understanding what is going on, I recommend that you modify
the above `fetchAlt` implementation with some `printfn` calls to reveal what
happens.  In particular, add a `printfn` after the `Cancel ()` call and after
the `rI <-= sprintf ...` expression.
