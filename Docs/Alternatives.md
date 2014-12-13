## Cancellation with Negative Acknowledgments

The F# `async` mechanism supports *cancellation* in the form of having a
cancellation token carried along and checked by the async monad.  Hopac jobs do
not implicitly carry or check any form of cancellation token.  On the other
hand, the
[synchronous channels](http://hopac.github.io/Hopac/Hopac.html#def:type%20Hopac.Ch)
of Hopac support *rendezvous* and the alternative mechanism provides *negative
acknowledgments* via the
`withNack`[*](http://hopac.github.io/Hopac/Hopac.html#def:val%20Hopac.Alt.withNack)
combinator.  The idea is that alternatives represent *selective synchronous
operations*.  When synchronizing on a choice of multiple alternatives, only one
alternative will be *committed to*.  Relying on *idempotency*, *rendezvous*, or
*negative acknowledgments* alternatives can be designed and implemented so that
they do not have an effect unless they are committed to.

The
[MSDN documentation on asynchronous workflows](http://msdn.microsoft.com/en-us/library/dd233250.aspx)
contains an example that downloads a number of web pages in parallel.  Let's
wrap the `fetchAsync` function from the example as a selective synchronous
operation.  First here is a slightly modified version of `fetchAsync`:

```fsharp
open Microsoft.FSharp.Control.WebExtensions
open System.Net

let fetchAsync (name, url:string) = async { 
  let uri = new System.Uri(url)
  let webClient = new WebClient()
  let! html = webClient.AsyncDownloadString(uri)
  return sprintf "Read %d characters for %s" html.Length name
}
```

The difference is that the above version lets exceptions fall through.

Let's then write a generic helper function for wrapping `async` operations as
selective synchronous operations:

```fsharp
open System.Threading

let asyncAsAlt (xA: Async<'x>) : Alt<'x> = Alt.withNack <| fun nack ->
  let rI = ivar ()
  let tokenSource = new CancellationTokenSource ()
  let dispose () =
    tokenSource.Dispose ()
    // printfn "Dispose"
  let op = async {
      try
        let! x = xA
        do rI <-= x |> start
        // do printfn "Success"
      with e ->
        do rI <-=! e |> start
        // do printfn "Failure"
    }
  Async.Start (op, cancellationToken = tokenSource.Token)
  nack
  |>> fun () ->
        tokenSource.Cancel ()
        // printfn "Cancel"
        dispose ()
  |> Job.start >>%
  Alt.tryFinallyFun rI dispose
```

Let's then use it to wrap `fetchAsync`:

```fsharp
let fetchAlt (name, url) : Alt<string> =
  fetchAsync (name, url) |> asyncAsAlt
```

Here is the list of URLs used in the MSDN example:

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

we can also use
`Alt.select`[*](http://hopac.github.io/Hopac/Hopac.html#def:val%20Hopac.Alt.select),
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
the above `asyncAsAlt` implementation by turning the `printfn` calls in the
comments to code.  When you rerun the example, you can then observe what happens
inside the abstraction.  Don't forget to recompile all the code!

Of course, we can also select from other kinds of alternatives.  For example,
operations created with `fetchAlt` also work with timeouts:

```fsharp
let runWithTimeout seconds =
  Alt.select [
    fetchAlt ("MSDN", "http://msdn.microsoft.com/") |>>? fun s ->
      printfn "%s before timeout." s
    Timer.Global.timeOut (TimeSpan.FromSeconds seconds) |>>? fun () ->
      printfn "timeout!" ]
  |> run
```

Note that the above `run...` functions are only intended as examples.  The idea
with alternatives is that users can freely combine them as selective synchronous
operations.  Wrapping an alternative as a function defeats that purpose.
