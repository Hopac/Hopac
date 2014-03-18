// Copyright (C) by Housemarque, Inc.

module PrimesStream

open System
open System.Diagnostics
open Hopac
open Hopac.Extra
open Hopac.Alt.Infixes
open Hopac.Job.Infixes

/////////////////////////////////////////////////////////////////////////

module Sequential =
  // Hmm... Memoization seems to cause stack overflows.
  type Delay<'a> = Unit -> 'a

  let inline delay th = th

  let inline force (d: Delay<'a>) : 'a = d ()

  type Stream<'a> = {Value: 'a; Next: Delay<Stream<'a>>}

  let rec iterate (init: 'a) (step: 'a -> 'a) : Stream<'a> =
    {Value = init
     Next = delay <| fun () -> iterate (step init) step}

  let rec filter (pred: 'a -> bool) (xs: Stream<'a>) : Stream<'a> =
    let next = xs.Next
    if pred xs.Value then
      {Value = xs.Value
       Next = delay <| fun () -> filter pred (force next)}
    else
      filter pred (force next)

  let sieve =
    let rec sieve nats =
      let prime = nats.Value
      let next = nats.Next
      {Value = prime
       Next = delay <| fun () -> sieve (filter (fun x -> x % prime <> 0) (force next))}
    sieve (iterate 2 (fun x -> x+1))

  let primes n =
    let result = Array.zeroCreate n
    let mutable stream = sieve
    for i=0 to n-1 do
      result.[i] <- stream.Value
      stream <- force stream.Next
    result

  let run n =
    printf "Sequential:   "
    let timer = Stopwatch.StartNew ()
    let ps = primes n
    let d = timer.Elapsed
    printf "%7d - %fs\n" ps.[ps.Length-1] d.TotalSeconds

/////////////////////////////////////////////////////////////////////////

module HopacCh =
  let sieve (primesOut: Stream.Out<_>) =
    let rec sieve natsIn =
      natsIn >>= fun prime ->
      primesOut prime >>= fun () ->
      Stream.imp
       (Stream.filterFun
         (fun x -> x % prime <> 0)
         natsIn) >>= sieve
    Job.server (Stream.imp (Stream.iterateFun 2 (fun x -> x+1)) >>= sieve)

  let primes n =
    Stream.imp sieve >>= fun primes ->
    let result = Array.zeroCreate n
    Job.forUpTo 0 (n-1) (fun i ->
      primes |>> fun p ->
      result.[i] <- p) >>%
    result

  let run n =
    printf "HopacCh:      "
    let timer = Stopwatch.StartNew ()
    let ps = run (primes n)
    let d = timer.Elapsed
    printf "%7d - %fs\n" ps.[ps.Length-1] d.TotalSeconds
    
/////////////////////////////////////////////////////////////////////////

module HopacPromise =
  type [<NoComparison>] Stream<'a> = {Value: 'a; Next: Promise<Stream<'a>>}

  let rec iterate (step: 'a -> 'a) (init: 'a) : Job<Stream<_>> =
    Job.result
     {Value = init;
      Next = Promise.Now.delay (Job.delay <| fun () -> iterate step (step init))}

  let rec filter (pred: 'a -> bool) (xs: Stream<'a>) : Job<Stream<'a>> =
    let next = Promise.read xs.Next >>= filter pred
    if pred xs.Value then
      Job.result {Value = xs.Value; Next = Promise.Now.delay next}
    else
      next

  let sieve : Job<Stream<_>> =
    let rec sieve nats =
      let prime = nats.Value
      Job.result
       {Value = prime;
        Next =
         Promise.Now.delay (Promise.read nats.Next >>= filter (fun x -> x % prime <> 0) >>= sieve)}
    iterate (fun x -> x+1) 2 >>= sieve
    
  let primes n = Job.delay <| fun () ->
    let result = Array.zeroCreate n
    let rec loop i primes =
      if i < n then
        result.[i] <- primes.Value
        Promise.read primes.Next >>= loop (i+1)
      else
        Job.result result
    sieve >>= loop 0

  let run n =
    printf "HopacPromise: "
    let timer = Stopwatch.StartNew ()
    let ps = run (primes n)
    let d = timer.Elapsed
    printf "%7d - %fs\n" ps.[ps.Length-1] d.TotalSeconds
    
/////////////////////////////////////////////////////////////////////////

module Async =
  type Stream<'a> = MailboxProcessor<AsyncReplyChannel<'a>>
  let inline take (s: Stream<'a>) = s.PostAndAsyncReply (fun x -> x)

  let iterate (init: 'a) (step: 'a -> 'a) : Stream<'a> =
    MailboxProcessor.Start <| fun self -> async {
      let state = ref init
      while true do
        let! out = self.Receive ()
        do out.Reply (!state)
        do state := step (!state)
    }

  let filter (pred: 'a -> bool) (stream: Stream<'a>) : Stream<'a> =
    MailboxProcessor.Start <| fun self -> async {
      while true do
        let! x = take stream
        if pred x then
          let! out = self.Receive ()
          do out.Reply x
    }

  let sieve () : Stream<int> =
    MailboxProcessor.Start <| fun self -> async {
      let rec sieve natsIn = async {
        let! prime = take natsIn
        let! out = self.Receive ()
        do out.Reply prime
        return! sieve (filter (fun n -> n % prime <> 0) natsIn)
      }
      return! sieve (iterate 2 (fun i -> i+1))
    }

  let primes n = async {
    let sieve = sieve ()
    let result = Array.zeroCreate n
    for i=0 to (n-1) do
      let! p = take sieve
      result.[i] <- p
    return result
  }

  let run n =
    printf "Async:        "
    use cancelSource = new System.Threading.CancellationTokenSource ()
    let timer = Stopwatch.StartNew ()
    let ps = Async.RunSynchronously (primes n, -1, cancelSource.Token)
    let d = timer.Elapsed
    cancelSource.Cancel ()
    printf "%7d - %fs\n" ps.[ps.Length-1] d.TotalSeconds

/////////////////////////////////////////////////////////////////////////

let inline cleanup () =
  for i=1 to 5 do
    Runtime.GCSettings.LargeObjectHeapCompactionMode <- Runtime.GCLargeObjectHeapCompactionMode.CompactOnce
    GC.Collect ()
    Threading.Thread.Sleep 50

do let ns = [10; 100; 1000; 10000]
   for n in ns do
     Sequential.run n ; cleanup ()
   for n in ns do
     HopacCh.run n ; cleanup ()
   for n in ns do
     HopacPromise.run n ; cleanup ()
   for n in ns do
     Async.run n ; cleanup ()
