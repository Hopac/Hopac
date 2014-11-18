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
    let before = GC.GetTotalMemory true
    let result = Array.zeroCreate n
    let mutable stream = sieve
    let last = n-1
    for i=0 to last do
      result.[i] <- stream.Value
      stream <- force stream.Next
      if i = last then
        printf "%5d b/p " (max 0L (GC.GetTotalMemory true - before) / int64 n)
    result

  let run n =
    printf "Sequential:    "
    let timer = Stopwatch.StartNew ()
    let ps = primes n
    let d = timer.Elapsed
    printf "%7d - %fs\n" ps.[ps.Length-1] d.TotalSeconds
    d

/////////////////////////////////////////////////////////////////////////

module HopacJob =
  type [<NoComparison>] Stream<'a> = {Value: 'a; Next: Job<Stream<'a>>}

  let rec iterate (step: 'a -> 'a) (init: 'a) : Job<Stream<_>> = Job.thunk <| fun () ->
    {Value = init;
     Next = iterate step (step init)}

  let rec filter (pred: 'a -> bool) (xs: Stream<'a>) : Job<Stream<'a>> =
    let next = xs.Next >>= filter pred
    if pred xs.Value then
      Job.result {Value = xs.Value; Next = next}
    else
      next

  let sieve : Job<Stream<_>> =
    let rec sieve nats =
      let prime = nats.Value
      {Value = prime;
       Next = nats.Next >>= filter (fun x -> x % prime <> 0) |>> sieve}
    iterate (fun x -> x+1) 2 |>> sieve

  let primes n = Job.delay <| fun () ->
    let before = GC.GetTotalMemory true
    let result = Array.zeroCreate n
    let rec loop i primes =
      if i < n then
        result.[i] <- primes.Value
        primes.Next >>= loop (i+1)
      else
        printf "%5d b/p " (max 0L (GC.GetTotalMemory true - before) / int64 n)
        Job.result result
    sieve >>= loop 0

  let run n =
    printf "JobStream:     "
    let timer = Stopwatch.StartNew ()
    let ps = run (primes n)
    let d = timer.Elapsed
    printf "%7d - %fs\n" ps.[ps.Length-1] d.TotalSeconds
    d

/////////////////////////////////////////////////////////////////////////

module HopacPromise =
  type [<NoComparison>] Stream<'a> = {Value: 'a; Next: Promise<Stream<'a>>}

  let rec iterate (step: 'a -> 'a) (init: 'a) : Job<Stream<_>> = Job.thunk <| fun () ->
    {Value = init;
     Next = Promise.Now.delay (iterate step (step init))}

  let rec filter (pred: 'a -> bool) (xs: Stream<'a>) : Job<Stream<'a>> =
    let next = xs.Next >>= filter pred
    if pred xs.Value then
      Job.result {Value = xs.Value; Next = Promise.Now.delay next}
    else
      next

  let sieve : Job<Stream<_>> =
    let rec sieve nats =
      let prime = nats.Value
      {Value = prime;
       Next =
        Promise.Now.delay (nats.Next >>= filter (fun x -> x % prime <> 0) |>> sieve)}
    iterate (fun x -> x+1) 2 |>> sieve

  let primes n = Job.delay <| fun () ->
    let before = GC.GetTotalMemory true
    let result = Array.zeroCreate n
    let rec loop i primes =
      if i < n then
        result.[i] <- primes.Value
        primes.Next >>= loop (i+1)
      else
        printf "%5d b/p " (max 0L (GC.GetTotalMemory true - before) / int64 n)
        Job.result result
    sieve >>= loop 0

  let run n =
    printf "PromiseStream: "
    let timer = Stopwatch.StartNew ()
    let ps = run (primes n)
    let d = timer.Elapsed
    printf "%7d - %fs\n" ps.[ps.Length-1] d.TotalSeconds
    d

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

  let primes n = Job.delay <| fun () ->
    let before = GC.GetTotalMemory true
    Stream.imp sieve >>= fun primes ->
    let result = Array.zeroCreate n
    let last = n-1
    Job.forUpTo 0 last (fun i ->
      primes |>> fun p ->
      result.[i] <- p
      if i = last then
        printf "%5d b/p " (max 0L (GC.GetTotalMemory true - before) / int64 n)) >>%
    result

  let run n =
    printf "HopacCh:       "
    let timer = Stopwatch.StartNew ()
    let ps = run (primes n)
    let d = timer.Elapsed
    printf "%7d - %fs\n" ps.[ps.Length-1] d.TotalSeconds
    d
    
/////////////////////////////////////////////////////////////////////////

module AsyncMb =
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

  let sieve () : Stream<int * Stream<int>> =
    MailboxProcessor.Start <| fun self -> async {
      let rec sieve natsIn = async {
        let! prime = take natsIn
        let! out = self.Receive ()
        do out.Reply (prime, natsIn)
        return! sieve (filter (fun n -> n % prime <> 0) natsIn)
      }
      return! sieve (iterate 2 (fun i -> i+1))
    }

  let primes n = async {
    let before = GC.GetTotalMemory true
    use sieve = sieve ()
    let result = Array.zeroCreate n
    let last = n-1
    for i=0 to last do
      let! p = take sieve
      result.[i] <- p
      if i = last then
        printf "%5d b/p " (max 0L (GC.GetTotalMemory true - before) / int64 n)
    return result
  }

  let run n =
    printf "Async:         "
    use cancelSource = new System.Threading.CancellationTokenSource ()
    let timer = Stopwatch.StartNew ()
    let ps = Async.RunSynchronously (primes n, -1, cancelSource.Token)
    cancelSource.Cancel ()
    for (_, s) in ps do
      (s :> IDisposable).Dispose ()
    let d = timer.Elapsed
    printf "%7d - %fs\n" (fst ps.[ps.Length-1]) d.TotalSeconds
    d

/////////////////////////////////////////////////////////////////////////

module Async =
  type [<NoComparison>] Stream<'a> = {Value: 'a; Next: Async<Stream<'a>>}

  let inline result x = async.Return x
  let inline (>>=) x f = async.Bind (x, f)
  let inline (|>>) x f = async.Bind (x, f >> result)

  let rec iterate (step: 'a -> 'a) (init: 'a) : Async<Stream<_>> = async {
    return {Value = init;
            Next = iterate step (step init)}
  }

  let rec filter (pred: 'a -> bool) (xs: Stream<'a>) : Async<Stream<'a>> =
    let next = xs.Next >>= filter pred
    if pred xs.Value then
      result {Value = xs.Value; Next = next}
    else
      next

  let sieve : Async<Stream<_>> =
    let rec sieve nats =
      let prime = nats.Value
      {Value = prime;
       Next = nats.Next >>= filter (fun x -> x % prime <> 0) |>> sieve}
    iterate (fun x -> x+1) 2 |>> sieve

  let primes n = async {
    let before = GC.GetTotalMemory true
    let results = Array.zeroCreate n
    let rec loop i primes =
      if i < n then
        results.[i] <- primes.Value
        primes.Next >>= loop (i+1)
      else
        printf "%5d b/p " (max 0L (GC.GetTotalMemory true - before) / int64 n)
        result results
    return! sieve >>= loop 0
  }

  let run n =
    printf "AsyncStream:   "
    let timer = Stopwatch.StartNew ()
    let ps = Async.RunSynchronously (primes n)
    let d = timer.Elapsed
    printf "%7d - %fs\n" ps.[ps.Length-1] d.TotalSeconds
    d

/////////////////////////////////////////////////////////////////////////

let inline cleanup (d: TimeSpan) =
  let stop = DateTime.UtcNow + d + d + TimeSpan.FromSeconds 2.0
  while DateTime.UtcNow <= stop do
    GC.Collect ()
    GC.WaitForPendingFinalizers ()
    Threading.Thread.Sleep 250

do let ns = [10; 100; 1000; 2500; 5000; 7500; 10000]
   for n in ns do
     Sequential.run n |> cleanup
   for n in ns do
     HopacCh.run n |> cleanup
   for n in ns do
     HopacJob.run n |> cleanup
   for n in ns do
     Async.run n |> cleanup
   for n in ns do
     HopacPromise.run n |> cleanup
   for n in ns do
     AsyncMb.run n |> cleanup
