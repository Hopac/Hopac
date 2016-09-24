// Copyright (C) by Housemarque, Inc.

module PrimesStream

#nowarn "40"

open System
open System.Diagnostics
open Hopac
open Hopac.Bench
open Hopac.Infixes

////////////////////////////////////////////////////////////////////////////////

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

////////////////////////////////////////////////////////////////////////////////

module HopacJob =
  type [<NoComparison>] Stream<'a> = {Value: 'a; Next: Job<Stream<'a>>}

  let rec iterate (step: 'a -> 'a) (init: 'a) = Job.thunk <| fun () ->
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
       Next = nats.Next >>= filter (fun x -> x % prime <> 0) >>- sieve}
    iterate (fun x -> x+1) 2 >>- sieve

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

////////////////////////////////////////////////////////////////////////////////

module HopacPromise =
  type [<NoComparison>] Stream<'a> = {Value: 'a; Next: Promise<Stream<'a>>}

  let rec iterate (step: 'a -> 'a) (init: 'a) = Job.thunk <| fun () ->
    {Value = init;
     Next = memo (iterate step (step init))}

  let rec filter (pred: 'a -> bool) (xs: Stream<'a>) : Job<Stream<'a>> =
    let next = xs.Next >>= filter pred
    if pred xs.Value then
      Job.result {Value = xs.Value; Next = memo next}
    else
      next

  let sieve : Job<Stream<_>> =
    let rec sieve nats =
      let prime = nats.Value
      {Value = prime;
       Next =
        Promise<Stream<_>> (nats.Next >>= filter (fun x -> x % prime <> 0) >>- sieve)}
    iterate (fun x -> x+1) 2 >>- sieve

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

////////////////////////////////////////////////////////////////////////////////

module HopacStream =
  let sieve () = Stream.delay <| fun () ->
    let rec sieve nats =
      nats >>=* function
       | Stream.Nil -> Stream.nil
       | Stream.Cons (prime, nats) ->
         Stream.cons prime
          (nats
           |> Stream.filterFun (fun x -> x % prime <> 0)
           |> sieve)
    Stream.iterateFun (fun x -> x+1) 2
    |> sieve

  let primes n = Job.delay <| fun () ->
    let before = GC.GetTotalMemory true
    let result = Array.zeroCreate n
    let rec loop i xs =
      if i < n then
        xs >>= function
         | Stream.Nil -> failwith "Impossible"
         | Stream.Cons (x, xs) ->
           result.[i] <- x
           loop (i+1) xs
      else
        printf "%5d b/p " (max 0L (GC.GetTotalMemory true - before) / int64 n)
        Job.result result
    loop 0 (sieve ())

  let run n =
    printf "HopacStream:   "
    let timer = Stopwatch.StartNew ()
    let ps = run (primes n)
    let d = timer.Elapsed
    printf "%7d - %fs\n" ps.[ps.Length-1] d.TotalSeconds
    d

////////////////////////////////////////////////////////////////////////////////

module HopacCh =
  open Hopac.Experimental

  let sieve (primesOut: Pipe.Out<_>) =
    let rec sieve natsIn =
      natsIn >>= fun prime ->
      primesOut prime >>= fun () ->
      natsIn
      |> Pipe.filterFun (fun x -> x % prime <> 0)
      |> Pipe.imp
      >>= sieve
    Pipe.iterateFun 2 (fun x -> x+1) |> Pipe.imp >>= sieve |> Job.server

  let primes n = Job.delay <| fun () ->
    let before = GC.GetTotalMemory true
    Pipe.imp sieve >>= fun primes ->
    let result = Array.zeroCreate n
    let last = n-1
    Job.forUpTo 0 last <| fun i ->
          primes >>- fun p ->
          result.[i] <- p
          if i = last then
            printf "%5d b/p "
              (max 0L (GC.GetTotalMemory true - before) / int64 n)
    >>-. result

  let run n =
    printf "HopacCh:       "
    let timer = Stopwatch.StartNew ()
    let ps = run (primes n)
    let d = timer.Elapsed
    printf "%7d - %fs\n" ps.[ps.Length-1] d.TotalSeconds
    d
    
////////////////////////////////////////////////////////////////////////////////

module AsyncMb =
  type Stream<'a> = MailboxProcessor<AsyncReplyChannel<'a>>
  let inline take (s: Stream<'a>) = s.PostAndAsyncReply id

  let inline result x = async.Return x
  let inline (>>=) x f = async.Bind (x, f)

  let iterate (init: 'a) (step: 'a -> 'a) : Stream<'a> =
    MailboxProcessor.Start <| fun self ->
      let state = ref init
      let rec loop =
        self.Receive () >>= fun out ->
        out.Reply (!state)
        state := step (!state)
        loop
      loop

  let filter (pred: 'a -> bool) (stream: Stream<'a>) : Stream<'a> =
    MailboxProcessor.Start <| fun self ->
      let rec loop =
        async.Delay (fun () -> take stream) >>= fun x ->
        if pred x then
          self.Receive () >>= fun out ->
          out.Reply x
          loop
        else
          loop
      loop

  let sieve () : Stream<int * Stream<int>> =
    MailboxProcessor.Start <| fun self ->
      let rec sieve natsIn =
        take natsIn >>= fun prime ->
        self.Receive () >>= fun out ->
        out.Reply (prime, natsIn)
        sieve (filter (fun n -> n % prime <> 0) natsIn)
      sieve (iterate 2 (fun i -> i+1))

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

////////////////////////////////////////////////////////////////////////////////

module Async =
  open Async.Infixes

  type [<NoComparison>] Stream<'a> = {Value: 'a; Next: Async<Stream<'a>>}

  let rec iterate (step: 'a -> 'a) (init: 'a) : Async<Stream<_>> = async {
    return {Value = init;
            Next = iterate step (step init)}
  }

  let rec filter (pred: 'a -> bool) (xs: Stream<'a>) : Async<Stream<'a>> =
    let next = xs.Next >>= filter pred
    if pred xs.Value then
      Async.result {Value = xs.Value; Next = next}
    else
      next

  let sieve : Async<Stream<_>> =
    let rec sieve nats =
      let prime = nats.Value
      {Value = prime;
       Next = nats.Next >>= filter (fun x -> x % prime <> 0) >>- sieve}
    iterate (fun x -> x+1) 2 >>- sieve

  let primes n = async {
    let before = GC.GetTotalMemory true
    let results = Array.zeroCreate n
    let rec loop i primes =
      if i < n then
        results.[i] <- primes.Value
        primes.Next >>= loop (i+1)
      else
        printf "%5d b/p " (max 0L (GC.GetTotalMemory true - before) / int64 n)
        Async.result results
    return! sieve >>= loop 0
  }

  let run n =
    printf "AsyncStream:   "
    let timer = Stopwatch.StartNew ()
    let ps = Async.RunSynchronously (primes n)
    let d = timer.Elapsed
    printf "%7d - %fs\n" ps.[ps.Length-1] d.TotalSeconds
    d

////////////////////////////////////////////////////////////////////////////////

let inline cleanup (d: TimeSpan) =
  let stop = DateTime.UtcNow + d + d + TimeSpan.FromSeconds 2.0
  while DateTime.UtcNow <= stop do
    GC.clean ()
    Threading.Thread.Sleep 250

do let ns = [10; 100; 1000; 2500; 5000; 7500; 10000]
   for n in ns do
     HopacStream.run n |> cleanup
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
