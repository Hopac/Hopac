// Copyright (C) by Housemarque, Inc.

module PrimesStream

open Hopac
open Hopac.Extra
open Hopac.Alt.Infixes
open Hopac.Job.Infixes
open System.Diagnostics

/////////////////////////////////////////////////////////////////////////

let n = 10000

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

  let run () =
    let timer = Stopwatch.StartNew ()
    let ps = primes n
    let d = timer.Elapsed
    printf "Primes are %A\n\nLast prime is %A\n\nSequential time %fs\n" ps ps.[ps.Length-1] d.TotalSeconds

/////////////////////////////////////////////////////////////////////////

module HopacCh =

  let sieve primesOutCh =
    let rec sieve natsInCh =
      Ch.take natsInCh >>= fun prime ->
      Ch.give primesOutCh prime >>= fun () ->
      Ch.Stream.imp
       (Ch.Stream.filter
         (fun x -> Job.result (x % prime <> 0))
         natsInCh) >>= sieve
    Job.start (Ch.Stream.imp (Ch.Stream.iterate 2 (fun x -> Job.result (x+1))) >>= sieve)

  let primes n =
    Ch.Stream.imp sieve >>= fun primesCh ->
    let result = Array.zeroCreate n
    Job.forUpTo 0 (n-1) (fun i ->
      Ch.take primesCh |>> fun p ->
      result.[i] <- p) >>%
    result

  let run () =
    let timer = Stopwatch.StartNew ()
    let ps = run (primes n)
    let d = timer.Elapsed
    printf "Primes are %A\n\nLast prime is %A\n\nHopacCh time %fs\n" ps ps.[ps.Length-1] d.TotalSeconds
    
/////////////////////////////////////////////////////////////////////////

module HopacPromise =
  type [<NoComparison>] Stream<'a> = {Value: 'a; Next: Promise<Stream<'a>>}

  let rec iterate (step: 'a -> Job<'a>) (init: 'a) : Job<Stream<_>> =
    Job.result
     {Value = init;
      Next = Promise.Now.delay (step init >>= iterate step)}

  let rec filter (pred: 'a -> Job<bool>) (xs: Stream<'a>) : Job<Stream<'a>> =
    pred xs.Value >>= fun b ->
    let next = Promise.read xs.Next >>= filter pred
    if b then
      Job.result {Value = xs.Value; Next = Promise.Now.delay next}
    else
      next

  let sieve : Job<Stream<_>> =
    let rec sieve nats =
      let prime = nats.Value
      let pred = Job.lift (fun x -> x % prime <> 0)
      Job.result
       {Value = prime;
        Next =
         Promise.Now.delay (Promise.read nats.Next >>= filter pred >>= sieve)}
    iterate (Job.lift (fun x -> x+1)) 2 >>= sieve
    
  let primes n = Job.delay <| fun () ->
    let result = Array.zeroCreate n
    let rec loop i primes =
      if i < n then
        result.[i] <- primes.Value
        Promise.read primes.Next >>= loop (i+1)
      else
        Job.result result
    sieve >>= loop 0

  let run () =
    let timer = Stopwatch.StartNew ()
    let ps = run (primes n)
    let d = timer.Elapsed
    printf "Primes are %A\n\nLast prime is %A\n\nHopacPromise time %fs\n" ps ps.[ps.Length-1] d.TotalSeconds
    
/////////////////////////////////////////////////////////////////////////

module Async =
  type Stream<'a> = MailboxProcessor<AsyncReplyChannel<'a>>
  let inline take (s: Stream<'a>) = s.PostAndAsyncReply (fun x -> x)
  let inline give (s: Stream<'a>) (x: 'a) = async {
    let! out = s.Receive ()
    out.Reply x
  }

  let iterate (init: 'a) (step: 'a -> Async<'a>) : Stream<'a> =
    MailboxProcessor.Start <| fun self -> async {
      let state = ref init
      while true do
        do! give self (!state)
        let! newState = step (!state)
        state := newState
    }

  let filter (pred: 'a -> Async<bool>) (stream: Stream<'a>) : Stream<'a> =
    MailboxProcessor.Start <| fun self -> async {
      while true do
        let! x = take stream
        let! b = pred x
        do! if b then give self x else async {return ()}
    }

  let sieve () : Stream<int> =
    MailboxProcessor.Start <| fun self -> async {
      let rec sieve natsIn = async {
        let! prime = take natsIn
        do! give self prime
        return! sieve (filter (fun n -> async {return n % prime <> 0}) natsIn)
      }
      return! sieve (iterate 2 (fun i -> async {return i+1}))
    }

  let primes n = async {
    let sieve = sieve ()
    let result = Array.zeroCreate n
    for i=0 to (n-1) do
      let! p = take sieve
      result.[i] <- p
    return result
  }

  let run () =
    use cancelSource = new System.Threading.CancellationTokenSource ()
    let timer = Stopwatch.StartNew ()
    let ps = Async.RunSynchronously (primes n, -1, cancelSource.Token)
    let d = timer.Elapsed
    cancelSource.Cancel ()
    printf "Primes are %A\n\nLast prime is %A\n\nAsync time %fs\n" ps ps.[ps.Length-1] d.TotalSeconds

/////////////////////////////////////////////////////////////////////////

do 
   printf "Running Sequential...\n"
   System.GC.Collect() ; Sequential.run ()
   printf "\n\nSleeping a bit...\n"
   System.Threading.Thread.Sleep (System.TimeSpan.FromSeconds 1.0)

   for i=1 to 3 do
     printf "Running HopacCh...\n"
     System.GC.Collect() ; HopacCh.run ()
     printf "\n\nSleeping a bit...\n"
     System.Threading.Thread.Sleep (System.TimeSpan.FromSeconds 3.0)

   printf "Running HopacPromise...\n"
   System.GC.Collect() ; HopacPromise.run ()
   printf "\n\nSleeping a bit...\n"
   System.Threading.Thread.Sleep (System.TimeSpan.FromSeconds 3.0)

   printf "Running Async...\n"
   System.GC.Collect() ; Async.run ()
