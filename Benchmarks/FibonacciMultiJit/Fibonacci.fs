
module Fibonacci

/////////////////////////////////////////////////////////////////////////

open Hopac
open Hopac.Infixes
open System
open System.Threading
open System.Threading.Tasks
open System.Diagnostics


/////////////////////////////////////////////////////////////////////////

let mutable numSpawns = 0L

module SerialFun =
  let rec fib n =
    if n < 2L then
      n
    else
      numSpawns <- numSpawns + 1L
      fib (n-2L) + fib (n-1L)

  let run n =
    numSpawns <- 0L
    //printf "SerFun: "
    //let timer = Stopwatch.StartNew ()
    let r = fib n
    //let d = timer.Elapsed
    //printf "%d - %fs (%d recs)\n" r d.TotalSeconds numSpawns
    ()

/////////////////////////////////////////////////////////////////////////

module SerialJob =
  let rec fib n = job {
    if n < 2L then
      return n
    else
      let! x = fib (n-2L)
      let! y = fib (n-1L)
      return x + y
  }

  let run n =
    printf "SerJob: "
    let timer = Stopwatch.StartNew ()
    let r = run (fib n)
    let d = timer.Elapsed
    printf "%d - %fs\n" r d.TotalSeconds

/////////////////////////////////////////////////////////////////////////

module SerialOpt =
  let rec fib n =
    if n < 2L then
      Job.result n
    else
      fib (n-2L) >>= fun x ->
      fib (n-1L) >>- fun y ->
      x + y

  let run n =
    //printf "SerOpt: "
    //let timer = Stopwatch.StartNew ()
    let r = run (fib n)
    //let d = timer.Elapsed
    //printf "%d - %fs\n" r d.TotalSeconds
    ()

/////////////////////////////////////////////////////////////////////////

module ParallelJob =
  let rec fib n = job {
    if n < 2L then
      return n
    else
      let! (x, y) = fib (n-2L) <*> fib (n-1L)
      return x + y
  }

  let run n =
    printf "ParJob: "
    let timer = Stopwatch.StartNew ()
    let r = run (fib n)
    let d = timer.Elapsed
    printf "%d - %fs (%f jobs/s)\n"
     r d.TotalSeconds (float numSpawns / d.TotalSeconds)

/////////////////////////////////////////////////////////////////////////

module ParallelPro =
  let rec fib n =
    if n < 2L then
      Job.result n
    else
      fib (n-2L) |> Promise.start >>= fun xP ->
      fib (n-1L) >>= fun y ->
      xP >>- fun x ->
      x + y

  let run n =
    printf "ParPro: "
    let timer = Stopwatch.StartNew ()
    let r = run (fib n)
    let d = timer.Elapsed
    printf "%d - %fs (%f jobs/s)\n"
     r d.TotalSeconds (float numSpawns / d.TotalSeconds)

/////////////////////////////////////////////////////////////////////////

module ParallelOpt =
  let rec fib n =
    if n < 2L then
      Job.result n
    else
      fib (n-2L) <*> Job.delayWith fib (n-1L) >>- fun (x, y) ->
      x + y

  let run n =
    //printf "ParOpt: "
    //let timer = Stopwatch.StartNew ()
    let r = run (fib n)
    //let d = timer.Elapsed
    //printf "%d - %fs (%f jobs/s)\n"
    // r d.TotalSeconds (float numSpawns / d.TotalSeconds)
    ()

/////////////////////////////////////////////////////////////////////////

module SerAsc =
  let rec fib n = async {
    if n < 2L then
      return n
    else
      let! x = fib (n-2L)
      let! y = fib (n-1L)
      return x + y
  }

  let run n =
    printf "SerAsc: "
    let timer = Stopwatch.StartNew ()
    let r = Async.RunSynchronously (fib n)
    let d = timer.Elapsed
    printf "%d - %fs\n" r d.TotalSeconds

/////////////////////////////////////////////////////////////////////////

module ParAsc =
  let rec fib n = async {
    if n < 2L then
      return n
    else
      let! x = Async.StartChild (fib (n-2L))
      let! y = fib (n-1L)
      let! x = x
      return x + y
  }

  let run n =
    printf "ParAsc: "
    let timer = Stopwatch.StartNew ()
    let r = Async.RunSynchronously (fib n)
    let d = timer.Elapsed
    printf "%d - %fs\n" r d.TotalSeconds

/////////////////////////////////////////////////////////////////////////

module Task =
  let rec fib n =
    if n < 2L then
      n
    else
      let x = Task.Factory.StartNew (fun _ -> fib (n-2L))
      let y = fib (n-1L)
      x.Result + y

  let run n =
    printf "ParTsk: "
    let timer = Stopwatch.StartNew ()
    let r = fib n
    let d = timer.Elapsed
    printf "%d - %fs (%f tasks/s)\n"
     r d.TotalSeconds (float numSpawns / d.TotalSeconds)

/////////////////////////////////////////////////////////////////////////

module FibNck =
  let rec fibWithNack (n: int64) (cancel: Alt<int64>) : Job<int64> =
    if n < 2L then
      Job.result n
    else
      Promise.queue (fibWithNack (n-2L) cancel) >>= fun xP ->
      Promise.start (fibWithNack (n-1L) cancel) >>= fun yP ->
          cancel
      <|> xP ^=> fun x -> cancel <|> yP ^-> fun y -> x+y
      <|> yP ^=> fun y -> cancel <|> xP ^-> fun x -> x+y

  let fib n =
    Alt.withNackJob <| fun nack ->
    Promise.start (fibWithNack n (nack ^->. 0L))

  let run n =
    printf "FibNck: "
    let timer = Stopwatch.StartNew ()
    let r = run (fib n)
    let d = timer.Elapsed
    printf "%d - %fs\n" r d.TotalSeconds

/////////////////////////////////////////////////////////////////////////

open BenchmarkDotNet.Attributes

//[<Jobs.ClrJob; Jobs.CoreJob>]
//[<Jobs.AllJitsJob; Jobs.ClrJob>]
//[<Config("jobs=Dry")>]
type FibBench () =

    //[<Params(10L, 20L, 30L, 40L)>]
    [<Params(40L)>]
    member val N = 0L with get, set

(*
    [<Benchmark>]
    member this.SerialFun () =
        SerialFun.run this.N
*)

    [<Benchmark>]
    member this.ParallelOpt () =
        ParallelOpt.run this.N

//ParallelJob
//ParallelPro
//Task

    [<Benchmark>]
    member this.SerialOpt () =
       SerialOpt.run this.N

//SerialJob
//FibNck
//SerAsc
//ParAsc


#if FALSE

do for n in [10L; 20L; 30L; 40L] do
     SerialFun.run n ; cleanup ()
     ParallelOpt.run n ; cleanup ()
     ParallelJob.run n ; cleanup ()
     ParallelPro.run n ; cleanup ()
     Task.run n ; cleanup ()
     SerialOpt.run n ; cleanup ()
     SerialJob.run n ; cleanup ()
     FibNck.run n ; cleanup ()
     SerAsc.run n ; cleanup ()
     if n <= 30L then
       ParAsc.run n ; cleanup ()

#endif

open BenchmarkDotNet.Configs
open BenchmarkDotNet.Jobs
open BenchmarkDotNet.Validators


[<EntryPoint>]
let main argv =

#if BENCHMARKDOTNET
    let cfg = 
      ManualConfig
        .Create(DefaultConfig.Instance)
        .With(Job.Dry.With(Runtime.Clr).With(Jit.LegacyJit))
        .With(Job.Dry.With(Runtime.Clr).With(Jit.RyuJit))
        .With(Job.Dry.With(Runtime.Core))
        //.With(Job.Core)
        //.With(ExecutionValidator.FailOnError)

    let summary = BenchmarkDotNet.Running.BenchmarkRunner.Run<FibBench>(cfg)
#else
    let tryRun name f =
      try
        printfn "Running %s .." name
        f ()
        printfn " [OK]"
      with ex ->
        printfn " [Failed]: %s" (ex.Message)

    let bench = FibBench()
    bench.N <- 40L

    tryRun "ParallelOpt" (fun () -> bench.ParallelOpt())
    tryRun "SerialOpt" (fun () -> bench.SerialOpt())
#endif

    0
