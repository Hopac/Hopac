// Copyright (C) by Vesa Karvonen

namespace Hopac.Platform

open System
open System.Threading
open Hopac
open Hopac.Core

// Note that maxStackSize is never used due to
//   http://apisof.net/catalog/System.Threading.Thread..ctor(ThreadStart,Int32)

module Scheduler =
  let create foreground
             idleHandler
#if !CORECLR
             maxStackSize
#else
             _
#endif
             numWorkers
//             priority
             topLevelHandler =
    let s = Hopac.Scheduler ()
    StaticData.Init ()
    s.TopLevelHandler <- topLevelHandler
    s.IdleHandler <- idleHandler
    s.WaiterStack <- -1
    s.NumActive <- numWorkers
    s.Events <- Array.zeroCreate numWorkers
    let threads = Array.zeroCreate numWorkers
    for i = 0 to numWorkers - 1 do
      s.Events.[i] <- new WorkerEvent (i)
      let thread = new Thread ((fun () -> Worker.Run (s, i))
#if !CORECLR
                             , maxStackSize
#endif
                               )
      thread.Name <- sprintf "Hopac.Worker.%d/%d" i numWorkers
      threads.[i] <- thread
//      thread.Priority <- priority;
      thread.IsBackground <- not foreground
    for i = 0 to numWorkers - 1 do
      threads.[i].Start ()
    s

type [<Class>] Init =
  static member Do () =
    StaticData.createScheduler <- Func<_, _, _, _, _, _>(Scheduler.create)
    StaticData.writeLine <- Action<String>(Console.WriteLine)
    StaticData.Init ()
