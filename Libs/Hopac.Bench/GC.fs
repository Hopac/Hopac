namespace Hopac.Bench

open System
open System.Threading

module GC =
  let clean () =
    for i=1 to 2 do
      GC.Collect ()
      GC.WaitForPendingFinalizers ()
    Thread.Sleep 10
