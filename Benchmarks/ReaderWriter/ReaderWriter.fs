module ReaderWriter

// Inspired by http://t0yv0.blogspot.com/2011/12/making-async-5x-faster.html

module HopacRW =
  open Hopac
  open Hopac.Job.Infixes
  open System
  open System.IO
  open System.Diagnostics

  let run n =
    let timer = Stopwatch.StartNew ()
    let i =
      Job.Now.run <| job {
        let ch = Ch.Now.create ()
        let rec writer i = job {
          if i = 0 then
            return! Ch.give ch 0
          else
            do! Ch.give ch i
            return! writer (i-1)
        }
        let rec reader sum = job {
          let! x = Ch.take ch
          if x = 0 then
            return sum
          else
            return! reader (sum + x)
        }
        do! Job.start (writer n)
        return! reader 0
      }
    let d = timer.Elapsed
    let m = sprintf "Hops per second: %f (Hopac)\n" (float n / d.TotalSeconds)
  //  do use w = new StreamWriter ("Results.txt", true)
  //     w.Write m
    printf "%s" m

do System.GC.Collect () ; HopacRW.run 2000
   System.GC.Collect () ; HopacRW.run 20000000
