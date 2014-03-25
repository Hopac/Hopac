﻿open System
open Hopac
open Hopac.Job.Infixes
open Hopac.Alt.Infixes

let workerThreads, _ = System.Threading.ThreadPool.GetAvailableThreads()

let green, red, gray =
    let setColor color () = Console.ForegroundColor <- color
    setColor ConsoleColor.Green, setColor ConsoleColor.Red, setColor ConsoleColor.Gray
        
green()
printfn "Hopac port"
gray()
printfn "Worker threads: %i" workerThreads
printfn "OSVersion: %A" Environment.OSVersion
printfn "ProcessorCount: %A" Environment.ProcessorCount
printfn "ClockSpeed: %s MHZ" (
    try
        use mo = new System.Management.ManagementObject("Win32_Processor.DeviceID='CPU0'")
        string mo.["CurrentClockSpeed"]
    with _ -> "0"
)
printfn "Throughput Setting: 1\n"
printfn "Actor count, Messages/sec"

let destination ch =
    Ch.take ch
    >>= Ch.give ch
    |> Job.forever
    |> Job.server

let client count ch finished =
    let rec loop = function
        | 0, 1 -> Ch.take ch
        | sent, received ->
            Job.delay (fun () ->
                Ch.take ch
                >>= Ch.give ch
                >>. loop (sent - 1, received - 1)
            )

    Ch.give ch ()
    >>. loop (count - 1, count)
    >>= Ch.give finished
    |> Job.start

let benchmark actorPairs =
    let repeatFactor = 500
    let repeat = 30000 * repeatFactor
    let totalMessagesReceived = repeat * 2

    let repeatsPerClient = repeat / actorPairs

    let sw = System.Diagnostics.Stopwatch.StartNew()

    job {
        let! completion = Ch.create()

        for x = 1 to actorPairs do
            let! channel = Ch.create()
            do! client (int repeatsPerClient) channel completion
            do! destination channel
        for x = 1 to actorPairs do
            do! Ch.take completion
    } |> run

    float totalMessagesReceived / sw.Elapsed.TotalSeconds |> int

do
    [1..17]
    |> List.fold (fun fastest actorPairs ->
        let throughput = benchmark actorPairs

        if throughput > fastest then green()
        else red()

        printfn "%i, %i messages/s" (2 * actorPairs) throughput

        max fastest throughput
    ) 0
    |> ignore

    gray()
    printfn "Done..."