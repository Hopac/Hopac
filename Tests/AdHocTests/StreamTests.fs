// Copyright (C) by Vesa Karvonen

module StreamTests

open FsCheck
open System
open System.Numerics
open System.Threading
open Hopac
open Hopac.Infixes

module Stream =
  let ofList (xs: list<_>) = Stream.ofSeq xs
  let toList xs = Stream.toSeq xs >>- List.ofSeq
  let onList f xs = ofList xs |> f |> toList |> run
  let onList2 f xs ys = f (ofList xs) (ofList ys) |> toList |> run

let quick = Check.Quick

let run () =
  let src = Stream.Src.create ()
  let filler = Job.Scheduler.switchToWorker ()
          >>=. Job.forN 10000 ^ Stream.Src.value src 0
          >>=. Stream.Src.error src ^ Expected 1
  filler <*> filler
  |> Job.catch |> run
  |> testExpected [Expected 1]

  let fibs: Stream<BigInteger> =
    let rec lp f0 f1 = Stream.cons f0 << Stream.delay <| fun () -> lp f1 (f0 + f1)
    lp 0I 1I
  fibs
  |> Stream.take 8L
  |> Stream.toList
  |> run
  |> testEq [0I;1I;1I;2I;3I;5I;8I;13I]

  Stream.append
   (Stream.iterateFun ((+) 1) 1 |> Stream.take 4L)
   (Stream.ofSeq [5;6;7])
  |> Stream.mapFun (fun x -> x + 1)
  |> Stream.toList
  |> run
  |> testEq [2;3;4;5;6;7;8]

  // The following are some quickly written naive property based test

  do let n = ref 0
     let m = ref 0
     quick <| fun (xs: list<int>) ->
       Stream.ofList xs
       |> Stream.mapPipelinedJob 10 ^ fun x ->
            let n' = Interlocked.Increment n
            if n' > 10 then failwithf "Too many in parallel."
            m := max !m n'
            timeOutMillis (Math.Abs (x % 10)) >>- fun () ->
            Interlocked.Decrement n |> ignore
            x
       |> Stream.toList |> run = xs
     if !m <> 10
     then printfn "Not OK? Effective degree only %d with mapPipelinedJob %d." !m 10
     else printfn "OK, reached effective degree %d with mapPipelinedJob." !m

  quick <| fun (toTake: uint8) ->
    let n = ref 0
    Stream.unfoldFun (fun x -> n := !n + 1 ; Some (x, x+1)) 1
    |> Stream.take (int64 toTake) |> Stream.iter |> run
    !n = int toTake
  quick <| fun (toTake: uint8) ->
    let n = ref 0
    Stream.unfoldJob (fun x -> Job.thunk <| fun () -> n := !n + 1 ; Some (x, x+1)) 1
    |> Stream.take (int64 toTake) |> Stream.iter |> run
    !n = int toTake

  quick <| fun f (toTake: uint8) ->
    let n = ref 0
    Stream.iterateFun (fun x -> n := !n + 1 ; f x) 1
    |> Stream.take (int64 toTake) |> Stream.iter |> run
    !n = max 0 (int toTake - 1)
  quick <| fun f (toTake: uint8) ->
    let n = ref 0
    Stream.iterateJob (fun x -> Job.thunk <| fun () -> n := !n + 1 ; f x) 1
    |> Stream.take (int64 toTake) |> Stream.iter |> run
    !n = max 0 (int toTake - 1)

  quick <| fun xs ->
    let s = Stream.ofList xs
    Stream.zip s (Stream.indefinitely (Stream.values s))
    |> Stream.toList |> run = List.zip xs xs

  quick <| fun (xs: list<_>) (f: _ -> _) ->
    Stream.onList (Stream.mapFun f) xs = List.map f xs
  quick <| fun (xs: list<_>) (f: _ -> _) ->
    Stream.onList (Stream.mapJob (Job.lift f)) xs = List.map f xs
  quick <| fun (xs: list<_>) (f: _ -> option<_>) ->
    Stream.onList (Stream.chooseFun f) xs = List.choose f xs
  quick <| fun (xs: list<_>) (f: _ -> option<_>) ->
    Stream.onList (Stream.chooseJob (Job.lift f)) xs = List.choose f xs
  quick <| fun xs ys ->
    Stream.onList2 Stream.append xs ys = List.append xs ys
  quick <| fun i xs ->
    let i = abs i
    Stream.onList (fun xs -> Stream.append (Stream.take i xs) (Stream.skip i xs)) xs = xs
  quick <| fun xs ->
    Stream.onList (fun xs -> Stream.append (Stream.head xs) (Stream.tail xs)) xs = xs
  quick <| fun xs ->
    Stream.onList (fun xs -> Stream.append (Stream.init xs) (Stream.last xs)) xs = xs
  quick <| fun xs ->
    xs
    |> Stream.onList (fun xs ->
       Stream.zip (Stream.inits xs) (Stream.tails xs)
       |> Stream.mapJob (fun (init, tail) ->
          Stream.append init tail |> Stream.toList))
    |> List.forall (fun xs' -> xs' = xs)
  quick <| fun xs ys ->
    Stream.onList2 Stream.zip xs ys = List.ofSeq (Seq.zip xs ys)
  quick <| fun xs ->
    Stream.ofSeq xs |> Stream.count |> run |> int = List.length xs
  quick <| fun xs s f ->
    xs |> Stream.onList (Stream.scanFun f s) = List.scan f s xs
  quick <| fun xs s f ->
    xs
    |> Stream.onList
        (Stream.scanJob (fun s x -> f s x |> Job.result) s) = List.scan f s xs
  quick <| fun xs f ->
    xs |> Stream.onList (Stream.distinctByFun f) = List.ofSeq (Seq.distinctBy f xs)
  quick <| fun (xs: list<byte>) (f: byte -> byte) ->
    xs |> Stream.onList (Stream.distinctByJob (Job.lift f)) = List.ofSeq (Seq.distinctBy f xs)
  quick <| fun (xs: list<int>) (f: int -> byte) ->
    (xs
     |> Stream.onList (fun xs ->
        xs
        |> Stream.groupByFun (fun _ _ xs -> xs) f
        |> Stream.mapJob (fun xs -> Stream.toList xs)))
     = (xs
       |> Seq.groupBy f
       |> Seq.map (fun (_, xs) -> List.ofSeq xs)
       |> List.ofSeq)
  quick <| fun xs f ->
    xs |> Stream.onList (Stream.filterFun f) = List.filter f xs
  quick <| fun xs f ->
    xs |> Stream.onList (Stream.filterJob (Job.lift f)) = List.filter f xs
  quick <| fun s f ->
    Stream.unfoldFun f s |> Stream.take 10L |> Stream.toList |> run =
     List.ofSeq (Seq.unfold f s |> Seq.truncate 10)
  quick <| fun s f ->
    Stream.unfoldJob (Job.lift f) s |> Stream.take 10L |> Stream.toList |> run =
     List.ofSeq (Seq.unfold f s |> Seq.truncate 10)
  quick <| fun (xs: list<byte>) ->
    Stream.onList (Stream.distinctUntilChangedWithFun (=)) xs =
     Stream.onList (Stream.distinctUntilChangedWithJob (fun a b -> a = b |> Job.result)) xs
  quick <| fun (xs: list<byte>) (f: byte -> byte) ->
    Stream.onList (Stream.distinctUntilChangedByFun f) xs =
     Stream.onList (Stream.distinctUntilChangedByJob (Job.lift f)) xs
  quick <| fun (xs: list<byte>) ->
    Stream.onList Stream.distinctUntilChanged xs =
     Stream.onList (Stream.distinctUntilChangedByFun id) xs

  quick <| fun xs -> xs |> Stream.onList (Stream.shift (timeOutMillis 1)) = xs
  quick <| fun xs -> xs |> Stream.onList (Stream.delayEach (timeOutMillis 1)) = xs
  quick <| fun xs -> xs |> Stream.onList (Stream.afterEach (timeOutMillis 1)) = xs
  quick <| fun xs -> xs |> Stream.onList (Stream.beforeEach (timeOutMillis 1)) = xs
  quick <| fun xs ->
    let values =
        [ Stream.ofList xs; Stream.never ]
        |> Stream.ofSeq
        |> Stream.mergeAll
        |> Stream.take (int64 xs.Length)
        |> Stream.toList
        |> memo
        :> Alt<_>
    
    values ^-> Choice1Of2
    <|> timeOutMillis 100 ^-> Choice2Of2
    |> run = Choice1Of2 xs

  quick <| fun xs ->
    let values =
        [ Stream.ofList xs; Stream.once (timeOutMillis 50) ]
        |> Stream.ofSeq
        |> Stream.mergeAll
        |> Stream.toList
        |> memo
        :> Alt<_>
    
    values ^-> Choice1Of2
    <|> timeOutMillis 100 ^-> Choice2Of2
    |> run = Choice1Of2 (xs @ [ () ])

