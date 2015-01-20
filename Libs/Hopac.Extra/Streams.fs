// Copyright (C) by Vesa Karvonen

namespace Hopac.Extra

open System
open System.Collections.Generic
open System.Threading
open Hopac
open Hopac.Infixes
open Hopac.Job.Infixes
open Hopac.Alt.Infixes
open Hopac.Extensions

[<AutoOpen>]
module internal Util =
  let inline memo x = Promise.Now.delay x :> Alt<_>
  let inline (>>=*) x f = x >>= f |> memo
  let inline (|>>*) x f = x |>> f |> memo
  let inline (<|>*) x y = x <|>? y |> memo

  let inline tryIn u2v vK eK =
    let mutable e = null
    let v = try u2v () with e' -> e <- e' ; Unchecked.defaultof<_>
    match e with
     | null -> vK v
     | e -> eK e

  let inline (|Nothing|Just|) (b, x) = if b then Just x else Nothing

type Stream<'x> =
  | Nil
  | Cons of Value: 'x * Next: Alt<Stream<'x>>

type Streams<'x> = Alt<Stream<'x>>

type StreamSrc<'x> = {mutable src: IVar<Stream<'x>>}

module StreamSrc =
  let create () = {src = ivar ()}
  let rec value s x = Job.delay <| fun () ->
    let w = ivar ()
    let v = s.src
    if IVar.Now.isFull v then raise <| Exception ("StreamSrc closed")
    let v' = Interlocked.CompareExchange (&s.src, w, v)
    if LanguagePrimitives.PhysicalEquality v' v
    then v <-= Cons (x, w)
    else value s x
  let error s e = Job.delay <| fun () -> s.src <-=! e // delay required
  let close s = Job.delay <| fun () -> s.src <-= Nil // delay required
  let tap s = s.src :> Alt<_>

type StreamVar<'x> = {mutable var: Stream<'x>}

module StreamVar =
  let create x = {var = Cons (x, ivar ())}
  let get v = match v.var with Cons (x, _) -> x | Nil -> failwith "Impossible"
  let set v x = Job.delay <| fun () ->
    let c = Cons (x, ivar ())
    match Interlocked.Exchange (&v.var, c) with
     | Cons (_, i) -> (i :?> IVar<_>) <-= c
     | Nil -> failwith "Impossible"
  let tap v = Alt.always v.var

module Streams =
  let inline nil<'x> = Alt.always Nil :> Streams<'x>

  let inline consf x xs = Cons (x, xs)
  let cons x xs = Alt.always (consf x xs)

  let inline error e = Alt.raises e :> Streams<_>

  let one x = cons x nil

  let inline never<'x> = Alt.never () :> Streams<'x>

  let rec ofEnum (xs: IEnumerator<'x>) = memo << Job.thunk <| fun () ->
    if xs.MoveNext () then Cons (xs.Current, ofEnum xs) else xs.Dispose () ; Nil

  let ofSeq (xs: seq<_>) = memo <| Job.delay (ofEnum << xs.GetEnumerator)

  type Subscribe<'x> (src: IVar<Stream<'x>>) =
    let mutable src = src
    interface IObserver<'x> with
     override t.OnCompleted () = src <-= Nil |> start
     override t.OnError (e) = src <-=! e |> start
     override t.OnNext (x) =
       let nxt = ivar ()
       src <-= Cons (x, nxt) |> start
       src <- nxt

  let subscribeOnFirst (xs: IObservable<'x>) =
    memo << Job.delay <| fun () ->
    let src = ivar ()
    xs.Subscribe (Subscribe (src)) |> ignore
    src

  let subscribingTo (xs: IObservable<'x>) xs2yJ = Job.delay <| fun () ->
    let ss = StreamSrc.create ()
    Job.using (xs.Subscribe {new IObserver<_> with
      override t.OnCompleted () = StreamSrc.close ss |> start
      override t.OnError (e) = StreamSrc.error ss e |> start
      override t.OnNext (v) = StreamSrc.value ss v |> start}) <| fun _ ->
    StreamSrc.tap ss |> xs2yJ :> Job<_>

  let toObservable xs =
    // XXX Use a better approach than naive locking.
    let subs = HashSet<IObserver<_>>()
    let inline iter f =
      Array.iter f << lock subs <| fun () ->
      let xs = Array.zeroCreate subs.Count
      subs.CopyTo xs
      xs
    let rec loop xs =
      Job.tryIn xs
       <| function Nil -> Job.unit << iter <| fun xS -> xS.OnCompleted ()
                 | Cons (x, xs) -> iter (fun xS -> xS.OnNext x); loop xs
       <| fun e -> Job.unit << iter <| fun xS -> xS.OnError e
    loop xs |> start
    {new IObservable<'x> with
      override this.Subscribe xS =
       lock subs <| fun () -> subs.Add xS |> ignore
       {new IDisposable with
         override this.Dispose () =
          lock subs <| fun () -> subs.Remove xS |> ignore}}

  let rec foreverJob xJ = xJ |>>* fun x -> Cons (x, foreverJob xJ)

  let repeatJobs xJ uJ = xJ |>>* fun x -> Cons (x, foreverJob (uJ >>. xJ))

  let onceJob xJ = xJ |>>* fun x -> Cons (x, nil)

  let inline mapfC c = function Nil -> Nil | Cons (x, xs) -> c x xs
  let inline mapfc c xs = xs |>>? mapfC c
  let inline mapfcm c xs = mapfc c xs |> memo

  let inline mapnc n (c: _ -> _ -> #Job<_>) xs =
    xs >>=? function Nil -> n | Cons (x, xs) -> c x xs
  let inline mapC (c: _ -> _ -> #Job<_>) =
    function Nil -> nil :> Job<_> | Cons (x, xs) -> upcast c x xs
  let inline mapc c xs = xs >>=? mapC c
  let inline mapcm c xs = mapc c xs |> memo

  let rec chooseJob' x2yOJ x xs =
    x2yOJ x >>= function None -> mapc (chooseJob' x2yOJ) xs
                       | Some y -> cons y (chooseJob x2yOJ xs)
  and chooseJob x2yOJ xs = mapcm (chooseJob' x2yOJ) xs
  let rec chooseFun' x2yO x xs =
    match x2yO x with None -> mapc (chooseFun' x2yO) xs
                    | Some y -> cons y (chooseFun x2yO xs)
  and chooseFun x2yO xs = mapcm (chooseFun' x2yO) xs
  let rec choose' xO xOs =
    match xO with None -> mapc choose' xOs | Some x -> cons x (choose xOs)
  and choose xOs = mapcm choose' xOs

  let filterJob x2bJ xs =
    chooseJob (fun x -> x2bJ x |>> fun b -> if b then Some x else None) xs
  let filterFun x2b xs = filterJob (x2b >> Job.result) xs

  let mapJob x2yJ xs = chooseJob (x2yJ >> Job.map Some) xs
  let rec mapFun x2y xs = mapfcm (fun x xs -> Cons (x2y x, mapFun x2y xs)) xs

  let amb ls rs = mapfc consf ls <|>* mapfc consf rs

  let rec mergeSwap ls rs = mapnc rs (fun l ls -> cons l (merge rs ls)) ls
  and merge ls rs = mergeSwap ls rs <|>* mergeSwap rs ls

  let rec append ls rs = mapnc rs (fun l ls -> cons l (append ls rs)) ls

  let rec switch ls rs = rs <|>* mapnc rs (fun l ls -> cons l (switch ls rs)) ls

  let rec joinWith (join: Streams<'x> -> Streams<'y> -> Streams<'y>)
                   (xxs: Streams<Streams<'x>>) =
    mapcm (fun xs xxs -> join xs (joinWith join xxs)) xxs

  let rec mapJoin (join: Streams<'y> -> Streams<'z> -> Streams<'z>)
                  (x2ys: 'x -> Streams<'y>) (xs: Streams<'x>) : Streams<'z> =
    mapcm (fun x xs -> join (x2ys x) (mapJoin join x2ys xs)) xs

  let ambMap x2ys xs = mapJoin amb x2ys xs
  let mergeMap x2ys xs = mapJoin merge x2ys xs
  let appendMap x2ys xs = mapJoin append x2ys xs
  let switchMap x2ys xs = mapJoin switch x2ys xs

  let rec skipUntil evt xs =
    (evt >>=? fun _ -> xs) <|>* mapc (fun _ -> skipUntil evt) xs

  let switchOn rs ls = switch ls rs
  let takeUntil evt xs = switch xs (evt >>%? Nil)

  let rec catchOnce (e2xs: _ -> Streams<_>) xs =
    Job.tryIn xs (mapC (fun x xs -> cons x (catchOnce e2xs xs))) e2xs |> memo

  let rec catch (e2xs: _ -> Streams<_>) xs =
    catchOnce (fun e -> catch e2xs (e2xs e)) xs

  let rec throttleGot1 timeout x xs =
    (timeout |>>? fun _ -> Cons (x, throttle timeout xs)) <|>?
    (xs >>=? function Nil -> one x | Cons (x, xs) -> throttleGot1 timeout x xs)
  and throttle timeout xs = mapcm (throttleGot1 timeout) xs

  let rec holdGot1 timeout timer x xs =
    (timer |>>? fun _ -> Cons (x, hold timeout xs)) <|>?
    (mapc (holdGot1 timeout timer) xs)
  and hold timeout xs = mapcm (holdGot1 timeout (memo timeout)) xs

  let rec clXY x xs y ys = Cons ((x, y), clY xs y ys <|>* clX ys x xs)
  and clYX y ys x xs = Cons ((x, y), clX ys x xs <|>* clY xs y ys)
  and clX ys x xs = mapfc (clXY x xs) ys
  and clY xs y ys = mapfc (clYX y ys) xs
  let combineLatest xs ys = mapc (clX ys) xs <|>* mapc (clY xs) ys

  let rec zipXY x y xs ys = Cons ((x, y), zip xs ys)
  and zipX ys x xs = mapfc (fun y ys -> zipXY x y xs ys) ys
  and zipY xs y ys = mapfc (fun x xs -> zipXY x y xs ys) xs
  and zip xs ys = mapc (zipX ys) xs <|>* mapc (zipY xs) ys

  let rec sj f s x xs = f s x |>> fun s -> Cons (s, mapcm (sj f s) xs)
  let scanJob f s xs = cons s (mapcm (sj f s) xs)
  let rec sf f s x xs = let s = f s x in Cons (s, mapfcm (sf f s) xs)
  let scanFun f s xs = cons s (mapfcm (sf f s) xs)
  let scanFromJob s f xs = scanJob f s xs
  let scanFromFun s f xs = scanFun f s xs

  let rec foldJob f s xs =
    xs >>= function Nil -> Job.result s
                  | Cons (x, xs) -> f s x >>= fun s -> foldJob f s xs
  let foldFun f s xs = foldJob (fun s x -> f s x |> Job.result) s xs
  let foldFromJob s f xs = foldJob f s xs
  let foldFromFun s f xs = foldFun f s xs

  let count xs = foldFun (fun s _ -> s+1) 0 xs |> memo

  let rec iterJob (f: _ -> #Job<unit>) xs =
    xs >>= function Nil -> Job.unit () | Cons (x, xs) -> f x >>. iterJob f xs
  let iterFun (x2u: _ -> unit) xs = iterJob (x2u >> Job.result) xs

  let toSeq xs = Job.delay <| fun () ->
    let ys = ResizeArray<_>()
    iterFun ys.Add xs >>% ys

  let delayEachBy evt xs = mapJob (fun x -> evt >>% x) xs

  let distinctByJob x2kJ xs = memo << Job.delay <| fun () ->
    let ks = HashSet<_>() in filterJob (x2kJ >> Job.map ks.Add) xs
  let distinctByFun x2k xs = distinctByJob (Job.lift x2k) xs

  let rec ducwj eqJ x' x xs =
    eqJ x' x >>= function true -> mapc (ducwj eqJ x) xs
                        | false -> cons x (mapcm (ducwj eqJ x) xs)
  let distinctUntilChangedWithJob eqJ xs =
    mapfcm (fun x xs -> Cons (x, mapcm (ducwj eqJ x) xs)) xs

  let rec ducwf eq x' x xs =
    if eq x' x then mapc (ducwf eq x) xs else cons x (mapcm (ducwf eq x) xs)
  let distinctUntilChangedWithFun eq xs =
    mapfcm (fun x xs -> Cons (x, mapcm (ducwf eq x) xs)) xs

  let rec ducbj x2kJ k' x xs =
    x2kJ x >>= fun k ->
    if k = k' then mapc (ducbj x2kJ k) xs else cons x (mapcm (ducbj x2kJ k) xs)
  let distinctUntilChangedByJob x2kJ xs =
    mapcm (fun x xs -> x2kJ x >>= fun k -> cons x (mapcm (ducbj x2kJ k) xs)) xs

  let rec ducbf x2k k' x xs =
    let k = x2k x
    if k = k' then mapc (ducbf x2k k) xs else cons x (mapcm (ducbf x2k k) xs)
  let distinctUntilChangedByFun x2k xs =
    mapfcm (fun x xs -> Cons (x, mapcm (ducbf x2k (x2k x)) xs)) xs

  let groupByJob (keyOf: 'x -> #Job<'k>) ss =
    let key2br = Dictionary<'k, IVar<Stream<'x>>>()
    let main = ref (ivar ())
    let baton = mvarFull ss
    let raised e =
      key2br.Values |> Seq.iterJob (fun i -> i <-=! e) >>. (!main <-=! e) >>! e
    let rec wrap self xs newK oldK =
      (mapfc (fun x xs -> Cons (x, self xs)) xs) <|>*
      (let rec serve ss =
         Job.tryIn ss
          <| function
              | Nil ->
                key2br.Values
                |> Seq.iterJob (fun i -> i <-= Nil) >>.
                (!main <-= Nil) >>% Nil
              | Cons (s, ss) ->
                Job.tryIn (Job.delay <| fun () -> keyOf s)
                 <| fun k ->
                      match key2br.TryGetValue k with
                       | Just i ->
                         let i' = ivar ()
                         key2br.[k] <- i'
                         i <-= Cons (s, i') >>. oldK serve ss k s i'
                       | Nothing ->
                         let i' = ivar ()
                         let i = ivarFull (Cons (s, i'))
                         key2br.Add (k, i')
                         let i' = ivar ()
                         let m = !main
                         main := i'
                         let ki = (k, wrapBranch k (i :> Streams<_>))
                         m <-= Cons (ki, i') >>. newK serve ss ki i'
                 <| raised
          <| raised
       baton >>=? serve)
    and wrapBranch k xs =
      wrap (wrapBranch k) xs
       <| fun serve ss _ _ -> serve ss
       <| fun serve ss k' x xs ->
            if k = k'
            then baton <<-= ss >>% Cons (x, wrapBranch k xs)
            else serve ss
    let rec wrapMain xs =
      wrap wrapMain xs
       <| fun _ ss ki i -> baton <<-= ss >>% Cons (ki, wrapMain i)
       <| fun serve ss _ _ _ -> serve ss
    !main |> wrapMain
  let groupByFun keyOf ss = groupByJob (keyOf >> Job.result) ss

  let rec sampleGot0 ts xs =
    mapc (fun _ ts -> sampleGot0 ts xs) ts <|>? mapc (sampleGot1 ts) xs
  and sampleGot1 ts x xs =
    mapfc (fun _ ts -> Cons (x, sample ts xs)) ts <|>? mapc (sampleGot1 ts) xs
  and sample ts xs = sampleGot0 ts xs |> memo

  let rec skip' n xs = if 0 < n then mapc (fun _ -> skip' (n-1)) xs else xs
  let skip n xs = if n < 0 then failwith "skip: n < 0" else skip' n xs |> memo

  let rec take' n xs =
    if 0 < n then mapfcm (fun x xs -> Cons (x, take' (n-1) xs)) xs else nil
  let take n xs = if n < 0 then failwith "take: n < 0" else take' n xs

  let inline mapcnm f (xs: Streams<_>) =
    xs >>=* function Nil -> failwith "empty" | Cons (x, xs) -> f x xs

  let head xs = mapcnm (fun x _ -> Job.result x) xs
  let tail xs = skip 1 xs
  let single xs =
    mapcnm (fun x xs -> xs |>> function Nil -> x | _ -> failwith "single") xs
  let last xs = mapcnm (foldFun (fun _ x -> x)) xs

  let rec ts' = function Nil -> Nil | Cons (_, xs) -> Cons (xs, xs |>>* ts')
  let tails xs = cons xs (xs |>>* ts')

  let rec unfoldJob f s =
    f s |>>* function None -> Nil | Some (x, s) -> Cons (x, unfoldJob f s)
  let unfoldFun f s = unfoldJob (Job.lift f) s
  let rec iterateJob' x2xJ x = Cons (x, x2xJ x |>>* iterateJob' x2xJ)
  let iterateJob x2xJ x = cons x (x2xJ x |>>* iterateJob' x2xJ)
  let iterateFun x2x x = iterateJob (x2x >> Job.result) x

  let atDateTimeOffsets dtos =
    dtos
    |> mapJob (fun dto ->
       let ts = dto - DateTimeOffset.Now
       if ts.Ticks <= 0L then Job.result dto else Timer.Global.timeOut ts >>% dto)
  let atDateTimeOffset dto = atDateTimeOffsets (one dto)

  let afterTimeSpan ts = onceJob (Timer.Global.timeOut ts)

  let values (xs: Streams<_>) = Job.delay <| fun () ->
    let out = ch ()
    Job.iterateServer xs (fun xs ->
    Job.tryIn xs
     <| function Nil -> Job.abort ()
               | Cons (x, xs) -> out <-- Choice1Of2 x >>% xs
     <| fun e -> out <-- Choice2Of2 e >>= Job.abort) >>%
    (out |>>? function Choice1Of2 x -> x | Choice2Of2 e -> raise e)

  let rec finallyJob (uJ: Job<unit>) xs =
    Job.tryIn xs
       <| function Nil -> uJ >>% Nil
                 | Cons (x, xs) -> upcast cons x (finallyJob uJ xs)
       <| fun e -> uJ >>! e
    |> memo
