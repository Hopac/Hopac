// Copyright (C) by Vesa Karvonen

namespace Hopac.Experimental

open System
open System.Collections.Generic
open Hopac
open Hopac.Infixes
open Hopac.Job.Infixes
open Hopac.Alt.Infixes
open Hopac.Extensions

[<AutoOpen>]
module internal Util =
  let inline memo x = Promise.Now.delayAsAlt x
  let inline (>>=*) x f = x >>= f |> memo
  let inline (|>>*) x f = x |>> f |> memo
  let inline (<|>*) x y = x <|> y |> memo

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

type StreamSrc<'x> = {tail: MVar<IVar<Stream<'x>>>}

module StreamSrc =
  let create () = {tail = mvarFull (ivar ())}
  let inline doTail ss op =
    ss.tail >>=? fun tail ->
    if IVar.Now.isFull tail
    then ss.tail <<-= tail >>! Exception ("StreamSrc: closed")
    else op tail
  let value ss x =
    doTail ss <| fun tail ->
    let next = ivar ()
    ss.tail <<-= next >>. (tail <-= Cons (x, next))
  let error ss e = doTail ss <| fun tail -> tail <-=! e >>. (ss.tail <<-= tail)
  let close ss = doTail ss <| fun tail -> tail <-= Nil >>. (ss.tail <<-= tail)
  let tap ss = MVar.read ss.tail >>= IVar.read |> memo

type StreamVar<'x> = {state: MVar<'x * IVar<Stream<'x>>>}

module StreamVar =
  let create x = {state = mvarFull (x, ivar ())}
  let updateJob sv x2xJ =
    sv.state >>=? fun (x, n) ->
    x2xJ x >>= fun x' ->
    let n' = ivar ()
    n <-= Cons (x', n') >>. (sv.state <<-= (x', n'))
  let updateFun sv x2x = updateJob sv (x2x >> Job.result)
  let tap sv = MVar.read sv.state |>> (fun (x, n) -> Cons (x, n)) |> memo

module Streams =
  let inline nil'<'x> = Alt.always Nil :> Streams<'x>
  let inline nil<'x> = nil'<'x> :> Job<_>
  let inline cons' x xs = Alt.always (Cons (x, xs))
  let inline cons x xs = cons' x xs :> Job<_>

  let zero<'x> = nil'<'x>

  let one x = cons' x zero

  let never<'x> = Alt.never () :> Streams<'x>

  let rec ofEnum (xs: IEnumerator<'x>) = memo << Job.thunk <| fun () ->
    if xs.MoveNext () then Cons (xs.Current, ofEnum xs) else xs.Dispose () ; Nil

  let ofSeq (xs: seq<_>) = memo << Job.delay <| fun () ->
    upcast ofEnum (xs.GetEnumerator ())

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

  let rec ofAlt (xA: Alt<_>) = xA |>>* fun x -> Cons (x, ofAlt xA)

  let rec merge ls rs =
    mergeSwap ls rs <|>* mergeSwap rs ls
  and mergeSwap ls rs =
    ls >>=? function Nil -> upcast rs
                   | Cons (l, ls) -> cons l (merge rs ls)

  let rec append (ls: Streams<_>) (rs: Streams<_>) =
    ls >>=* function Nil -> upcast rs
                   | Cons (l, ls) -> cons l (append ls rs)

  let rec chooseJob x2yOJ xs =
    xs >>=* function
       | Nil -> nil
       | Cons (x, xs) ->
         x2yOJ x >>= function
          | None -> upcast chooseJob x2yOJ xs
          | Some y -> cons y (chooseJob x2yOJ xs)
  let chooseFun x2yO xs = chooseJob (x2yO >> Job.result) xs

  let filterJob x2bJ xs =
    chooseJob (fun x -> x2bJ x |>> fun b -> if b then Some x else None) xs
  let filterFun x2b xs = filterJob (x2b >> Job.result) xs

  let mapJob x2yJ xs = chooseJob (x2yJ >> Job.map Some) xs
  let mapFun x2y xs = mapJob (x2y >> Job.result) xs

  let rec joinWithJob (join: Streams<'x> -> Streams<'y> -> #Job<Streams<'y>>)
                      (xxs: Streams<Streams<'x>>) =
    xxs >>=* function Nil -> nil
                    | Cons (xs, xxs) ->
                      join xs (joinWithJob join xxs) |> Job.join

  let joinWithFun join xxs = joinWithJob (fun a b -> join a b |> Job.result) xxs

  let mergeMapJob x2ysJ xs = xs |> mapJob x2ysJ |> joinWithFun merge
  let mergeMapFun x2ys xs = xs |> mapFun x2ys |> joinWithFun merge

  let appendMapJob x2ysJ xs = xs |> mapJob x2ysJ |> joinWithFun append
  let appendMapFun x2ys xs = xs |> mapFun x2ys |> joinWithFun append

  let rec skipUntil evt xs =
    (evt >>=? fun _ -> upcast xs) <|>*
    (xs >>=? function Nil -> nil
                    | Cons (x, xs) -> upcast skipUntil evt xs)

  let rec switchOn ys xs =
    ys <|>*
    xs >>=? function Nil -> nil
                   | Cons (x, xs) -> cons x (switchOn ys xs)

  let takeUntil evt xs = switchOn (evt >>%? Nil) xs

  let rec catchOnce (e2xs: _ -> Streams<_>) xs =
    Job.tryIn xs
      <| function Nil -> nil
                | Cons (x, xs) -> cons x (catchOnce e2xs xs)
      <| fun e -> upcast e2xs e
    |> memo

  let rec collectLatest (x2ys: 'x -> Streams<'y>) (xs: Streams<'x>) =
    xs >>=* function
       | Nil -> nil
       | Cons (x, xs) ->
         upcast append (takeUntil xs (x2ys x)) (collectLatest x2ys xs)

  let rec throttle timeout xs =
    xs >>=* function Nil -> nil
                   | Cons (x, xs) -> throttleGot1 timeout xs x
  and throttleGot1 timeout xs x =
    (timeout >>=? fun _ -> cons x (throttle timeout xs)) <|>
    (xs >>=? function Nil -> cons x zero
                    | Cons (x, xs) -> throttleGot1 timeout xs x)

  let inline got ls zip =
    ls >>=? function Nil -> nil | Cons (l, ls) -> upcast zip l ls
  let rec zipXY f x xs y ys =
    f x y >>= fun z -> cons z (zipY f xs y ys <|>* zipX f ys x xs)
  and zipYX f y ys x xs =
    f x y >>= fun z -> cons z (zipX f ys x xs <|>* zipY f xs y ys)
  and zipX f ys x xs = got ys (zipXY f x xs)
  and zipY f xs y ys = got xs (zipYX f y ys)
  let zipWithJob f xs ys = got xs (zipX f ys) <|>* got ys (zipY f xs)
  let zipWithFun f xs ys = zipWithJob (fun x y -> f x y |> Job.result) xs ys

  let rec scanJob s2x2sJ s xs =
    cons' s (xs >>=* function Nil -> nil
                            | Cons (x, xs) ->
                              s2x2sJ s x >>= fun s -> upcast scanJob s2x2sJ s xs)
  let scanFun s2x2s s xs = scanJob (fun s x -> s2x2s s x |> Job.result) s xs

  let rec iterJob x2uJ xs =
    xs >>= function Nil -> Job.unit ()
                  | Cons (x, xs) -> x2uJ x >>. iterJob x2uJ xs
  let iterFun (x2u: _ -> unit) xs = iterJob (x2u >> Job.result) xs

  let collectAllAsSeq (xs: Streams<'x>) = Job.delay <| fun () ->
    let ys = ResizeArray<_>()
    iterFun ys.Add xs >>% (ys :> seq<_>)

  let delayEachBy evt xs =
    mapJob (fun x -> evt >>% x) xs

  let groupBy (keyOf: 'x -> 'k) (ss: Streams<'x>) : Streams<'k * Streams<'x>> =
    let key2branch = Dictionary<'k, IVar<Stream<'x>>>()
    let main = ref (ivar ())
    let baton = mvarFull ss
    let raised e =
      key2branch.Values
      |> Seq.iterJob (fun i -> i <-=! e) >>.
      (!main <-=! e) >>! e
    let rec wrap self xs newK oldK =
      (xs >>=? function Nil -> nil | Cons (x, xs) -> cons x (self xs)) <|>*
      (let rec serve ss =
         Job.tryIn ss
          <| function
              | Nil ->
                key2branch.Values
                |> Seq.iterJob (fun i -> i <-= Nil) >>.
                (!main <-= Nil) >>% Nil
              | Cons (s, ss) ->
                tryIn <| fun () -> keyOf s
                 <| fun k ->
                      match key2branch.TryGetValue k with
                       | Just i ->
                         let i' = ivar ()
                         key2branch.[k] <- i'
                         i <-= Cons (s, i') >>. oldK serve ss k s i'
                       | Nothing ->
                         let i' = ivar ()
                         let i = ivarFull (Cons (s, i'))
                         key2branch.Add (k, i')
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
