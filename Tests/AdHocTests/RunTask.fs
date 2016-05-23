// Copyright (C) by Vesa Karvonen

module RunTask

open System
open System.Runtime.CompilerServices
open System.Threading.Tasks

type Stop () = class end
let stop = Task.FromResult (Stop ()) :> Task
let zero = Task.FromResult ()

let inline (^) x = x
let inline tryAp x2y x yK eK =
  let mutable e = null
  let y = try x2y x with e' -> e <- e' ; Unchecked.defaultof<_>
  match e with
   | null -> yK y
   | e -> eK e
let inline tryIn u2x xK eK = tryAp u2x () xK eK
let inline withTcs tcs2u =
  let tcs = TaskCompletionSource ()
  tcs2u tcs
  tcs.Task
let inline setRes (tcs: TaskCompletionSource<_>) = tcs.SetResult
let inline setExn (tcs: TaskCompletionSource<_>) (e: exn) =
  match e with
   | :? TaskCanceledException -> tcs.SetCanceled ()
   | e -> tcs.SetException e
let inline bindTo tcs (xT: Task<_>) =
  let xA = xT.GetAwaiter ()
  xA.OnCompleted(fun () ->
    tryIn xA.GetResult
     <| setRes tcs
     <| setExn tcs)
let inline bracket (w2xT: _ -> Task<_>) u2u w = withTcs <| fun tcs ->
  tryAp w2xT w
   <| fun xT ->
        let xA = xT.GetAwaiter ()
        xA.OnCompleted(fun () ->
          tryIn (u2u >> xA.GetResult) (setRes tcs) (setExn tcs))
   <| fun e ->
        tryIn u2u
         <| fun () -> setExn tcs e
         <| setExn tcs

type RunTaskBuilder () =
  member __.Bind (wT: Task, w2xT: _ -> Task<_>) = withTcs <| fun tcs ->
    let wA = wT.GetAwaiter()
    wA.OnCompleted(fun () ->
      tryIn (wA.GetResult >> w2xT) (bindTo tcs) (setExn tcs))
  member __.Bind (wT: Task<_>, w2xT: _ -> Task<_>) = withTcs <| fun tcs ->
    let wA = wT.GetAwaiter ()
    wA.OnCompleted(fun () ->
      tryIn (wA.GetResult >> w2xT) (bindTo tcs) (setExn tcs))
  member __.Bind (wT: ConfiguredTaskAwaitable, w2xT: _ -> Task<_>) =
    withTcs <| fun tcs ->
    let wA = wT.GetAwaiter ()
    wA.OnCompleted(fun () ->
      tryIn (wA.GetResult >> w2xT) (bindTo tcs) (setExn tcs))
  member __.Bind (wT: ConfiguredTaskAwaitable<_>, w2xT: _ -> Task<_>) =
    withTcs <| fun tcs ->
    let wA = wT.GetAwaiter ()
    wA.OnCompleted(fun () ->
      tryIn (wA.GetResult >> w2xT) (bindTo tcs) (setExn tcs))
  member __.Combine (wT: Task, w2xT) = __.Bind (wT, w2xT)
  member __.Delay (w2xT: unit -> Task<_>) = w2xT
  member __.For (vs: seq<_>, v2wT: _ -> #Task) = withTcs <| fun tcs ->
    tryIn vs.GetEnumerator
     <| fun vs ->
          let inline disposeAnd ef x =
            try vs.Dispose () ; ef tcs x
            with e -> setExn tcs e
          let rec loop () =
            tryIn <| fun () -> if vs.MoveNext ()
                               then v2wT vs.Current :> Task
                               else stop
             <| fun wT ->
                  if LanguagePrimitives.PhysicalEquality wT stop
                  then disposeAnd setRes ()
                  else let wA = wT.GetAwaiter ()
                       wA.OnCompleted(fun () ->
                         tryIn wA.GetResult loop (disposeAnd setExn))
             <| disposeAnd setExn
          loop ()
     <| setExn tcs
  member __.Return x = Task.FromResult x
  member __.ReturnFrom (xT: Task) =
    match xT with
     | :? Task<unit> as xT -> xT
     | _ -> withTcs <| fun tcs ->
       let xA = xT.GetAwaiter ()
       xA.OnCompleted(fun () ->
         tryIn xA.GetResult (setRes tcs) (setExn tcs))
  member __.ReturnFrom (xT: Task<_>) = xT
  member __.ReturnFrom (xT: ConfiguredTaskAwaitable) = withTcs <| fun tcs ->
    let xA = xT.GetAwaiter ()
    xA.OnCompleted(fun () ->
      tryIn xA.GetResult (setRes tcs) (setExn tcs))
  member __.ReturnFrom (xT: ConfiguredTaskAwaitable<_>) = withTcs <| fun tcs ->
    let xA = xT.GetAwaiter ()
    xA.OnCompleted(fun () ->
      tryIn xA.GetResult (setRes tcs) (setExn tcs))
  member __.Run (w2xT: _ -> Task<_>) = w2xT ()
  member __.TryFinally (w2xT: _ -> Task<_>, u2u) =
    bracket w2xT u2u ()
  member __.TryWith (w2xT: _ -> Task<_>, e2xT: _ -> Task<_>) =
    withTcs <| fun tcs ->
    let e2u e = tryAp e2xT e (bindTo tcs) (setExn tcs)
    tryIn w2xT
     <| fun xT ->
          let xA = xT.GetAwaiter ()
          xA.OnCompleted(fun () ->
            tryIn xA.GetResult (setRes tcs) e2u)
     <| e2u
  member __.Using (x: #IDisposable, x2yT: _ -> Task<_>) =
    bracket x2yT x.Dispose x
  member __.While (u2b, u2wT: _ -> #Task) = withTcs <| fun tcs ->
    let rec loop () =
      tryIn <| fun () -> if u2b () then u2wT () :> Task else stop
       <| fun wT ->
            if LanguagePrimitives.PhysicalEquality wT stop
            then setRes tcs ()
            else let wA = wT.GetAwaiter ()
                 wA.OnCompleted(fun () ->
                   tryIn wA.GetResult loop (setExn tcs))
       <| setExn tcs
    loop ()
  member __.Zero () = zero

let runTask = RunTaskBuilder ()
