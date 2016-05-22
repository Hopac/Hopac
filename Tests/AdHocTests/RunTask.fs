// Copyright (C) by Vesa Karvonen

module RunTask

open System
open System.Threading.Tasks

let inline (^) x = x
let inline tryAp x2y x yK eK =
  let mutable e = null
  let y = try x2y x with e' -> e <- e' ; Unchecked.defaultof<_>
  match e with
   | null -> yK y
   | e -> eK e
let inline tryIn u2x xK eK = tryAp u2x () xK eK
let inline onCompleted xT2u (xT: #Task) =
  xT.ConfigureAwait(false).GetAwaiter().UnsafeOnCompleted(fun () -> xT2u xT)
let inline setTcs (tcs: TaskCompletionSource<_>) (xT: Task<_>) =
  match xT.Status with
   | TaskStatus.Canceled -> tcs.SetCanceled ()
   | TaskStatus.Faulted  -> tcs.SetException xT.Exception
   | _                   -> tcs.SetResult xT.Result
let inline withTcs tcs2u =
  let tcs = TaskCompletionSource ()
  tcs2u tcs
  tcs.Task
let stop = Task.FromResult () :> Task

let inline result x = Task.FromResult x
let zero = result ()
let inline bind (x2yT: _ -> Task<_>) (xT: Task<_>) = withTcs <| fun tcs ->
  xT
  |> onCompleted ^ fun xT ->
       tryIn <| fun () -> x2yT xT.Result
        <| onCompleted ^ setTcs tcs
        <| fun e ->
             if xT.Status = TaskStatus.Canceled
             then tcs.SetCanceled ()
             else tcs.SetException e
let inline bindTask (u2xT: _ -> Task<_>) (uT: Task) = withTcs <| fun tcs ->
  uT
  |> onCompleted ^ fun uT ->
       match uT.Status with
        | TaskStatus.Canceled -> tcs.SetCanceled ()
        | TaskStatus.Faulted  -> tcs.SetException uT.Exception
        | _ ->
          tryIn u2xT
           <| onCompleted ^ setTcs tcs
           <| tcs.SetException
let inline fromTask (uT: Task) =
  match uT with
   | :? Task<unit> as uT -> uT
   | _ -> withTcs <| fun tcs ->
     uT
     |> onCompleted ^ fun uT ->
          match uT.Status with
           | TaskStatus.Canceled -> tcs.SetCanceled ()
           | TaskStatus.Faulted  -> tcs.SetException uT.Exception
           | _                   -> tcs.SetResult ()
let inline whileDo (u2b: _ -> bool) (u2uT: _ -> #Task) = withTcs <| fun tcs ->
  let rec loop () =
    tryIn <| fun () -> if u2b () then u2uT () :> Task else stop
     <| fun uT ->
          if LanguagePrimitives.PhysicalEquality uT stop
          then tcs.SetResult ()
          else uT
               |> onCompleted ^ fun uT ->
                    match uT.Status with
                     | TaskStatus.Canceled -> tcs.SetCanceled ()
                     | TaskStatus.Faulted  -> tcs.SetException uT.Exception
                     | _                   -> loop ()
     <| tcs.SetException
  loop ()
let inline iterTask (x2uT: _ -> #Task) (xs: seq<_>) = withTcs <| fun tcs ->
  tryIn xs.GetEnumerator
   <| fun xs ->
        let inline disposeAnd ef x = xs.Dispose () ; ef x
        let rec loop () =
          tryIn <| fun () -> if xs.MoveNext ()
                             then x2uT xs.Current :> Task
                             else stop
           <| fun uT ->
                if LanguagePrimitives.PhysicalEquality uT stop
                then disposeAnd tcs.SetResult ()
                else uT
                     |> onCompleted ^ fun uT ->
                          match uT.Status with
                           | TaskStatus.Canceled ->
                             disposeAnd tcs.SetCanceled ()
                           | TaskStatus.Faulted ->
                             uT.Exception |> disposeAnd tcs.SetException
                           | _ ->
                             loop ()
           <| disposeAnd tcs.SetException
        loop ()
   <| tcs.SetException
let inline tryFinally u2u (u2xT: _ -> Task<_>) = withTcs <| fun tcs ->
  tryIn u2xT
   <| onCompleted ^ fun xT ->
        tryIn u2u
         <| fun () -> setTcs tcs xT
         <| tcs.SetException
   <| tcs.SetException
let inline tryWith (e2xT: _ -> Task<_>) (u2xT: _ -> Task<_>) = withTcs <| fun tcs ->
  tryIn u2xT
   <| onCompleted ^ fun xT ->
        let s = xT.Status
        if s = TaskStatus.RanToCompletion
        then tcs.SetResult xT.Result
        else tryAp e2xT <| if s = TaskStatus.Canceled
                           then TaskCanceledException (xT) :> exn
                           else xT.Exception :> exn
              <| onCompleted ^ setTcs tcs
              <| tcs.SetException
   <| tcs.SetException
let inline using (x2yT: _ -> Task<_>) (x: #IDisposable) = withTcs <| fun tcs ->
  tryAp x2yT x
   <| onCompleted ^ fun yT ->
        tryIn x.Dispose
         <| fun () -> setTcs tcs yT
         <| tcs.SetException
   <| fun e ->
        tryIn x.Dispose
         <| fun () -> tcs.SetException e
         <| tcs.SetException

type RunTaskBuilder () =
  member __.Bind (uT, u2xT) = bindTask u2xT uT
  member __.Bind (xT, x2yT) = bind x2yT xT
  member __.Combine (uT, u2xT) = bindTask u2xT uT
  member __.Delay (u2xT: unit -> Task<_>) = u2xT
  member __.For (xs, x2uT) = iterTask x2uT xs
  member __.Return x = result x
  member __.ReturnFrom (xT: Task<_>) = xT
  member __.ReturnFrom uT = fromTask uT
  member __.Run (u2xT: _ -> Task<_>) = u2xT ()
  member __.TryFinally (u2xT, u2u) = tryFinally u2u u2xT
  member __.TryWith (u2xT, e2xT) = tryWith e2xT u2xT
  member __.Using (x, x2yT) = using x2yT x
  member __.While (u2b, u2uT) = whileDo u2b u2uT
  member __.Zero () = zero

let runTask = RunTaskBuilder ()
