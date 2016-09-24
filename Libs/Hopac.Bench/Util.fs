namespace Hopac.Bench

open System

[<AutoOpen>]
module Util =
  let inline (^) f x = f x

  let isMono =
    match Type.GetType "Mono.Runtime" with
     | null -> false
     | _ -> true

  let inline dispose (d: IDisposable) = d.Dispose ()
