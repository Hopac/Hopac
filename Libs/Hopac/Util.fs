namespace Hopac

[<AutoOpen>]
module internal Util =
  let inline (^) f x = f x

  let imp () = failwith "Impossible"

  let inline tryIn u2x x2y e2y =
    let mutable e : exn = null
    let x = try u2x () with e' -> e <- e' ; Unchecked.defaultof<_>
    match e with
     | null -> x2y x
     | e    -> e2y e

  let inline tryAp x2y x yK eK =
    let mutable e = null
    let y = try x2y x with e' -> e <- e' ; Unchecked.defaultof<_>
    match e with
     | null -> yK y
     | e -> eK e

  let inline inc (i: byref<int>) : int =
    let j = i+1 in i <- j ; j
  let inline dec (i: byref<int>) : int =
    let j = i-1 in i <- j ; j

  module Option =
    let inline orDefaultOf x =
      match x with
       | None -> Unchecked.defaultof<_>
       | Some x -> x
