// Copyright (C) by Housemarque, Inc.

namespace Hopac.Extra

open Hopac
open Hopac.Infixes
open Hopac.Job.Infixes

type EagerSeq<'x> = {EagerSeq: IVar<option<'x * EagerSeq<'x>>>}

module EagerSeq =
  let node () = {EagerSeq = ivar ()}

  let choose (x2yOJ: 'x -> Job<option<'y>>) (xs: EagerSeq<'x>) = Job.delay <| fun () ->
    let rec loop xs ys =
      Job.tryIn (xs >>= function
                  | None -> Job.result None
                  | Some (x, xs) -> x2yOJ x |>> fun yO -> Some (yO, xs))
       <| function
           | None -> ys <-= None
           | Some (None, xs) -> loop xs.EagerSeq ys
           | Some (Some y, xs) ->
             let ys' = node ()
             ys <-= Some (y, ys') >>= fun () ->
             loop xs.EagerSeq ys'.EagerSeq
       <| fun e -> IVar.fillFailure ys e
    let ys = node ()
    Job.queue (loop xs.EagerSeq ys.EagerSeq) >>% ys
  
  let collect (xoJ: Job<option<'x>>) = Job.delay <| fun () ->
    let rec loop xs =
      Job.tryIn xoJ
       <| function
           | None -> xs <-= None
           | Some x ->
             let xs' = node ()
             xs <-= Some (x, xs') >>= fun () ->
             loop xs'.EagerSeq
       <| fun e -> IVar.fillFailure xs e
    let xs = node ()
    Job.queue (loop xs.EagerSeq) >>% xs

  let filter x2b xs =
    choose (fun x -> Job.result (if x2b x then Some x else None)) xs

  let rec iter (x2yJ: 'x -> Job<'y>) (xs: EagerSeq<'x>) =
    xs.EagerSeq >>= function
     | None -> Job.result ()
     | Some (x, xs) ->
       x2yJ x >>= fun _ -> iter x2yJ xs

  let map (x2yJ: 'x -> Job<'y>) xs =
    choose (fun x -> x2yJ x |>> Some) xs

  let toSeq (xs: EagerSeq<'x>) = Job.delay <| fun () ->
    let rs = ResizeArray<_>()
    let rec loop xs =
      xs >>= function
       | None -> Job.result rs
       | Some (x, xs) ->
         rs.Add x
         loop xs.EagerSeq
    loop xs.EagerSeq

  let unfold (s2xsOJ: 's -> Job<option<'x * 's>>) (s: 's) = Job.delay <| fun () ->
    let rec loop s xs =
      Job.tryIn (Job.delayWith s2xsOJ s)
       <| function
           | None -> xs <-= None
           | Some (x, s) ->
             let xs' = node ()
             xs <-= Some (x, xs') >>= fun () ->
             loop s xs'.EagerSeq
       <| fun e -> IVar.fillFailure xs e
    let xs = node ()
    Job.queue (loop s xs.EagerSeq) >>% xs
    