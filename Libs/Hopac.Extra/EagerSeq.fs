// Copyright (C) by Housemarque, Inc.

namespace Hopac.Extra

open Hopac
open Hopac.Infixes
open Hopac.Job.Infixes

type EagerSeq<'x> = {EagerSeq: IVar<option<'x * EagerSeq<'x>>>}

module EagerSeq =
  let node () = {EagerSeq = ivar ()}

  let rec tryPickFun (x2yO: 'x -> option<'y>) (xs: EagerSeq<'x>) =
    xs.EagerSeq >>= function
     | None -> Job.result None
     | Some (x, xs) ->
       match x2yO x with
        | None -> tryPickFun x2yO xs
        | some -> Job.result some

  let chooseFun (x2yO: 'x -> option<'y>) (xs: EagerSeq<'x>) = Job.delay <| fun () ->
    let rec loop xs ys =
      Job.tryIn (xs |>> function None -> None
                               | Some (x, xs) -> Some (x2yO x, xs))
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

  let chooseJob (x2yOJ: 'x -> Job<option<'y>>) (xs: EagerSeq<'x>) = Job.delay <| fun () ->
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

  let filterFun x2b xs =
    chooseFun (fun x -> if x2b x then Some x else None) xs
  
  let generateJob (xoJ: Job<option<'x>>) = Job.delay <| fun () ->
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

  let rec iterFun (x2u: 'x -> unit) (xs: EagerSeq<'x>) =
    xs.EagerSeq >>= function None -> Job.result ()
                           | Some (x, xs) -> x2u x ; iterFun x2u xs

  let rec iterJob (x2yJ: 'x -> Job<'y>) (xs: EagerSeq<'x>) =
    xs.EagerSeq >>= function
     | None -> Job.result ()
     | Some (x, xs) -> x2yJ x >>= fun _ -> iterJob x2yJ xs

  let mapFun (x2y: 'x -> 'y) xs =
    chooseFun (fun x -> x2y x |> Some) xs

  let mapJob (x2yJ: 'x -> Job<'y>) xs =
    chooseJob (fun x -> x2yJ x |>> Some) xs

  let toSeq (xs: EagerSeq<'x>) = Job.delay <| fun () ->
    let rs = ResizeArray<_>()
    let rec loop xs =
      xs >>= function
       | None -> Job.result rs
       | Some (x, xs) ->
         rs.Add x
         loop xs.EagerSeq
    loop xs.EagerSeq

  let unfoldFun (s2xsO: 's -> option<'x * 's>) (s: 's) = Job.delay <| fun () ->
    let rec loop s xs =
      let mutable exn = null
      let xsO = try s2xsO s with e -> exn <- e ; None
      match xsO with
       | None ->
         match exn with
          | null ->
            xs <-= None
          | e ->
            IVar.fillFailure xs e
       | Some (x, s) ->
         let xs' = node ()
         xs <-= Some (x, xs') >>= fun () ->
         loop s xs'.EagerSeq
    let xs = node ()
    Job.queue (loop s xs.EagerSeq) >>% xs

  let unfoldJob (s2xsOJ: 's -> Job<option<'x * 's>>) (s: 's) = Job.delay <| fun () ->
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
