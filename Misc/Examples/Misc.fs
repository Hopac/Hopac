// Copyright (C) by Housemarque, Inc.

namespace Misc

open Hopac
open Hopac.Infixes
open Hopac.Extra.Alt.Infixes
open Hopac.Alt.Infixes
open Hopac.Job.Infixes

module Alts =
  let sumWith (xy2z: 'x -> 'y -> Job<'z>)
              (xAlt: Alt<'x>)
              (yAlt: Alt<'y>) : Alt<'z> =
    xAlt <+> yAlt >>=? fun (x, y) -> xy2z x y

module BufferedChViaPick =
  type Buffer<'a> =
    {InsCh: Ch<'a>
     RemCh: Ch<'a>}
  let create () = Job.delay <| fun () ->
    let insCh = ch ()
    let remCh = ch ()
    let rec loop buf =
      match buf with
       | [] ->
         insCh >>= fun x -> loop [x]
       | x::xs ->
         (remCh <-- x >>=? fun () -> loop xs) <|>?
         (insCh       >>=? fun x  -> loop (buf @ [x])) :> Job<_>
    Job.server (loop []) >>%
    {InsCh=insCh; RemCh=remCh}
  let insert b x = b.InsCh <-- x
  let remove b = asJob b.RemCh
