// Copyright (C) by Housemarque, Inc.

namespace Misc

open Hopac
open Hopac.Extra.Alt.Infixes
open Hopac.Alt.Infixes
open Hopac.Job.Infixes

module Alts =
  let sumWith (xy2z: 'x -> 'y -> Job<'z>)
              (xAlt: Alt<'x>)
              (yAlt: Alt<'y>) : Alt<'z> =
    xAlt <+> yAlt >=> fun (x, y) -> xy2z x y

module BufferedChViaPick =
  type Buffer<'a> =
   {InsCh: Ch<'a>
    RemCh: Ch<'a>}
  let create () =
    Ch.create () >>= fun insCh ->
    Ch.create () >>= fun remCh ->
    let rec loop buf =
      match buf with
       | [] ->
         Ch.take insCh >>= fun x -> loop [x]
       | x::xs ->
         Alt.pick (Ch.Alt.give remCh x >=> fun () -> loop xs
               <|> Ch.Alt.take insCh   >=> fun x  -> loop (buf @ [x]))
    Job.start (loop []) >>%
    {InsCh=insCh; RemCh=remCh}
  let insert b x = Ch.give b.InsCh x
  let remove b = Ch.take b.RemCh
