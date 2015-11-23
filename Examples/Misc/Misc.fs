// Copyright (C) by Housemarque, Inc.

namespace Misc

open Hopac
open Hopac.Infixes

module Alts =
  let sumWith (xy2z: 'x -> 'y -> Job<'z>)
              (xAlt: Alt<'x>)
              (yAlt: Alt<'y>) : Alt<'z> =
    (xAlt <+> yAlt) ^=> fun (x, y) -> xy2z x y

module BufferedChViaPick =
  type Buffer<'a> =
    {InsCh: Ch<'a>
     RemCh: Alt<'a>}
  let create () = Job.delay <| fun () ->
    let insCh = Ch ()
    let remCh = Ch ()
    let rec loop buf =
      match buf with
       | [] ->
         insCh ^=> fun x -> loop [x]
       | x::xs ->
             remCh *<- x ^=> fun () -> loop xs
         <|> insCh       ^=> fun x  -> loop (buf @ [x])
    Job.server (loop []) >>-.
    {InsCh=insCh; RemCh=remCh}
  let insert b x = b.InsCh *<- x
  let remove b = b.RemCh
