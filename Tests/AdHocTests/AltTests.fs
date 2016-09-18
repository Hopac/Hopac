// Copyright (C) by Vesa Karvonen

module AltTests

open FsCheck
open System
open System.Numerics
open Hopac
open Hopac.Infixes

exception TestExn

let run () =
  do let qCh = Ch ()
     qCh >>= fun (rCh, nack, x) ->
         nack <|> rCh *<- (x + 1)
     |> Job.forever |> Hopac.server

     qCh *<+->- fun rCh nack -> (rCh, nack, 2)
     |> run |> testEq 3

     qCh *<+->- fun _ _ -> raise TestExn
     <|> Alt.always 2
     |> run |> testEq 2

     topLevelExns |> Ch.Try.take |> run |> testEq ^ Some TestExn
