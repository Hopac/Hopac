// Copyright (C) by Vesa Karvonen

[<AutoOpen>]
module Util

let inline (^) x = x

let testEq exp act =
  if exp <> act
  then printfn "Expected %A, but got %A" exp act
  else printfn "OK"
