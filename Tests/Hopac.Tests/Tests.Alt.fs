module Hopac.Tests.Alt

open System
open Expecto
open Expecto.Flip
open Hopac
open Hopac.Infixes

[<Tests>]
let __ = testList "Alt" [

  testList "promises" [

    testCase "unfulfilled promise is unavailable" <| fun _ ->
      Promise.Now.never () ^->. false
      <|> Alt.always () ^->. true
      |> run |> Expect.equal "Unfulfilled promise was unavailable" true

    testCase "fulfilled promise is available" <| fun _ ->
      Promise.Now.withValue () ^->. true
      <|> Alt.always () ^->. false
      |> run |> Expect.equal "Fulfilled promise was available" true

    testProp "*<+->- nack is unavailable if client commits" <| fun _ ->
      let reqCh = Ch()
      reqCh >>= fun (repCh, nack, x) ->
          nack <|> repCh *<- (x + 1)
      |> Job.forever |> server

      reqCh *<+->- fun repCh nack -> (repCh, nack, 2)
      |> run |> Expect.equal "Nack was unavailable" 3

    testProp "*<+->- nack is available if client doesn't commit" <| fun _ ->
      let reqCh, nackCh = Ch(), Ch()
      reqCh >>= fun (repCh, nack, x) ->
          nack ^=>. nackCh *<+ true
          <|> timeOutMillis 1000 ^=>. repCh *<- x
      |> Job.forever |> server

      (reqCh *<+->- fun repCh nack -> (repCh, nack, 2)) ^->. ()
      <|> Alt.always () |> start

      Ch.take nackCh <|> timeOutMillis 1000 ^->. false
      |> run |> Expect.equal "Nack was available" true
  ]

]