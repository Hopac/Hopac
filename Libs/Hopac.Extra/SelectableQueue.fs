// Copyright (C) by Housemarque, Inc.

namespace Hopac.Extra

open System.Collections.Generic
open Hopac
open Hopac.Alt.Infixes
open Hopac.Infixes
open Hopac.Job.Infixes

type SelectableQueue<'a> = {
  SendCh: Ch<'a>
  TakeCh: Ch<('a -> bool) * Promise<unit> * Ch<'a>>
}

module SelectableQueue =
  let nodes (q: LinkedList<'a>) =
    seq {let node = ref q.First
         while null <> !node do
           yield !node
           node := (!node).Next}

  let create () = Job.delay <| fun () ->
    let q = {SendCh = Ch (); TakeCh = Ch ()}
    let msgs = LinkedList<'a>()
    let reqs = LinkedList<('a -> bool) * Promise<unit> * Ch<'a>>()
    let sendAlt = q.SendCh ^-> fun m -> msgs.AddLast (LinkedListNode<_>(m))
    let takeAlt = q.TakeCh ^-> fun r -> reqs.AddLast (LinkedListNode<_>(r))
    let prepare (reqNode: LinkedListNode<_>) =
      let (pred, cancel, replyCh) = reqNode.Value
      let cancelAlt = cancel ^-> fun () -> reqs.Remove reqNode
      let giveAlt (msgNode: LinkedListNode<_>) =
        replyCh *<- msgNode.Value ^-> fun () ->
        reqs.Remove reqNode
        msgs.Remove msgNode
      match nodes msgs |> Seq.tryFind (fun x -> pred x.Value) with
       | None         -> cancelAlt
       | Some msgNode -> cancelAlt <|> giveAlt msgNode
    sendAlt <|> takeAlt <|> Alt.choose (Seq.map prepare (nodes reqs))
    |> Job.foreverServer >>-. q

  let send q x = q.SendCh *<+ x
  let take q p = q.TakeCh *<+-> fun replyCh nack -> (p, nack, replyCh)
