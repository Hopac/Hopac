// Copyright (C) by Housemarque, Inc.

namespace Hopac.Experimental

open System.Collections.Generic
open Hopac
open Hopac.Infixes

type SelectableQueue<'a> =
  {SendCh: Ch<'a>
   TakeCh: Ch<('a -> bool) * Ch<'a> * Promise<unit>>}

module SelectableQueue =
  let nodes (q: LinkedList<_>) =
    seq {let node = ref q.First
         while null <> node.Value do
           yield node.Value
           node.Value <- node.Value.Next}

  let create () = Job.delay ^ fun () ->
    let q = {SendCh = Ch (); TakeCh = Ch ()}
    let msgs = LinkedList<'a>()
    let reqs = LinkedList<('a -> bool) * Ch<'a> * Promise<unit>>()
     in nodes reqs
        |> Seq.map ^ fun (reqNode: LinkedListNode<_>) ->
             let (pred, replyCh, cancel) = reqNode.Value
             let cancelAlt = cancel ^-> fun () -> reqs.Remove reqNode
             match nodes msgs |> Seq.tryFind ^ fun x -> pred x.Value with
              | None         -> cancelAlt
              | Some msgNode -> cancelAlt
                            <|> replyCh *<- msgNode.Value ^-> fun () ->
                                  reqs.Remove reqNode
                                  msgs.Remove msgNode
        |> Alt.choose
    <|> q.SendCh ^-> (LinkedListNode >> msgs.AddLast)
    <|> q.TakeCh ^-> (LinkedListNode >> reqs.AddLast)
     |> Job.foreverServer >>-. q

  let send q x = q.SendCh *<+ x
  let take q p = q.TakeCh *<+->- fun replyCh nack -> (p, replyCh, nack)
