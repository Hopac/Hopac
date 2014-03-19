// Copyright (C) by Housemarque, Inc.

namespace Hopac.Extra

open System.Collections.Generic
open Hopac
open Hopac.Infixes

[<AutoOpen>]
module SelectableQueueTypes =
  type SelectableQueue<'a> = {
    SendCh: Ch<'a>
    TakeCh: Ch<('a -> bool) * Alt<unit> * Ch<'a>>
  }

module SelectableQueue =
  open Hopac.Alt.Infixes
  open Hopac.Job.Infixes

  type Queue<'a> = LinkedList<'a>
  type Node<'a> = LinkedListNode<'a>
  let nodes (q: Queue<'a>) : seq<Node<'a>> =
    seq {let node = ref q.First
         while null <> !node do
           yield !node
           node := (!node).Next}

  let create () : Job<SelectableQueue<'a>> = Job.delay <| fun () ->
    let q = {SendCh = ch (); TakeCh = ch ()}
    let msgs = Queue<'a>()
    let reqs = Queue<('a -> bool) * Alt<unit> * Ch<'a>>()
    let sendAlt = q.SendCh |>>? fun msg -> msgs.AddLast (Node<_>(msg))
    let takeAlt = q.TakeCh |>>? fun req -> reqs.AddLast (Node<_>(req))
    let prepare (reqNode: Node<_>) : Alt<unit> =
      let (pred, cancel, replyCh) = reqNode.Value
      let cancelAlt = cancel |>>? fun () -> reqs.Remove reqNode
      let giveAlt (msgNode: Node<_>) =
        replyCh <-? msgNode.Value |>>? fun () ->
        reqs.Remove reqNode
        msgs.Remove msgNode
      match nodes msgs |> Seq.tryFind (fun x -> pred x.Value) with
       | None         -> cancelAlt
       | Some msgNode -> cancelAlt <|> giveAlt msgNode
    Job.server
     (Job.forever
       (sendAlt
        <|> takeAlt
        <|> Alt.choose (Seq.map prepare (nodes reqs)))) >>% q

  module Alt =
    let send (q: SelectableQueue<'a>) (x: 'a) : Alt<unit> =
      q.SendCh <-? x

    let take (q: SelectableQueue<'a>) (p: 'a -> bool) : Alt<'a> =
      Alt.withNack <| fun nack ->
      let replyCh = ch ()
      q.TakeCh <-- (p, nack, replyCh) >>% asAlt replyCh
