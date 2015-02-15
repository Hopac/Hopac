Background reading: [Where is the monad?](http://www.quanttec.com/fparsec/users-guide/where-is-the-monad.html)

 Workflow expression                      | Function expression                | Operator expression
:---------------------------------------- |:---------------------------------- |:----------------------------
`let! x = xJ; ...`                        | `xJ |> Job.bind (fun x -> ...)`    | `xJ >>= fun x -> ...`
`let! x = xJ; return f x`                 | `xJ |> Job.map f`                  | `xJ |>> f`
`let! _ = xJ; return! yJ`                 |                                    | `xJ >>. yJ`
`let! x = xJ; let! _ = yJ; return x`      |                                    | `xJ .>> yJ`
`let! _ = xJ; return y`                   |                                    | `xJ >>% y`
`let! _ = xJ; raise e`                    |                                    | `xJ >>! e`
`let! x = xJ; let! y = yJ; return (x, y)` |                                    | `xJ <&> yJ`

 Function expression               | Operator expression
:--------------------------------- |:-------------------------
`Alt.choose [xA1; xA2]`            | `xA1 <|>? xA2`
`Alt.choose [xA1; ...; xAn]`       | `xA1 <|>? ... <|>? xAn`

 Workflow expression       | Function expression
:------------------------- |:----------------------------------------------------------
`use x = makeX ; ...`      | `Job.using (makeX) (fun x -> ...)`
`for x in xs do ...`       | `xs |> Seq.iterJob (fun x -> ...)`
`while b do ...`           | `Job.whileDo (fun () -> b) (Job.delay (fun () -> ...))`
`try ... with e -> ...`    | `Job.tryWith (Job.delay (fun () -> ...)) (fun e -> ...)`
`try ... finally ...`      | `Job.tryFinallyFun (Job.delay (fun () -> ...)) (fun () -> ...)`

 Explicit workflow               | Implicit workflow   | Implicit expression
:------------------------------- |:------------------- |:-----------------------------
`let! x = Ch.take xCh; ...`      | `let! x = xCh; ...` | `xCh >>= fun x -> ...`
`let! x = IVar.read xI; ...`     | `let! x = xI; ...`  | `xI >>= fun x -> ...`
`let! x = MVar.take xM; ...`     | `let! x = xM; ...`  | `xM >>= fun x -> ...`
`let! x = Mailbox.take xMb; ...` | `let! x = xMb; ...` | `xMb >>= fun x -> ...`
`let! x = Promise.read xP; ...`  | `let! x = xP; ...`  | `xP >>= fun x -> ...`

 Function expression    | Operator expression
:---------------------- |:----------------------
`Ch.give xCh x`         | `xCh <-- x`
`Ch.send xCh x`         | `xCh <-+ x`
`IVar.fill xI x`        | `xI <-= x`
`IVar.fillFailure xI e` | `xI <-=! e`
`MVar.fill xM x`        | `xM <<-= x`
`Mailbox.send xMb x`    | `xMb <<-+ x`

 Function/workflow expression    | Operator expression
:------------------------------- |:----------------------------
`memo (Alt.choose [xP1; xP2])`   | `xP1 <|>* xP2`
