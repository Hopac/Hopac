Background reading: [Where is the monad?](http://www.quanttec.com/fparsec/users-guide/where-is-the-monad.html)

 Workflow expression                      | Function expression                | Operator expression
:---------------------------------------- |:---------------------------------- |:----------------------------
`let! x = xJ; ...`                        | `xJ |> Job.bind (fun x -> ...)`    | `xJ >>= fun x -> ...`
`let! _ = xJ; return! ...`                |                                    | `xJ >>=. ...`
`let! x = xJ; return ...`                 | `xJ |> Job.map (fun x -> ...)`     | `xJ >>- fun x -> ...`
`let! _ = xJ; return ...`                 |                                    | `xJ >>-. ...`
`let! _ = xJ; raise ...`                  |                                    | `xJ >>-! ...`
`let! x = xJ; let! y = yJ; return (x, y)` |                                    | `xJ <&> yJ`

 Function expression               | Operator expression
:--------------------------------- |:-------------------------
`Alt.choose [xA1; xA2]`            | `xA1 <|> xA2`
`Alt.choose [xA1; ...; xAn]`       | `xA1 <|> ... <|> xAn`
`xA |> Alt.afterFun (fun x -> y)`  | `xA ^-> fun x -> y`
`xA |> Alt.afterJob (fun x -> yJ)` | `xA ^=> fun x -> yJ`

 Workflow expression         | Function expression
:--------------------------- |:----------------------------------------------------------
`use x = makeX ; return! yJ` | `Job.using (makeX) <| fun x -> yJ`
`for x in xs do uJ`          | `xs |> Seq.iterJob (fun x -> uJ)`
`while b do uJ`              | `Job.whileDoDelay (fun () -> b) (fun () -> uJ)`
`try xJ with e -> xJ'`       | `Job.tryWithDelay (fun () -> xJ) (fun e -> xJ')`
`try xJ finally u`           | `Job.tryFinallyFunDelay (fun () -> xJ) (fun () -> u)`

 Explicit workflow               | Implicit workflow   | Implicit expression
:------------------------------- |:------------------- |:-----------------------------
`let! x = Ch.take xCh; ...`      | `let! x = xCh; ...` | `xCh >>= fun x -> ...`
`let! x = IVar.read xIv; ...`    | `let! x = xIv; ...` | `xIv >>= fun x -> ...`
`let! x = MVar.take xMv; ...`    | `let! x = xMv; ...` | `xMv >>= fun x -> ...`
`let! x = Mailbox.take xMb; ...` | `let! x = xMb; ...` | `xMb >>= fun x -> ...`
`let! x = Promise.read xPr; ...` | `let! x = xPr; ...` | `xPr >>= fun x -> ...`

 Function expression     | Operator expression
:----------------------- |:----------------------
`Ch.give xCh x`          | `xCh *<- x`
`Ch.send xCh x`          | `xCh *<+ x`
`IVar.fill xIv x`        | `xIv *<= x`
`IVar.fillFailure xIv e` | `xIv *<=! e`
`MVar.fill xMv x`        | `xMv *<<= x`
`Mailbox.send xMb x`     | `xMb *<<+ x`

 Function/workflow expression    | Operator expression
:------------------------------- |:----------------------------
`memo (Alt.choose [xP1; xP2])`   | `xP1 <|>* xP2`
