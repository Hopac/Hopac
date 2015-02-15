
Workflow expression                       | Function expression               | Operator expression
-----------------------------------------:|:---------------------------------:|:----------------------------:
`let! x = xJ; ...`                        | `xJ |> Job.bind (fun x -> ...)`   | `xJ >>= fun x -> ...`
`let! x = xJ; return f x`                 | `xJ |> Job.map f`                 | `xJ |>> f`
`let! _ = xJ; return y`                   |                                   | `xJ >>% y`
`let! _ = xJ; raise e`                    |                                   | `xJ >>! e`
`let! x = xJ; let! y = yJ; return (x, y)` |                                   | `xJ <&> yJ`
