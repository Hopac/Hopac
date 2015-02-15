
Workflow expression               | Function expression               | Operator expression
---------------------------------:|:---------------------------------:|:----------------------------:
`let! x = xJ ; ...`               | `xJ |> Job.bind (fun x -> ...)`   | `xJ >>= fun x -> ...`


