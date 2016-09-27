## Core (minimal subset) of Hopac

```fs
type Job<'x>
val ( >>= ): Job<'x> -> ('x -> #Job<'y>) -> Job<'y>

type Alt<'x> :> Job<'x>
val always: 'x -> Alt<'x>

type Ch<'x> :> Alt<'x>
val ( *<- ): Ch<'x> -> 'x -> Alt<unit>

val never: unit -> Alt<'x>

val ( <|> ): Alt<'x> -> Alt<'x> -> Alt<'x>
val ( ^=> ): Alt<'x> -> ('x -> #Job<'y>) -> Alt<'y>

type Promise<'x> :> Alt<'x>
val memo: Job<'x> -> Promise<'x>

val withNackJob: (Promise<unit> -> #Job<#Alt<'x>>) -> Alt<'x>

val timeOutMillis: int -> Alt<unit>

type Proc :> Alt<unit>
val self: unit -> Job<Proc>

val startAsProc: Job<unit> -> Proc
val queueAsProc: Job<unit> -> Proc
```
