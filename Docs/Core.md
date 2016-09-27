## Core (minimal subset) of Hopac

For practical reasons (performance and convenience), Hopac has a relatively
large [API](http://hopac.github.io/Hopac/Hopac.html).  This document tries to
capture and describe a *minimal subset* of Hopac that could be used to implement
*everything* else.  Note that for just understanding the main ideas, an even
smaller subset should suffice, but then there would be some important semantics
that could not be implemented precisely.

```fs
type Job<'x> = class end
val ( >>= ): Job<'x> -> ('x -> #Job<'y>) -> Job<'y>

type Alt<'x> = inherit Job<'x>
val always: 'x -> Alt<'x>

val tryIn: Job<'x> -> ('x -> #Job<'y>) -> (exn -> #Job<'y>) -> Job<'y>

type Ch<'x> = inherit Alt<'x>
val ( *<- ): Ch<'x> -> 'x -> Alt<unit>

val never: unit -> Alt<'x>

val ( <|> ): Alt<'x> -> Alt<'x> -> Alt<'x>
val ( ^=> ): Alt<'x> -> ('x -> #Job<'y>) -> Alt<'y>

type Promise<'x> = inherit Alt<'x>
val memo: Job<'x> -> Promise<'x>

val withNackJob: (Promise<unit> -> #Job<#Alt<'x>>) -> Alt<'x>

val tryIn: Alt<'x> -> ('x -> #Job<'y>) -> (exn -> #Job<'y>) -> Alt<'y>

val timeOutMillis: int -> Alt<unit>

type Proc = inherit Alt<unit>
val self: unit -> Job<Proc>

val startAsProc: Job<unit> -> Proc
val queueAsProc: Job<unit> -> Proc
```
