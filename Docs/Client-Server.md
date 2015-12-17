# Client-Server Programming

## Server Pattern

```fsharp
type Server =
  {opCh1: Ch<Request1>
   ...
   opChN: Ch<RequestN>}

let server params =
  let chs = {opCh1 = Ch ()
             ...
             opChN = Ch ()}
  ...
  let rec loop state =
    // "server" ops
    ...
    let op1 ... = ... chs.opCh1 ...
    ...
    let opN ... = ... chs.opChN ...
    ...
    match state with
     | ... ->
       op? ... <|> ... <|> op? ...
     | ... ->
       op? ... <|> ... <|> op? ...
  start <| loop state
  chs

// "client" ops
let op1 server ... = ... server.opCh1 ...
...
let opN server ... = ... server.opChN ...
```

```fsharp
type Server (params) =
  let opCh1 = Ch ()
  ...
  let opChN = Ch ()
  do ...
     let rec loop state =
       // "server" ops
       ...
       let op1 ... = ... opCh1 ...
       ...
       let opN ... = ... opCh2 ...
       ...
       match state with
        | ... ->
          op? ... <|> ... <|> op? ...
        | ... ->
          op? ... <|> ... <|> op? ...
     start <| loop state

  // "client" ops
  member server.op1 ... = ... opCh1 ...
  ...
  member server.opN ... = ... opChN ...
```

## Call Patterns

### SyncOnReply: unit -&gt; Alt&lt;Reply&gt;

```fsharp
server: opCh *<- Reply
```

```fsharp
client: opCh
```

### SyncOnRequest: Request -&gt; Alt&lt;unit&gt;

```fsharp
server: opCh ^=> fun Request -> ...
```

```fsharp
client: opCh *<- Request
```

### SyncOnRequest: Request -&gt; Alt&lt;unit&gt;

```fsharp
server: opCh ^=> function Request replyIv ->
          ...
          replyIv *<= Reply
```

```fsharp
client: opCh *<-=>- fun replyIv -> Request replyIv
```

### SyncOnReply: Request -&gt; Alt&lt;unit&gt;

```fsharp
server: opCh ^=> function Request (replyCh, nack) ->
          ...
          replyCh *<- Reply <|> nack
```

```fsharp
client: opCh *<+->- fun replyCh nack -> Request (replyCh, nack)
```

### AsyncNoReply: Request -&gt; Job&lt;unit&gt;

```fsharp
server: opCh ^=> function Request -> ...
```

```fsharp
client: opCh *<+ Request
```

### AsyncWithReply: Request -&gt; Alt&lt;Reply&gt;

```fsharp
server: opCh ^=> function Request replyIv ->
          ...
          replyIv *<= Reply
```

```fsharp
client: opCh *<+=>- fun replyIv -> Request replyIv
```
