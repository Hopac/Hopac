// Copyright (C) by Housemarque, Inc.

module LockServer

open Hopac

type Server
type Lock

val start: Job<Server>

module Now =
  val createLock: Server -> Lock

val acquire: Server -> Lock -> Alt<unit>
val withLock: Server -> Lock -> Job<'x> -> Alt<'x>

val release: Server -> Lock -> Alt<unit>
