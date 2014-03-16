// Copyright (C) by Housemarque, Inc.

module LockServer

open Hopac

type Server
type Lock

val start: Job<Server>

module Now =
  val createLock: Server -> Lock

module Alt =
  val acquire: Server -> Lock -> Alt<unit>

val release: Server -> Lock -> Job<unit>
