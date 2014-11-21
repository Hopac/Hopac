// Copyright (C) by Housemarque, Inc.

module FrequencyServer

open Hopac

type Frequency = int
type FrequencyServer

val create: seq<Frequency> -> Job<FrequencyServer>

val allocate: FrequencyServer -> Alt<Frequency>
val deallocate: FrequencyServer -> Frequency -> Job<unit>
