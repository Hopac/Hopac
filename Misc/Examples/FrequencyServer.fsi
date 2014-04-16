// Copyright (C) by Housemarque, Inc.

module FrequencyServer

open Hopac

type Frequency = int
type FrequencyServer

val create: unit -> Job<FrequencyServer>

exception NoFrequency

val allocate: FrequencyServer -> Job<Frequency>
val deallocate: FrequencyServer -> Frequency -> Job<unit>
