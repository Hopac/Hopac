#!/bin/bash

set -e

if [ ! -f packages/FAKE/tools/Fake.exe ] ; then
    nuget install FAKE -OutputDirectory packages -ExcludeVersion
fi

if [ ! -f packages/SourceLink.Fake/tools/SourceLink.fsx ] ; then
    nuget install SourceLink.Fake -OutputDirectory packages -ExcludeVersion
fi

mono packages/FAKE/tools/FAKE.exe build.fsx "$*"
