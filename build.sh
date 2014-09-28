#!/bin/bash

set -e

nuget restore Hopac.sln -Verbosity quiet

function build () {
    xbuild /nologo /verbosity:quiet /p:Configuration=$2 $1
}

build Hopac.sln Debug
build Hopac.sln Release
#build Hopac-Xamarin.sln Debug
#build Hopac-Xamarin.sln Release
