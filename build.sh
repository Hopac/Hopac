#!/bin/bash

set -x
set -e

nuget restore Hopac.sln

function build () {
    xbuild /p:Configuration=$1
}

build Debug
build Release
