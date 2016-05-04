#!/bin/bash

set -e

if [ ! -f paket.exe ]; then
    mono --debug paket.bootstrapper.exe
fi

if [ ! -d packages ] ; then
    mono --debug paket.exe install
fi

mono --debug packages/FAKE/tools/FAKE.exe build.fsx "$*"
