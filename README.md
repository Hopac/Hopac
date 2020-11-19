[Reference](http://hopac.github.io/Hopac/Hopac.html) —
[Guide](./Docs/Programming.md) —
[Docs](./Docs/)

Hopac is a [Concurrent ML](http://cml.cs.uchicago.edu/) style concurrent
programming library for F#.

[![NuGet version](https://badge.fury.io/nu/Hopac.svg)](https://badge.fury.io/nu/Hopac)
[![AppVeyor Build status](https://ci.appveyor.com/api/projects/status/srux0s4jy3ahvb84?svg=true)](https://ci.appveyor.com/project/VesaKarvonen/hopac) [![Travis Build Status](https://travis-ci.org/Hopac/Hopac.svg?branch=master)](https://travis-ci.org/Hopac/Hopac)

## Development

Check out the repo and use your favorite IDE. The project builds fine in VS and using the `dotnet` CLI.

## Usage

When you've followed the links at the top of this README, and you've read the programming guide,
you can use `./run repl` as well as the file `Hopac.fsx` to play around with.

Furthermore, you'll find a large number of examples in (./Examples)[./Examples].

## Release / publish

Build the `Hopac` project and publish the `nupkg` file in `/Libs/Hopac/bin/Release/*.nupkg`.

Update docs
-----------

You need the FsiRefGen git submodule for this. If it’s not already up to date, run:

```
git submodule update --init
```

TODO: Describe commands needed to update docs