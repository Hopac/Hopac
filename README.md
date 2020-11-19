[Reference](http://hopac.github.io/Hopac/Hopac.html) —
[Guide](./Docs/Programming.md) —
[Docs](./Docs/)

Hopac is a [Concurrent ML](http://cml.cs.uchicago.edu/) style concurrent
programming library for F#.

[![NuGet version](https://badge.fury.io/nu/Hopac.svg)](https://badge.fury.io/nu/Hopac)
[![AppVeyor Build status](https://ci.appveyor.com/api/projects/status/ux9rh6ouhuvu1yia?svg=true)](https://ci.appveyor.com/project/haf/hopac)

## Development

Check out the repo and use your favorite IDE. The project builds fine in VS and using the `dotnet` CLI.

## Usage

When you've followed the links at the top of this README, and you've read the programming guide,
you can use `./run repl` as well as the file `Hopac.fsx` to play around with.

Furthermore, you'll find a large number of examples in (./Examples)[./Examples].

## Release / publish

Build the `Hopac` project and publish the `nupkg` file in `/Libs/Hopac/bin/Release/*.nupkg`. Your commits are tested
on AppVeyor when you send PR:s and push to `master`.

Update docs
-----------

Build documentation and API Reference into `output` folder

```
dotnet tool restore
dotnet fsdocs build
```

Run locally

```
dotnet fsdocs watch
```

TODO: Describe commands needed to push updated docs
