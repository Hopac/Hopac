[ [Reference](http://hopac.github.io/Hopac/Hopac.html) ]

Hopac is a [Concurrent ML](http://cml.cs.uchicago.edu/) style concurrent
programming library for F#.

[![NuGet version](https://badge.fury.io/nu/Hopac.svg)](https://badge.fury.io/nu/Hopac) [![AppVeyor Build status](https://ci.appveyor.com/api/projects/status/srux0s4jy3ahvb84?svg=true)](https://ci.appveyor.com/project/VesaKarvonen/hopac) [![Travis Build Status](https://travis-ci.org/Hopac/Hopac.svg?branch=master)](https://travis-ci.org/Hopac/Hopac)

## Development

Here is a Bash script to get started:

```sh
git clone --recursive https://github.com/Hopac/Hopac.git
cd Hopac
./run tests
```

Note the use of `--recursive` option above.

### `run`

The Bash `run` script provides build automation.  You invoke `run` like any
other Bash command:

```bash
[VARIABLE=value] ./run [COMMAND]
```

Multiple commands can be specified.  For example,

```bash
USE=netcore ./run clean build
```

performs a clean build using the .NET Core stack.

#### `run` variables

* `CONFIGS` specifies the configuration(s) to operate on:
  * `CONFIGS='Debug Release'` is the default to build and run both `Debug` and
    `Release` configs.
  * `CONFIGS=Debug` to build and run only `Debug` config.
  * `CONFIGS=Release` to build and run only `Release` config.

* `USE` specifies the stack, which is auto detected by default, to build and run
  with:
  * `USE=mono` to use Mono.
  * `USE=net` to use .NET Framework (only on Windows).
  * `USE=netcore` to use .NET Core.

#### `run` commands

* `benchmarks` runs all benchmarks.  This may take a *very* long time!
* `build` builds all the subprojects.
* `clean` removes generated files.
* `tests` runs tests.
* `repl` starts the F# interactive with Hopac libs opened.  (Only works with
  Mono and .NET at the moment.)
* `restore` restores dependencies required to build the project.  To build from
  an IDE like Visual Studio or Xamarin, you need to `./run restore` manually.
* `generate_docs` generates the Reference manual under `.gh-pages`.

You can run `source run.complete` to get auto completion of `run` commands in
Bash.
