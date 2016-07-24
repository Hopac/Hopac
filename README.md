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

See the `run` script for other commands.

Note that auto-restore of dependencies has not been enabled due to it taking so
long.  If you want to build from an IDE like Visual Studio or Xamarin, you can
run

```sh
./run restore
```

to restore dependencies.
