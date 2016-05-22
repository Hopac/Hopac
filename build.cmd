@echo off
paket.bootstrapper.exe

if not exist packages\FAKE\tools\Fake.exe ( 
  paket.exe install
)
packages\FAKE\tools\FAKE.exe build.fsx %*

REM Test
msbuild /verbosity:quiet Hopac.sln
Tests\AdHocTests\bin\Debug\AdHocTests.exe
