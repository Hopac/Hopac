dotnet build Tests/Hopac.Tests/Hopac.Tests.fsproj -o Tests/Hopac.Tests/bin -c Release
dotnet Tests/Hopac.Tests/bin/Hopac.Tests.dll --join-with "/"