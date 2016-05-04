@echo off
if not exist packages\FAKE\tools\Fake.exe ( 
  .nuget\nuget.exe install FAKE -OutputDirectory packages -ExcludeVersion -Source "https://nuget.org/api/v2/"
)
if not exist packages\SourceLink.Fake\tools\SourceLink.fsx ( 
  .nuget\nuget.exe install SourceLink.Fake -OutputDirectory packages -ExcludeVersion -Source "https://nuget.org/api/v2/"
)
packages\FAKE\tools\FAKE.exe build.fsx %*