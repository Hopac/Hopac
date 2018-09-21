#r "paket:
nuget Fake.Core.Xml
nuget Fake.DotNet.Cli
nuget Fake.DotNet.Paket
nuget Fake.Tools.Git
nuget Fake.Api.GitHub
nuget Fake.Core.Target
nuget Fake.Core.Environment
nuget Fake.Core.UserInput
nuget Fake.DotNet.AssemblyInfoFile
nuget Fake.Core.ReleaseNotes //"
#load "./.fake/build.fsx/intellisense.fsx"
#if !FAKE
#r "netstandard"
#endif

open System
open Fake.Core
open Fake.DotNet
open Fake.Tools
open Fake.Api
open Fake.IO
open Fake.Core.TargetOperators
open Fake.IO.Globbing.Operators

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
let configuration = Environment.environVarOrDefault "Configuration" "Release"
let release = ReleaseNotes.load "RELEASE_NOTES.md"
let mutable dotnetExePath = "dotnet"

let exec program args =
  let result = Process.execWithResult (fun info -> { info with FileName = program; Arguments = args }) (TimeSpan.FromMinutes 1.)
  if result.ExitCode <> 0 then
    for error in result.Errors do
      eprintfn "%s" error
    failwith "Failed to execute."


Target.create "Clean" (fun _ ->
  !!"./**/bin/"
  ++ "./**/obj/"
  |> Shell.cleanDirs
  exec "rm" "-rf .gh-pages"
)

let normaliseFileToLFEnding filename =
  let s = File.readAsString filename
  s.Replace(String.WindowsLineBreaks,String.LinuxLineBreaks)
  |> File.writeString false filename

Target.create "AssemblyInfo" (fun _ ->
  !! "Libs/*/*.*proj" |> Seq.iter (fun projectPath ->
    let name, folder = IO.Path.GetFileName projectPath, IO.Path.GetDirectoryName projectPath
    let ext, create =
      if name.EndsWith "fsproj" then
        "fs", AssemblyInfoFile.createFSharpWithConfig
      else
        "cs", AssemblyInfoFile.createCSharpWithConfig
    let config = AssemblyInfoFileConfig(true, false, "Hopac.AssemblyInfo")
    let filePath = sprintf "%s/AssemblyInfo.%s" folder ext
    let attrs =
      [ AssemblyInfo.Title name
        AssemblyInfo.Product name
        AssemblyInfo.Version release.AssemblyVersion
        AssemblyInfo.FileVersion release.AssemblyVersion
      ]
    create filePath attrs config
    normaliseFileToLFEnding filePath
  )
)

Target.create "ProjectVersion" (fun _ ->
  !! "Libs/*/*.*proj" |> Seq.iter (fun projectPath ->
    printfn "Set version on project %s" projectPath
    Xml.pokeInnerText projectPath "Project/PropertyGroup/Version" release.NugetVersion
    normaliseFileToLFEnding projectPath
  )
)
let build project fws =
  for fw in fws do
    DotNet.build (fun p ->
      { p with
          Configuration = DotNet.BuildConfiguration.Custom configuration
          Common = DotNet.Options.withDotNetCliPath dotnetExePath p.Common
                   |> DotNet.Options.withCustomParams (Some "--no-dependencies")
          Framework = Some fw
      })
      project

Target.create "BuildLibs" (fun _ ->
  let fws = [ "net471"; "netstandard2.0" ]
  build "Libs/Hopac.Core/Hopac.Core.csproj" fws
  build "Libs/Hopac.Platform/Hopac.Platform.fsproj" fws
  build "Libs/Hopac/Hopac.fsproj" fws
)

Target.create "BuildTests" (fun _ ->
  let fws = [ "net471"; "netcoreapp2.0" ]
  build "Tests/AdHocTests/AdHocTests.fsproj" fws
)

Target.create "Build" ignore
Target.create "Pack" (fun _ ->
  let exeName = ".paket/paket.exe"
  let arguments = sprintf "pack --version %s --template Libs/Hopac/Hopac.fsproj.template pkg" release.NugetVersion
  let executable, arguments =
    if Environment.isWindows then exeName, arguments
    else "mono", exeName + " " + arguments
  exec executable arguments
)

Target.create "Push" (fun _ ->
  Paket.push (fun p -> { p with WorkingDir = "pkg" })
)

Target.create "Release" (fun _ ->
  let gitOwner = "Hopac"
  let gitName = "Hopac"
  let gitOwnerName = gitOwner + "/" + gitName
  let remote =
      Git.CommandHelper.getGitResult "" "remote -v"
      |> Seq.tryFind (fun s -> s.EndsWith "(push)" && s.Contains gitOwnerName)
      |> function | None -> "ssh://github.com/" + gitOwnerName
                  | Some s -> s.Split().[0]

  Git.Staging.stageAll ""
  Git.Commit.exec "" (sprintf "Bump version to %s" release.NugetVersion)
  Git.Branches.pushBranch "" remote (Git.Information.getBranchName "")

  Git.Branches.tag "" release.NugetVersion
  Git.Branches.pushTag "" remote release.NugetVersion

  Environment.environVar "GITHUB_TOKEN"
  |> GitHub.createClientWithToken
  |> GitHub.draftNewRelease gitOwner gitName release.NugetVersion
                            release.SemVer.PreRelease.IsSome release.Notes
  |> GitHub.publishDraft
  |> Async.RunSynchronously
)

Target.create "Docs" (fun _ ->
  exec "git" "clone -b gh-pages git@github.com:Hopac/Hopac.git .gh-pages"

  let args =
   "--out .gh-pages --name Hopac --icon https://avatars2.githubusercontent.com/u/10173903 "
   + sprintf "--version %O --project-url https://github.com/Hopac/Hopac -- Libs/Hopac/Hopac.fsi Libs/Hopac/Stream.fsi Libs/Hopac/TopLevel.fsi"
             release.SemVer
  exec "./FsiRefGen/run" args
)

Target.create "All" ignore

"Clean"
==> "AssemblyInfo"
==> "ProjectVersion"
==> "BuildLibs"
==> "BuildTests"
==> "Build"
==> "Pack"
==> "All"
==> "Push"
==> "Docs"
==> "Release"

Target.runOrDefaultWithArguments "All"
