#r "paket: groupref Build //"

#nowarn "52"
#load "./.fake/build.fsx/intellisense.fsx"
#load "paket-files/build/eiriktsarpalis/snippets/SlnTools/SlnTools.fs"
 
#if !FAKE
  #r "Facades/netstandard"
#endif

open System
open System.IO
open Fake.Core
open Fake.DotNet
open Fake.IO
open Fake.Tools
open Fake.IO.Globbing.Operators
open Fake.Core.TargetOperators
open Fake.Api
open Fake.BuildServer
open Fake.SystemHelper

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let configuration =
  Environment.environVarOrDefault "CONFIGURATION" "Release"
  |> DotNet.BuildConfiguration.fromString

let release = ReleaseNotes.load "RELEASE_NOTES.md"

let libProjects =
  !! "src/targets/**/*.fsproj"
  ++ "src/adapters/**/*.fsproj"

let testProjects = !! "Tests/AdHocTests/AdHocTests.fsproj"

Target.create "Clean" <| fun _ ->
  // This line actually ensures we get the correct version checked in
  // instead of the one previously bundled with 'fake`
  Git.CommandHelper.gitCommand "" "checkout .paket/Paket.Restore.targets"

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

/// This also restores.
Target.create "Build" <| fun _ ->
  let msbuildParams =
    { MSBuild.CliArguments.Create() with
        Verbosity = Some Quiet
        NoLogo = true
        Properties = [ "Optimize", "true"; "DebugSymbols", "true" ] }
  let setParams (o: DotNet.BuildOptions) =
    { o with
        Configuration = configuration
        MSBuildParams = msbuildParams }
  DotNet.build setParams "src/Hopac.sln"

Target.create "Tests" (fun _ ->
  let commandLine (file: string) =
    let projectName = file.Substring(0, file.Length - ".fsproj".Length) |> Path.GetFileName
    let path = Path.GetDirectoryName file
    sprintf "%s/bin/%O/netcoreapp2.2/%s.dll --summary" path configuration projectName
  testProjects
  |> Seq.iter (commandLine >> DotNet.exec id "" >> ignore))

Target.create "BuildTests" (fun _ ->
  let fws = [ "net471"; "netcoreapp2.0" ]
  build "" fws
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
