#r @"packages/FAKE/tools/FakeLib.dll"

open Fake
open Fake.Git
open Fake.AssemblyInfoFile
open Fake.ReleaseNotesHelper
open Fake.Paket
open Fake.PaketTemplate
open System.IO
open System

let inline (^) x = x

let project = "Hopac"

let summary = "A library for Higher-Order, Parallel, Asynchronous and Concurrent programming in F#."
let description = "Inspired by languages like Concurrent ML and Cilk, Hopac is a library for F# with the aim of making it easier to write efficient parallel, asynchronous, concurrent and reactive programs. Hopac is licensed under a MIT-style license. See project website for further information."
let authors = ["Housemarque Inc."]
let company = "Housemarque Inc."
let copyright = "© Housemarque Inc."
let tags = "f#, fsharp, parallel, async, concurrent, reactive"

let keyFile = "../../Hopac.snk"

let strongName = Environment.GetEnvironmentVariable "HOPAC_STRONG_NAME" <> null

let release = LoadReleaseNotes "RELEASE_NOTES.md"

type ProjectType = FSharp | CSharp

type Project =
  {Type: ProjectType
   Name: string
   Folder: string
   ProjectFile: string
   StrongName: bool}

let coreProjects =
  ["Hopac.Core"; "Hopac"; "Hopac.Platform.Net"]
  |> List.map ^ fun projectName ->
       let folder = "Libs" @@ projectName
       let projectType, projectFile =
         !! (folder + "/*")
         |> Seq.choose ^ fun file ->
              match Path.GetExtension(file).ToLower() with
               | ".fsproj" -> Some (FSharp, file)
               | ".csproj" -> Some (CSharp, file)
               | _         -> None
         |> Seq.toList
         |> function
             | h::_ -> h
             | _    -> failwithf "Project folder %s does not contain project file." projectName
       {Type = projectType
        Name = projectName
        Folder = folder
        ProjectFile = projectFile
        StrongName = strongName}

Target "AssemblyInfo" ^ fun _ ->
  coreProjects
  |> List.iter ^ fun project ->
       [Attribute.Title project.Name
        Attribute.Product project.Name
        Attribute.Description summary
        Attribute.Version release.AssemblyVersion
        Attribute.FileVersion release.AssemblyVersion
        Attribute.Company company
        Attribute.Copyright copyright]
       |> fun attrs ->
            if project.StrongName
            then Attribute.KeyFile keyFile :: attrs
            else attrs
       |> match project.Type with
           | FSharp -> CreateFSharpAssemblyInfo (project.Folder @@ "AssemblyInfo.fs")
           | CSharp -> Seq.filter ^ fun a -> a.Name <> "KeyFile"
                    >> CreateCSharpAssemblyInfo (project.Folder @@ "AssemblyInfo.cs")

Target "Clean" ^ fun _ ->
  CleanDirs ["bin"]

Target "Build" ^ fun _ ->
  coreProjects
  |> List.map ^ fun project -> project.ProjectFile
  |> MSBuildRelease "bin" "Rebuild"
  |> ignore

Target "PaketBootstrap" ^ fun _ ->
  TimeSpan.MaxValue
  |> ExecProcess ^ fun info ->
       info.FileName <- "./paket.bootstrapper.exe"
       info.WorkingDirectory <- "."
  |> ignore

Target "PaketTemplate" ^ fun _ ->
  PaketTemplate ^ fun p ->
    {p with
       TemplateType = PaketTemplateType.File
       Id = Some ^ if strongName then "Hopac-StrongName" else "Hopac"
       Files = [PaketFileInfo.Include ("bin", "lib/net45")
                PaketFileInfo.Exclude "bin/FSharp.Core*"]
       Authors = authors
       Owners = authors
       Copyright = Some "Copyright 2015"
       Summary = [summary]
       Description = [description]
       Version = Some release.NugetVersion
       ReleaseNotes = release.Notes
       Tags = [tags]
       IconUrl = Some "https://avatars2.githubusercontent.com/u/10173903"
       ProjectUrl = Some "https://github.com/Hopac/Hopac"
       LicenseUrl = Some "https://github.com/Hopac/Hopac/blob/master/LICENSE.md"
       Dependencies = [("FSharp.Core",
                        GreaterOrEqual (PaketDependencyVersion.Version "3.1.2.5"))]
       TemplateFilePath = Some "./Hopac.template"}

Target "Package" ^ fun _ ->
  Pack ^ fun p ->
    {p with
       TemplateFile = "./Hopac.template"
       OutputPath = "nuget"
       ToolPath = "paket.exe"}

Target "BuildPackage" DoNothing

Target "All" DoNothing

"Clean"
  ==> "AssemblyInfo"
  ==> "Build"
  ==> "PaketBootstrap"
  ==> "PaketTemplate"
  ==> "Package"
  ==> "All"

RunTargetOrDefault "All"
