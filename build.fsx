// --------------------------------------------------------------------------------------
// FAKE build script
// --------------------------------------------------------------------------------------

#r @"packages/FAKE/tools/FakeLib.dll"
open Fake
open Fake.Git
open Fake.AssemblyInfoFile
open Fake.ReleaseNotesHelper
open System.IO


// --------------------------------------------------------------------------------------
// START TODO: Provide project-specific details below
// --------------------------------------------------------------------------------------

// Information about the project are used
//  - for version and project name in generated AssemblyInfo file
//  - by the generated NuGet package
//  - to run tests and to publish documentation on GitHub gh-pages
//  - for documentation, you also need to edit info in "docs/tools/generate.fsx"

// The name of the project
// (used by attributes in AssemblyInfo, name of a NuGet package and directory in 'src')
let project = "Hopac"

// Short summary of the project
// (used as description in AssemblyInfo and as a short summary for NuGet package)
let summary = "A library for Higher-Order, Parallel, Asynchronous and Concurrent programming in F#."

// Longer description of the project
// (used as a description for NuGet package; line breaks are automatically cleaned up)
let description = "Inspired by languages like Concurrent ML and Cilk, Hopac is a library for F# with the aim of making it easier to write efficient parallel, asynchronous and concurrent programs. Hopac is licensed under a MIT-style license. See project website for further information."

// List of author names (for NuGet package)
let authors = ["Housemarque Inc."]

let company = "Housemarque Inc."
let copyright = "© Housemarque Inc."

// Tags for your project (for NuGet package)
let tags = "f#, fsharp, parallel, async, concurrent, reactive"

// Pattern specifying assemblies to be tested using NUnit
let testAssemblies = "tests/**/bin/Release/*Tests*.dll"

// Git configuration (used for publishing documentation in gh-pages branch)
// The profile where the project is posted
let gitOwner = "Hopac"
let gitHome = "https://github.com/" + gitOwner

// The name of the project on GitHub
let gitName = "Hopac"

// The url for the raw files hosted
let gitRaw = environVarOrDefault "gitRaw" "https://raw.github.com/Hopac"

// --------------------------------------------------------------------------------------
// END TODO: The rest of the file includes standard build steps
// --------------------------------------------------------------------------------------

let buildDir = "bin"
let nugetDir = "./nuget/"


// Read additional information from the release notes document
let release = LoadReleaseNotes "RELEASE_NOTES.md"

type ProjectType = FSharp | CSharp

type Project = 
    { Type: ProjectType
      Name: string
      Folder: string
      ProjectFile: string }

let projects = 
    ["Hopac.Core"; "Hopac"; "Hopac.Platform.Net"; "Hopac.Extra"]
    |> List.map (fun projectName ->
        let folder = "Libs" @@ projectName
        let projectType, projectFile = 
            !! (folder + "/*")
            |> Seq.choose (fun file -> 
                match  (Path.GetExtension file).ToLower() with
                | ".fsproj" -> Some (FSharp, file)
                | ".csproj" -> Some (CSharp, file)
                | _ -> None)
            |> Seq.toList
            |> function
               | h :: _ -> h
               | _ -> failwithf "Project folder %s does not contain project file." projectName
        { Type = projectType
          Name = projectName
          Folder = folder
          ProjectFile = projectFile })

// Generate assembly info files with the right version & up-to-date information
Target "AssemblyInfo" (fun _ ->
    projects
    |> List.iter (fun project ->
        [ Attribute.Title project.Name
          Attribute.Product project.Name
          Attribute.Description summary
          Attribute.Version release.AssemblyVersion
          Attribute.FileVersion release.AssemblyVersion
          Attribute.Company company
          Attribute.Copyright copyright ]
        |>
        match project.Type with
        | FSharp -> CreateFSharpAssemblyInfo (project.Folder @@ "AssemblyInfo.fs")
        | CSharp -> CreateCSharpAssemblyInfo (project.Folder @@ "AssemblyInfo.cs")
    )
)

// --------------------------------------------------------------------------------------
// Clean build results & restore NuGet packages

Target "Clean" (fun _ ->
    CleanDirs [buildDir; "temp"; nugetDir]
)

// --------------------------------------------------------------------------------------
// Build library & test project

Target "Build" (fun _ ->
    projects
    |> List.map (fun project -> project.ProjectFile)
    |> MSBuildRelease "bin" "Rebuild"
    |> ignore)

// --------------------------------------------------------------------------------------
// Build NuGet packages

Target "NuGet" (fun _ ->
    let nugetlibDir = nugetDir @@ "lib/net45"
    CleanDir nugetlibDir
    CopyDir nugetlibDir "bin" (fun file -> file.Contains "FSharp.Core." |> not)

    NuGet (fun p ->
        { p with
            Authors = authors
            Project = project
            Summary = summary
            Description = description
            Version = release.NugetVersion
            ReleaseNotes = release.Notes |> toLines
            Tags = tags
            OutputPath = nugetDir
            AccessKey = getBuildParamOrDefault "nugetkey" ""
            Publish = hasBuildParam "nugetkey"
            Dependencies = [] })
        (".nuget" @@ project + ".nuspec")
)

Target "Release" (fun _ ->
    StageAll ""
    Git.Commit.Commit "" (sprintf "Bump version to %s" release.NugetVersion)
    Branches.push ""

    Branches.tag "" release.NugetVersion
    Branches.pushTag "" "origin" release.NugetVersion
)

Target "BuildPackage" DoNothing

// --------------------------------------------------------------------------------------
// Run all targets by default. Invoke 'build <Target>' to override

Target "All" DoNothing

"Clean"
  ==> "AssemblyInfo"
  ==> "Build"
  ==> "All"

"All"
  ==> "NuGet"
  ==> "BuildPackage"

"BuildPackage"
  ==> "Release"

RunTargetOrDefault "All"
