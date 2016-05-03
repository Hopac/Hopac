namespace System
open System.Reflection

[<assembly: AssemblyTitleAttribute("Hopac")>]
[<assembly: AssemblyProductAttribute("Hopac")>]
[<assembly: AssemblyDescriptionAttribute("A library for Higher-Order, Parallel, Asynchronous and Concurrent programming in F#.")>]
[<assembly: AssemblyVersionAttribute("0.1.3")>]
[<assembly: AssemblyFileVersionAttribute("0.1.3")>]
[<assembly: AssemblyCompanyAttribute("Housemarque Inc.")>]
[<assembly: AssemblyCopyrightAttribute("� Housemarque Inc.")>]
[<assembly: AssemblyKeyFileAttribute("../../Hopac.snk")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "0.1.3"
    let [<Literal>] InformationalVersion = "0.1.3"
