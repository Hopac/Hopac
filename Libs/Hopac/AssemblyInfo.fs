namespace System
open System.Reflection

[<assembly: AssemblyTitleAttribute("Hopac")>]
[<assembly: AssemblyProductAttribute("Hopac")>]
[<assembly: AssemblyDescriptionAttribute("A library for Higher-Order, Parallel, Asynchronous and Concurrent programming in F#.")>]
[<assembly: AssemblyVersionAttribute("0.0.0.45")>]
[<assembly: AssemblyFileVersionAttribute("0.0.0.45")>]
[<assembly: AssemblyCompanyAttribute("Housemarque Inc.")>]
[<assembly: AssemblyCopyrightAttribute("� Housemarque Inc.")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "0.0.0.45"
