namespace Hopac.Experimental

open System.Reflection
open System.Runtime.CompilerServices
open System.Runtime.InteropServices

[<AutoOpen>]
module internal AssemblyInfo =
  [<Literal>]
  let Version = "0.0.0.0"

[<assembly: AssemblyTitle("Hopac.Experimental")>]
[<assembly: AssemblyDescription("Experimental utilities for programming with Hopac.")>]
[<assembly: AssemblyConfiguration(Hopac.CommonAssemblyInfo.Configuration)>]
[<assembly: AssemblyCompany(Hopac.CommonAssemblyInfo.Company)>]
[<assembly: AssemblyProduct(Hopac.CommonAssemblyInfo.Product)>]
[<assembly: AssemblyCopyright(Hopac.CommonAssemblyInfo.Copyright)>]
[<assembly: AssemblyTrademark(Hopac.CommonAssemblyInfo.Trademark)>]
[<assembly: AssemblyCulture(Hopac.CommonAssemblyInfo.Culture)>]

[<assembly: ComVisible(false)>]

//[<assembly: Guid("283dfc3e-4d39-404e-89f2-e45daf6b6890")>]

[<assembly: AssemblyVersion(Version)>]
[<assembly: AssemblyFileVersion(Version)>]

()
