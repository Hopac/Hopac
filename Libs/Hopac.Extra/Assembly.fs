namespace Hopac.Extra

open System.Reflection
open System.Runtime.CompilerServices
open System.Runtime.InteropServices

[<AutoOpen>]
module internal AssemblyInfo =
  [<Literal>]
  let Version = "0.0.0.7"

[<assembly: AssemblyTitle("Hopac.Extra")>]
[<assembly: AssemblyDescription("Provides additional utilities for programming with Hopac.")>]
[<assembly: AssemblyConfiguration(Hopac.CommonAssemblyInfo.Configuration)>]
[<assembly: AssemblyCompany(Hopac.CommonAssemblyInfo.Company)>]
[<assembly: AssemblyProduct(Hopac.CommonAssemblyInfo.Product)>]
[<assembly: AssemblyCopyright(Hopac.CommonAssemblyInfo.Copyright)>]
[<assembly: AssemblyTrademark(Hopac.CommonAssemblyInfo.Trademark)>]
[<assembly: AssemblyCulture(Hopac.CommonAssemblyInfo.Culture)>]

[<assembly: ComVisible(false)>]

//[<assembly: Guid("fe5d3c6d-66d0-41eb-bb18-897f8d495fd8")>]

[<assembly: AssemblyVersion(Version)>]
[<assembly: AssemblyFileVersion(Version)>]

()
