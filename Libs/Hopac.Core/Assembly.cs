// Copyright (C) by Housemarque, Inc.

using System.Reflection;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

// Hopac.Core is really only intended to be used by the main Hopac library.
[assembly: InternalsVisibleTo("Hopac")]

[assembly: AssemblyTitle("Hopac.Core")]
[assembly: AssemblyDescription(Hopac.CommonAssemblyInfo.Description)]
[assembly: AssemblyConfiguration(Hopac.CommonAssemblyInfo.Configuration)]
[assembly: AssemblyCompany(Hopac.CommonAssemblyInfo.Company)]
[assembly: AssemblyProduct(Hopac.CommonAssemblyInfo.Product)]
[assembly: AssemblyCopyright(Hopac.CommonAssemblyInfo.Copyright)]
[assembly: AssemblyTrademark(Hopac.CommonAssemblyInfo.Trademark)]
[assembly: AssemblyCulture(Hopac.CommonAssemblyInfo.Culture)]

[assembly: ComVisible(false)]

[assembly: Guid("9ee1980b-b62e-492a-a5a3-ed65007ced06")]

[assembly: AssemblyVersion(Hopac.CommonAssemblyInfo.Version)]
[assembly: AssemblyFileVersion(Hopac.CommonAssemblyInfo.FileVersion)]

namespace Hopac {
  internal static class CommonAssemblyInfo {
    internal const string Description =
      "A library for Higher-Order, Parallel, Asynchronous and Concurrent programming in F#";
    internal const string Configuration = "";
    internal const string Product = "Hopac";
    internal const string Company = "Housemarque Inc.";
    internal const string Copyright = "© Housemarque Inc.";
    internal const string Version = "0.0.0.0";
    internal const string FileVersion = Version;
    internal const string Trademark = "";
    internal const string Culture = "";
  }
}
