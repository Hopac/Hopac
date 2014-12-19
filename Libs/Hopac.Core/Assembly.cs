// Copyright (C) by Housemarque, Inc.

using System.Reflection;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

// Hopac.Core is really only intended to be used by other Hopac libraries.
[assembly: InternalsVisibleTo("Hopac")]
[assembly: InternalsVisibleTo("Hopac.Platform")]

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
#pragma warning disable 1591 // Missing XML comment

  public static class CommonAssemblyInfo {
    public const string Description =
      "A library for Higher-Order, Parallel, Asynchronous and Concurrent programming in F#.";
    public const string Configuration = "";
    public const string Product = "Hopac";
    public const string Company = "Housemarque Inc.";
    public const string Copyright = "© Housemarque Inc.";
    public const string Version = "0.0.0.35";
    public const string FileVersion = Version;
    public const string Trademark = "";
    public const string Culture = "";
  }
}
