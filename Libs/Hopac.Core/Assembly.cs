// Copyright (C) by Housemarque, Inc.

using System.Reflection;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

// Hopac.Core is really only intended to be used by other Hopac libraries.
[assembly: InternalsVisibleTo("Hopac")]
[assembly: InternalsVisibleTo("Hopac.Platform")]

namespace Hopac
{
#pragma warning disable 1591 // Missing XML comment

    public static class CommonAssemblyInfo
    {
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
