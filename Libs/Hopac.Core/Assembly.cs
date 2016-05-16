// Copyright (C) by Housemarque, Inc.

using System.Reflection;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

// Hopac.Core is really only intended to be used by other Hopac libraries.
[assembly: InternalsVisibleTo("Hopac, PublicKey=0024000004800000140100000602000000240000525341310008000011000000934bdab527f7c1b75f64288d92ffc72e2cdba9823589c12f246bcb15f6873124bf501cc1b90c1166881c41b2e871617e6a9e5bd45a95989a9e71b37fd3f6c85f589a1cd70a76ad7f9206bcc96497bef5e73820ca2f304c6c918faf9317f65dcc3fc6b7a8dac0d1c2dfa33df5475fd6a8a1c59be83c3f6a000e6f5590a632ef06a54f25874d96c8eac1754e1c384d120e64242e080da9b4c241e498890e40d36f56ca962adc11a06e3f0906344a6dbdc42ca489ea71ae837616aaf8c5e98d6a26ed7b3cf0d6842ffeb0eb528fc9c006f9cfbd3097564e14ae8491e3ffabeaffa04376b6f53fa56936471c2a0919542984f5e401984cd359cf80d7b3f5f6520689")]
[assembly: InternalsVisibleTo("Hopac.Platform, PublicKey=0024000004800000140100000602000000240000525341310008000011000000934bdab527f7c1b75f64288d92ffc72e2cdba9823589c12f246bcb15f6873124bf501cc1b90c1166881c41b2e871617e6a9e5bd45a95989a9e71b37fd3f6c85f589a1cd70a76ad7f9206bcc96497bef5e73820ca2f304c6c918faf9317f65dcc3fc6b7a8dac0d1c2dfa33df5475fd6a8a1c59be83c3f6a000e6f5590a632ef06a54f25874d96c8eac1754e1c384d120e64242e080da9b4c241e498890e40d36f56ca962adc11a06e3f0906344a6dbdc42ca489ea71ae837616aaf8c5e98d6a26ed7b3cf0d6842ffeb0eb528fc9c006f9cfbd3097564e14ae8491e3ffabeaffa04376b6f53fa56936471c2a0919542984f5e401984cd359cf80d7b3f5f6520689")]

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
