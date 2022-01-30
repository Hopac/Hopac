/// <summary>
/// Provides types and type generators for use with FsCheck property tests
/// </summary>
[<AutoOpen>]
module Hopac.Tests.Gens

open FsCheck
open Hopac.Tests


/// Maps an Arbitrary to a Gen, applying a transform
let inline private map f arb =
  arb |> Arb.toGen |> Gen.map (fun g -> f g)

/// Builds a string from a char list
let private stringify (cs: char list) =
  new string (List.toArray cs)


/// Generates bools
let private BoolGen =
  Arb.Default.Bool()
  |> Arb.toGen

/// Generates ordinary floats (not nan, infinity, etc.)
let private SafeFloatGen =
  Arb.Default.NormalFloat()
  |> map (fun nf -> nf.Get)
/// Generates positive floats
let private PositiveFloatGen =
  SafeFloatGen
  |> Gen.map (function
    | f when fNeg f -> -f
    | f when fPos f -> f
    | f -> f + 1.)
/// Generates negative floats
let private NegativeFloatGen =
  SafeFloatGen
  |> Gen.map (function
    | f when fPos f -> -f
    | f when fNeg f -> f
    | f -> f - 1.)
/// Generates non-negative floats
let private NonNegFloatGen =
  SafeFloatGen
  |> Gen.map (function
    | f when fNeg f -> -f
    | f -> f)
/// Generates non-positive floats
let private NonPosFloatGen =
  SafeFloatGen
  |> Gen.map (function
    | f when fPos f -> -f
    | f -> f)

/// Generates positive ints
let private PositiveIntGen =
  Arb.Default.PositiveInt()
  |> map (fun pi -> pi.Get)


/// An ordinary float (not nan, infinity, etc.), generated by FsCheck
type SafeFloat =
  { f : float }
/// A positive float generated by FsCheck
type PosFloat =
  { pf : float }
/// A negative float generated by FsCheck
type NegFloat =
  { nf : float }
/// A non-negative float generated by FsCheck
type NonNegFloat =
  { nnf : float }
/// A non-positive float generated by FsCheck
type NonPosFloat =
  { npf : float }
/// A normalized float in range [0.0-1.0) generated by FsCheck
type NormFloat =
  { Nf : float }

/// An int 1-100 generated by FsCheck
type SmallInt =
  { si : int }
/// An int 1-10 generated by FsCheck (unsized)
type MiniInt =
  { mi : int }

/// Provides numeric generators for FsCheck
type NumericGens =
  /// Generates ordinary floats (not nan, infinity, etc.)
  static member SafeFloat() =
    SafeFloatGen
    |> Gen.map (fun f -> { f = f } )
    |> Arb.fromGen

  /// Generates ordinary positive floats
  static member PosFloat() =
    PositiveFloatGen
    |> Gen.map (fun f -> { pf = f } )
    |> Arb.fromGen

  /// Generates ordinary negative floats
  static member NegFloat() =
    NegativeFloatGen
    |> Gen.map (fun f -> { nf = f } )
    |> Arb.fromGen

  /// Generates ordinary non-negative floats
  static member NonNegFloat() =
    NonNegFloatGen
    |> Gen.map (fun f -> { nnf = f } )
    |> Arb.fromGen

  /// Generates ordinary non-positive floats
  static member NonPosFloat() =
    NonPosFloatGen
    |> Gen.map (fun f -> { npf = f } )
    |> Arb.fromGen

  /// Generates normalized floats in range [0.-1.)
  static member NormFloat() =
    NonNegFloatGen
    |> Gen.resize 100
    |> Gen.map
        (fun f -> (f % 100.) / 100.
        >> fun f -> { Nf = f } )
    |> Arb.fromGen

  /// Generates small positive ints (1-100)
  static member SmallInt() =
    PositiveIntGen
    |> Gen.listOfLength 4
    |> Gen.filter (List.exists (fun i -> i < 100))
    |> Gen.map
        (List.find (fun i -> i < 100)
        >> fun i -> { si = i } )
    |> Arb.fromGen

  /// Generates mini positive ints (1-10); unsized
  static member MiniInt() =
    Gen.choose (1, 10)
    |> Gen.map (fun i -> { mi = i } )
    |> Arb.fromGen


/// A lowercase letter generated by FsCheck
type LowerChar =
  { c : char }
/// An uppercase letter generated by FsCheck
type UpperChar =
  { C : char }
/// A numeric char generated by FsCheck
type NumChar =
  { d : char }
/// An alphabetic char (upper or lowercase) generated by FsCheck
type AlphaChar =
  { ac : char }

/// A string containing only alphabetic chars, generated by FsCheck
type AlphaStr =
  { astr : string }
/// A string containing only alphabetic & numeric chars, generated by FsCheck
type AlphanumStr =
  { ANstr : string }

/// Provides char and string generators for FsCheck
type TextGens =
  /// Generates a lowercase char
  static member LowerChar() =
    Gen.elements
      [ 'a';'b';'c';'d';'e';'f';'g';'h';'i';'j';'k';'l';'m'
        'n';'o';'p';'q';'r';'s';'t';'u';'v';'w';'x';'y';'z' ]
    |> Gen.map (fun c -> { c = c } )
    |> Arb.fromGen

  /// Generates an uppercase char
  static member UpperChar() =
    Gen.elements
      [ 'A';'B';'C';'D';'E';'F';'G';'H';'I';'J';'K';'L';'M'
        'N';'O';'P';'Q';'R';'S';'T';'L';'V';'W';'X';'Y';'Z' ]
    |> Gen.map (fun c -> { C = c } )
    |> Arb.fromGen

  /// Generates a numeric digit char
  static member NumericChar() =
    Gen.elements
      [ '0';'1';'2';'3';'4';'5';'6';'7';'8';'9' ]
    |> Gen.map (fun c -> { d = c } )
    |> Arb.fromGen

  /// Generates an upper or lower alphabetic char
  static member AlphabeticChar() =
    [ TextGens.LowerChar() |> map (fun lo -> lo.c)
      TextGens.UpperChar() |> map (fun up -> up.C) ]
    |> Gen.oneof
    |> Gen.map (fun c -> { ac = c } )
    |> Arb.fromGen

  /// Generates a char that can appear in normal strings
  static member Char() =
    [ TextGens.LowerChar() |> map (fun lo -> lo.c)
      TextGens.UpperChar() |> map (fun up -> up.C)
      TextGens.NumericChar() |> map (fun no -> no.d) ]
    |> Gen.oneof
    |> Arb.fromGen

  /// Generates a non-empty string containing only alphabetic chars
  static member AlphabeticString() =
    TextGens.AlphabeticChar() |> map (fun c -> c.ac)
    |> Gen.nonEmptyListOf
    |> Gen.map (stringify >> fun s -> { astr = s } )
    |> Arb.fromGen

  /// Generates a non-empty alphanumeric string (not null, whitespace, etc.)
  static member AlphanumericString() =
    TextGens.Char() |> Arb.toGen
    |> Gen.nonEmptyListOf
    |> Gen.map (stringify >> fun s -> { ANstr = s } )
    |> Arb.fromGen


/// A list of types that provide FsCheck generators
let genTypes =
    [ typeof<NumericGens>
      typeof<TextGens> ]