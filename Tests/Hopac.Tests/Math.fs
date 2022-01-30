/// Simple math functions and operators, for use in tests
[<AutoOpen>]
module Hopac.Tests.Math


/// Laidback float tolerance for tests
let floatTol = 0.00001

/// True if f1 = f2 within float tolerance
let inline ( =. ) (f1: float) (f2: float) =
  abs (f1 - f2) < floatTol

/// True if f1 < f2 beyond float tolerance
let inline ( <. ) (f1: float) (f2: float) =
  f2 - f1 > floatTol

/// True if f1 > f2 beyond float tolerance
let inline ( >. ) (f1: float) (f2: float) =
  f1 - f2 > floatTol

/// True if f < 0 beyond float tolerance
let inline fNeg (f: float) = f < -floatTol

/// True if f > 0 beyond float tolerance
let inline fPos (f: float) = f > floatTol