module Tests

open System
open FsUnit.Xunit
open Xunit

[<Fact>]
let ``Exercise 1.1`` () =
    // This is not 0.5 because the argument to `sin` is in radians.
    sin 30.0
    |> ignore

    // Helper to convert from degrees to radians.
    let deg2rad x = (x / 180.0) * Math.PI

    // If we convert to radians first we get the expected outcome.
    let tolerance = 1e-15
    (sin <| deg2rad 30.0)
    |> should (equalWithin tolerance) 0.5

[<Fact>]
let ``Exercise 1.2`` () =
    // (a) 2 ^ 3 ^ 4
    // F# does not have an equivalent to the Haskell `^` operator.
    // The exponentiation operator is right associative.
    (2.0 ** 3.0 ** 4.0)
    |> should equal (2.0 ** (3.0 ** 4.0))
    
    // (b) 2 / 3 / 4
    // The division operator in F# is left associative.
    (2.0 / 3.0 / 4.0)
    |> should equal ((2.0 / 3.0) / 4.0)

    // (c) 7 - 5 / 4
    // Like in most programming languages, division precedes subtraction.
    (7.0 - 5.0 / 4.0)
    |> should equal (7.0 - (5.0 / 4.0))
    
    // (d) log 49/7
    // Function application has the highest precedence.
    log 49.0 / 7.0
    |> should equal ((log 49.0) / 7.0)
    
[<Fact>]
let ``Exercise 1.3`` () =
    // ghci> logBase 2 32 == 5.0
    Math.Log2 32
    |> should equal 5.0
    
[<Fact>]
let ``Exercise 1.4`` () =
    // Converts from cartesian to polar coordinates.
    let toPolar x y =
        let r = sqrt (x**2.0 + y**2.0)
        let phi = atan2 y x
        (r, phi)        
    
    // Converts from polar to cartesian coordinates.
    let fromPolar r phi =
        let x = r * (cos phi)
        let y = r * (sin phi)
        (x, y)
    
    let x, y = -3.0, 4.0
    
    // With the two functions to convert between cartesian and polar
    // coordinates we first convert to polar.
    let r, phi = toPolar x y
    
    // Next we convert the polar coordinates back to cartesian.
    let x', y' = fromPolar r phi

    // With a little bit of tolerance, the converted coordinates should equal
    // the original ones.
    let tolerance = 1e-15
    x |> should (equalWithin tolerance) x'
    y |> should (equalWithin tolerance) y'
    
[<Fact>]
let ``Exercise 1.5`` () =
    ()

[<Fact>]
let ``Exercise 1.6`` () =
    ()
