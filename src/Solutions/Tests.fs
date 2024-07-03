module Tests

open System
open FSharp.Data.UnitSystems.SI.UnitSymbols
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
    
let ``Exercise 1.5`` () =
    ()

let ``Exercise 1.6`` () =
    ()

[<Fact>]
let ``Exercise 2.1`` () =
    let tolerance = 1e-3
    let f x = sqrt <| 1.0 + x     
    f 0.0 |> should equal 1.0
    f 1.0 |> should (equalWithin tolerance) 1.414
    f 3.0 |> should equal 2.0 

let ``Exercise 2.2`` () =
    let yRock30 (t: float<s>) =
        t * 30.0<m/s>
    ()
    
let ``Exercise 2.3`` () =
    let vRock30 (t: float<s>) =
        let g = 9.8067<m/s^2>
        30.0<m/s> - t * g
    ()
    
[<Fact>]
let ``Exercise 2.4`` () =
    let sinDeg x = sin <| (x / 180.0) * Math.PI
    (sinDeg 30.0) |> should equal (sin <| Math.PI / 6.0)
    
let ``Exercise 2.5`` () =
    ()
    
let ``Exercise 2.6`` () =
    ()
    
let ``Exercise 3.1`` () =
    ()
    
[<Fact>]
let ``Exercise 3.2`` () =
    let f x = if x <= 0.0 then 0.0 else x
    let E r = if r <= 1.0 then r else (1.0 / (r**2))
    ()
    
[<Fact>]
let ``Exercise 3.3`` () =
    let isXorY c =
        match c with
        | 'X' -> true
        | 'Y' -> true
        | _ -> false
    (isXorY 'X') |> should equal true
    (isXorY 'Y') |> should equal true
    (isXorY 'Z') |> should equal false
    
[<Fact>]
let ``Exercise 3.4`` () =
    let bagFee check = if check then 100 else 0
    let bagFee2 = function
        | true -> 100
        | false -> 0
    (bagFee true) |> should equal 100
    (bagFee false) |> should equal 0
    (bagFee true) |> should equal (bagFee2 true)
    (bagFee false) |> should equal (bagFee2 false)

[<Fact>]
let ``Exercise 3.5`` () =
    let greaterThan50 x = x > 50
    (greaterThan50 50) |> should equal false
    (greaterThan50 51) |> should equal true
    
[<Fact>]
let ``Exercise 3.6`` () =
    let amazingCurve x = min (x * 2) 100
    (amazingCurve 50) |> should equal 100
    (amazingCurve 51) |> should equal 100
    (amazingCurve 49) |> should equal 98
    
let ``Exercise 3.7`` () =
    ()

let ``Exercise 3.8`` () =
    ()

let ``Exercise 3.9`` () =
    ()
    
let ``Exercise 3.10`` () =
    ()