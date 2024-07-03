#r "nuget: FsUnit.xUnit"

open System
open FsUnit.Xunit
open Xunit

[<Fact>]
let ``Exercise 1.1`` () =
    // This is not 0.5 because the argument to `sin` is in radians.
    sin 30.0
    |> ignore

    let deg2rad x = (x / 180.0) * Math.PI    
    sin <| deg2rad 30.0
    |> should equal 0.5

let ``Exercise 1.2`` () =
    // 2 ^ 3 ^ 4
    ()
