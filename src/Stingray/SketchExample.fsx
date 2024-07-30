#r "nuget: Raylib-CSharp"

open System
open System.Numerics
open Raylib_CSharp
open Raylib_CSharp.Colors
open Raylib_CSharp.Rendering
    
let random = Random()

let sampleGaussian mean stddev (random: Random) =
    let x1 = 1f - random.NextSingle()
    let x2 = 1f - random.NextSingle()
    let y1 =
        MathF.Sqrt(-2f * MathF.Log(x1)) *
        MathF.Cos(2f * MathF.PI * x2)
    y1 * stddev + mean

let setup () =
    Graphics.ClearBackground(Color.RayWhite)
    
let draw () =
    let x =
        random
        |> sampleGaussian 0f 30f
        |> round
        |> int
    let y =
        random
        |> sampleGaussian 0f 30f
        |> round
        |> int
    let g =
        random
        |> sampleGaussian 60f 10f
        |> round
        |> byte
    let b =
        random
        |> sampleGaussian 160f 10f
        |> round
        |> byte
    Graphics.DrawCircle(x, y, 4f, Color(0uy, g, b, 200uy))    
