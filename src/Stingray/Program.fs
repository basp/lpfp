open System
open System.Numerics

open Raylib_CSharp

module Noise =
    let permutations () =
        let random = Random()
        Array.init 256 (fun _ -> random.Next(255))
    
    let lerp a b t = RayMath.Lerp(a, b, t)
    
    let smoothstep a = a * a * (3f - 2f * a)
    
    let smootherstep a =
        let a3 = a * a * a
        let a4 = a3 * a
        let a5 = a4 * a
        (6f * a5) - (15f * a4) + (10f * a3)

module Program =
    
    [<EntryPoint>]
    let main argv =
        0
