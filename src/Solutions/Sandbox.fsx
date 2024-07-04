open FSharp.Data.UnitSystems.SI.UnitSymbols

let derivative
    (dt: float<'a>)
    (x: float<'a> -> float<'b>)
    =
    fun t ->
        let x0 = x (t - dt / 2.0)
        let x1 = x (t + dt / 2.0)
        (x1 - x0) / dt
       
module Seq =
    let iterate f s =
        seq {
            let mutable current = s
            while true do
                yield current                
                current <- f current
        }
        
    let cycle f =
        
        ()
        
    let iterateExample () =
        iterate (fun x -> 2 * x) 1
        |> Seq.take 10
        |> Seq.toList
       
module Example1 =
    let position (t: float<s>) =
        cos (t * 1.0<1/s>)
        |> LanguagePrimitives.FloatWithMeasure<m>

    let velocity = derivative 0.01<s> position

    let acceleration = derivative 0.01<s> velocity
    
Seq.iterateExample ()