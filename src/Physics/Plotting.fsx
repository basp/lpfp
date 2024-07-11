#r "nuget: XPlot.Plotly"

open System
open XPlot.Plotly

let integral dt f a b =
    let tMin = a + dt / 2.0
    let tMax = b - dt / 2.0
    [tMin..dt..tMax]
    |> List.map (fun t -> f t * dt)
    |> List.sum

let antiDerivative dt v0 f t =
    v0 + integral dt f 0.0 t

let example () =
    let pi = Math.PI
    // let f = cos
    // let f x = x**2.0
    let f x = 1.0 / x
    let f' x = -1.0 * (x**(-2.0))
    // let x = [0.0..(pi / 360.0)..(2.0 * pi)]
    let x = [1.0..0.1..5.0]
    let trace0 =
        Scatter(
            x = x,
            y = List.map f x
        )
    let trace1 =
        Scatter(
            x = x,
            y = List.map f' x
        )
    [trace0; trace1]
    |> Chart.Plot
    |> Chart.Show
    
example ()