#load "Seq.fs"

// Derivative
// (float -> float) -> float -> float

let derivative dt f t =
    let f0 = f (t + dt / 2.0)
    let f1 = f (t - dt / 2.0)
    (f0 - f1) / dt

// Integration
// (float -> float) -> float -> float -> float

let integral dt f a b =
    let tMin = a + dt / 2.0
    let tMax = b - dt / 2.0
    [tMin..dt..tMax]
    |> List.map (fun t -> f t * dt)
    |> List.sum

let integralN n f a b =
    let dt = (b - a) / float n
    integral dt f a b

let antiDerivative dt v0 f t =
    v0 + integral dt f 0.0 t

let linspace start stop n =
    let delta = (stop - start) / float (n - 1)
    let mutable current = start
    seq {
        for _ = 0 to (n - 1) do
            yield current
            current <- current + delta
    }

let trapz n (f: float -> float) a b =
    let x = linspace a b (n + 1)
    let y = Seq.map f x |> Seq.toList
    let right = y[1..]
    let left = y[..(n - 1)]
    let dx = (b - a) / float n
    List.concat [left; right]
    |> List.sum
    |> fun t -> t * (dx / 2.0)
    