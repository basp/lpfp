#r "nuget: Raylib-CSharp"

open System
open System.Numerics

open FSharp.Data.UnitSystems.SI.UnitSymbols

type [<Measure>] px

[<NoComparison; NoEquality>]
type Particle = {
    mutable Position: Vector2
    mutable Velocity: Vector2
    mutable Acceleration: Vector2
    mutable Age: float32
    Lifespan: float32
}

type EmitterMode = Flow of float32 | Explode

type EmitterOpOnEmit<'a> =
    | Static of 'a
    | Callback of (EmitterConfig -> 'a)
    | StartEnd of ('a * 'a)
    | Random of ('a * 'a)
    
and EmitterOpOnUpdate<'a> =
    | Static of 'a
    | Callback of (Particle * float32 * 'a -> 'a)
    | StartEnd of ('a * 'a)   

and EmitterOps<'a> =
    { OnEmit: EmitterOpOnEmit<'a> option
      OnUpdate: EmitterOpOnUpdate<'a> option }

and EmitterConfig = {
    Frequency: float32
    Lifespan: float32
    Gravity: Vector2
    Position: EmitterOps<Vector2>
    Velocity: EmitterOps<Vector2>
    Acceleration: EmitterOps<Vector2>
    MaxVelocity: Vector2
    MaxParticles: int
    MaxAlive: int
}

type Emitter = {
    Config: EmitterConfig
    Free: Particle list
    Alive: Particle list
}

let normalizedAge p = p.Age / p.Lifespan

module Particle =
    let emit config =
        let evalOnEmitOp op =
            match op with
            | EmitterOpOnEmit.Static x -> x
            | EmitterOpOnEmit.Callback f -> f config
        let position =
            config.Position.OnEmit
            |> Option.map evalOnEmitOp
            |> Option.defaultValue Vector2.Zero
        let acceleration =
            config.Acceleration.OnEmit
            |> Option.map evalOnEmitOp
            |> Option.defaultValue Vector2.Zero
        let mutable velocity =
            config.Velocity.OnEmit
            |> Option.map evalOnEmitOp
            |> Option.defaultValue Vector2.Zero
        velocity <- Vector2.Max(velocity, config.MaxVelocity)
        { Particle.Position = position
          Velocity = velocity
          Acceleration = acceleration
          Age = 0f
          Lifespan = config.Lifespan }
