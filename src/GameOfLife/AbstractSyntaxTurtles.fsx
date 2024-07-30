open System
open System.Numerics

type [<Measure>] rad

type [<Measure>] deg

type [<Measure>] px

type Angle =
    | Radians of float32<rad>
    | Degrees of float32<deg>

module Turtle =
    type PenState = Up | Down
    
    type State =
        { Position: Vector2
          Angle: Angle
          Pen: PenState
          Color: Vector4 }

    type Command =
        | Move of float32
        | Turn of Angle
    
    let deg2rad = MathF.PI / 180f<deg/rad>

    let initDefault () =
        { Position = Vector2.Zero
          Angle = Radians(0f<rad>)
          Pen = Down
          Color = Vector4.One }
        
    let init pos angle =
        { initDefault () with
            Position = pos
            Angle = angle }
    
    let updatePosition distance (angle: Angle) (pos: Vector2) =
        let alpha =
            match angle with
            | Radians x -> x
            | Degrees x -> x * deg2rad
        let x = pos.X + distance * (MathF.Cos <| float32 alpha)
        let y = pos.Y + distance * (MathF.Sin <| float32 alpha)
        Vector2(x, y)
    
    let move distance state =
        let pos = updatePosition distance state.Angle state.Position
        { state with Position = pos }
        
    let turn angle state =
        let alpha =
            match state.Angle with
            | Radians x ->
                match angle with
                | Radians y -> Radians(x + y)
                | Degrees y -> Radians(x + y * deg2rad)
            | Degrees x ->
                match angle with
                | Radians y -> Radians(x * deg2rad + y)
                | Degrees y -> Radians(x * deg2rad + y * deg2rad)
        { state with Angle = alpha }           

    let applyCommand command state =
        match command with
        | Move distance -> move distance state
        | Turn angle -> turn angle state
        
module Rendering =
    do
        ()