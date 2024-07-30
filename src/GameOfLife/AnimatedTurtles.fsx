#r "nuget: Raylib-CSharp"

open System
open System.Collections.Generic
open System.Numerics
open Raylib_CSharp
open Raylib_CSharp.Camera.Cam2D
open Raylib_CSharp.Colors
open Raylib_CSharp.Rendering
open Raylib_CSharp.Windowing

open FSharp.Data.UnitSystems.SI.UnitSymbols

type [<Measure>] px

type [<Measure>] rad

type Line =
    { From: Vector2
      To: Vector2 }
    
type Turn =
    { Direction: Vector2
      Angle: float32 }

type AnimationKind =
    | Line of Line
    | Turn of Turn
    
type Animation = {
    Type: AnimationKind
    mutable Age: float32
    Lifespan: float32 }

let moveSpeed = 30f<px/s>

let turnSpeed = LanguagePrimitives.Float32WithMeasure<rad/s> MathF.PI

let drawLine (a: Vector2) (b: Vector2) =
    let x0 = int <| round a.X
    let y0 = int <| round a.Y
    let x1 = int <| round b.X
    let y1 = int <| round b.Y
    Graphics.DrawLine(x0, y0, x1, y1, Color.White)

let drawTurtle (pos: Vector2) dir =
    let drawTriangle () =
        let scale = 10f
        let d = scale * (Vector2.Normalize(dir))
        let m = Matrix3x2.CreateRotation(0.5f * MathF.PI)
        let u = Vector2.Transform(d, m)
        let u = MathF.Tan(0.45f) * u
        let a = pos - u
        let b = pos + d
        let c = pos + u
        drawLine a b
        drawLine c b
        drawLine c a
    let drawCircle () =
        let x = int <| round pos.X
        let y = int <| round pos.Y
        let radius = 2.5f
        Graphics.DrawCircle(x, y, radius, Color.Red)
    drawTriangle ()

let turn1 =
    { Turn.Direction = Vector2(5f, 0f)
      Angle = MathF.PI / 2f }

let turn2 =
    { Turn.Direction = Vector2(0f, 5f)
      Angle = MathF.PI / 2f }

let turn3 =
    { Turn.Direction = Vector2(-5f, 0f)
      Angle = MathF.PI / 2f }

let line1 =
    { Line.From = Vector2(-50f, -50f)
      To = Vector2(50f, -50f) }
    
let line2 =
    { Line.From = Vector2(50f, -50f)
      To = Vector2(50f, 50f) }

let line3 =
    { Line.From = Vector2(50f, 50f)
      To = Vector2(-50f, 50f) }
    
let line4 =
    { Line.From = Vector2(-50f, 50f)
      To = Vector2(-50f, -50f) }
    
let anims = Queue<Animation>[
    { Type = Line(line1)
      Age = 0f
      Lifespan = 1f }
    { Type = Turn(turn1)
      Age = 0f
      Lifespan = 0.25f }
    { Type = Line(line2)
      Age = 0f
      Lifespan = 1f }
    { Type = Turn(turn2)
      Age = 0f
      Lifespan = 0.25f }
    { Type = Line(line3)
      Age = 0f
      Lifespan = 1f }
    { Type = Turn(turn3)
      Age = 0f
      Lifespan = 0.25f }
    { Type = Line(line4)
      Age = 0f
      Lifespan = 1f }
]
    
let lines = List<Line>()
    
let test1 () =
    let (width, height) = 640, 320
    
    let halfWidth = float32 width / 2f
    let halfHeight = float32 height / 2f
    
    let camera =
        let offset = Vector2(halfWidth, halfHeight)
        let target = Vector2.Zero
        Camera2D(offset, target, 0f, 1.5f)

    let mutable current: Animation option = None
    let mutable pos: Vector2 = Vector2.Zero
    let mutable dir: Vector2 = Vector2.Zero
            
    Window.Init(width, height, "Particles")
    Time.SetTargetFPS(60)

    while (not <| Window.ShouldClose()) do
        // Update
        let dt = Time.GetFrameTime()
        match current with
        | Some anim ->
            anim.Age <- anim.Age + dt
            if anim.Age >= anim.Lifespan then
                match anim.Type with
                | Line line -> lines.Add(line)
                | Turn _ -> ()
                current <- None
        | None ->
            current <- 
                match anims.TryDequeue() with
                | true, anim -> Some anim
                | _ -> None
            
        // Draw
        Graphics.BeginDrawing()
        do
            Graphics.ClearBackground(Color.Black)
            Graphics.BeginMode2D(camera)
            match current with
            | Some anim ->
                let t = anim.Age / anim.Lifespan
                match anim.Type with
                | Line x when t < 1f ->
                    pos <- Vector2.Lerp(x.From, x.To, t)
                    dir <- pos - x.From
                    drawTurtle pos dir
                    drawLine x.From pos
                | Line x when t >= 1f ->
                    drawTurtle pos dir
                    lines.Add(x)
                | Turn x when t < 1f ->
                    let m = Matrix3x2.CreateRotation(x.Angle * t)
                    dir <- Vector2.Transform(x.Direction, m)
                    drawTurtle pos dir
                | Turn _ when t >= 1f -> ()
                | _ -> ()
            | None ->
                // Not drawing the turtle here causes flickering.
                drawTurtle pos dir
            for line in lines do drawLine line.From line.To
            Graphics.EndMode2D()
        Graphics.EndDrawing()
        
    Window.Close()

test1 ()