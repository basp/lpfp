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

type [<Measure>] px;

type Line =
    { From: Vector2
      To: Vector2
      mutable Age: float32
      Lifetime: float32 }

module Line =
    let init a b =
        let lifetime = Vector2.Distance(a, b) / 60f<px/s>
        { From = a; To = b; Age = 0f; Lifetime = float32 lifetime }

let test1 () =
    let (width, height) = 640, 320
    let halfWidth = float32 width / 2f
    let halfHeight = float32 height / 2f    
    let camera =
        let offset = Vector2(halfWidth, halfHeight)
        let target = Vector2.Zero
        Camera2D(offset, target, 0f, 1f)
       
    let mutable todo = Queue<Line>()
    todo.Enqueue <| Line.init (Vector2(-50f, -50f)) (Vector2(50f, -50f))
    todo.Enqueue <| Line.init (Vector2(50f, -50f)) (Vector2(50f, 50f))
    todo.Enqueue <| Line.init (Vector2(50f, 50f)) (Vector2(-50f, 50f))
    todo.Enqueue <| Line.init (Vector2(-50f, 50f)) (Vector2(-50f, -50f)) 

    let mutable ``done`` = List<Line>()        
    let mutable current: Line option = None
    
    Window.Init(width, height, "Particles")
    Time.SetTargetFPS(60)
    
    let drawLine (a: Vector2) (b: Vector2) =
        let x0 = int <| round a.X
        let y0 = int <| round a.Y
        let x1 = int <| round b.X
        let y1 = int <| round b.Y
        Graphics.DrawLine(x0, y0, x1, y1, Color.White)    
    
    while (not <| Window.ShouldClose()) do
        // Update
        let dt = Time.GetFrameTime()
        match current with
        | Some x ->
            x.Age <- x.Age + dt
            if x.Age >= x.Lifetime then
                ``done``.Add(x)
                current <- None
        | None -> 
            current <-
                match todo.TryDequeue() with
                | (true, line) -> Some line
                | _ -> None
                
        // Draw
        Graphics.BeginDrawing()
        do
            Graphics.ClearBackground(Color.Black)
            Graphics.BeginMode2D(camera)
            do
                for line in ``done`` do
                    drawLine line.From line.To                
                match current with
                | Some line ->                    
                    let t = line.Age / line.Lifetime
                    if t < 1f then
                        let ``to`` = Vector2.Lerp(line.From, line.To, t)
                        drawLine line.From ``to``
                | None -> ()
            Graphics.EndMode2D()
        Graphics.EndDrawing()
        
    Window.Close()
    
test1 ()