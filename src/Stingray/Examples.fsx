#r "nuget: SharpNoise"
#r "nuget: SharpNoise.Utilities"
#r "nuget: Raylib-CSharp"

open System
open System.Drawing.Imaging
open System.IO
open System.Numerics

open Raylib_CSharp
open Raylib_CSharp.Colors
open Raylib_CSharp.Images

open Raylib_CSharp.Rendering
open Raylib_CSharp.Windowing
open SharpNoise
open SharpNoise.Builders
open SharpNoise.Modules
open SharpNoise.Utilities.Imaging

type RaylibColor = Raylib_CSharp.Colors.Color
    
type SketchConfig = {
    Width: int
    Height: int
    Title: string
    Setup: unit -> unit
    Draw: float32 -> unit
}    

let defaultSketchConfig = {
    Width = 640
    Height = 320
    Title = "Unnamed sketch"
    Setup = fun () -> ()
    Draw = fun _ -> ()
}

let sketch config =
    Window.Init(config.Width, config.Height, config.Title)
    Time.SetTargetFPS(60)    
    config.Setup ()    
    while (not <| Window.ShouldClose()) do
        let dt = Time.GetFrameTime()
        Graphics.BeginDrawing()
        config.Draw dt
        Graphics.EndDrawing()
    Window.Close()

let example1 () =
    let perlin = Perlin()
    let image = Image.GenColor(640, 240, Color.Black)
    let mutable yOffset = 0.0
    for j in [0 .. (image.Height - 1)] do
        let mutable xOffset = 0.0
        for i in [0 .. (image.Width - 1)] do
            let n =
                perlin.GetValue(yOffset, xOffset, 0.0)
                |> float32
                |> fun v -> RayMath.Clamp(v, -1f, 1f)
                |> fun v -> RayMath.Remap(v, -1f, 1f, 0f, 255f)
                |> round
                |> byte
            let color = RaylibColor(n, n, n, 255uy)
            image.DrawPixel(i, j, color)
            xOffset <- xOffset + 0.01
        yOffset <- yOffset + 0.01
    image.Export(@"d:\temp\test1.png")

let example2 () =
    let source = Perlin()
    let noiseMap = NoiseMap()
    let builder = PlaneNoiseMapBuilder()
    builder.DestNoiseMap <- noiseMap
    builder.SourceModule <- source
    builder.SetDestSize(640, 240)
    builder.SetBounds(-3.0, 3.0, -2.0, 2.0)
    builder.Build()
    let image = Image()
    let renderer = ImageRenderer()
    renderer.SourceNoiseMap <- noiseMap
    renderer.DestinationImage <- image
    renderer.BuildGrayscaleGradient()
    renderer.Render()
    use fs = File.OpenWrite(@"d:\temp\test2.png")
    image.SaveGdiBitmap(fs, ImageFormat.Png)

let example3 () =
    let screenWidth, screenHeight = 640, 240

    Window.Init(screenWidth, screenHeight, "Test")
    Time.SetTargetFPS(60)   
    
    let mutable x = 100f
    let mutable y = 100f
    
    let mutable xSpeed = 2.5f
    let mutable ySpeed = 2f
    
    while (not <| Window.ShouldClose()) do
        Graphics.BeginDrawing()
        do
            Graphics.ClearBackground(Color.White)
            x <- x + xSpeed
            y <- y + ySpeed
            if x > float32 screenWidth || x < 0f then
                xSpeed <- xSpeed * -1f
            if y > float32 screenHeight || y < 0f then
                ySpeed <- ySpeed * -1f
            let stroke = RaylibColor(0uy, 0uy, 0uy, 255uy)
            let fill = RaylibColor(127uy, 127uy, 127uy, 255uy)
            Graphics.DrawCircle(int x, int y, 48f, stroke)
            Graphics.DrawCircle(int x, int y, 44f, fill)            
        Graphics.EndDrawing()        
    Window.Close()
    
let example4 () =
    let mutable x = 100f
    let mutable y = 100f
    
    let mutable xSpeed = 2.5f
    let mutable ySpeed = 2f
    
    let width = 640
    let height = 240
    
    let draw _ =
        Graphics.ClearBackground(Color.White)
        x <- x + xSpeed
        y <- y + ySpeed
        if x > float32 width || x < 0f then xSpeed <- xSpeed * -1f
        if y > float32 height || y < 0f then ySpeed <- ySpeed * -1f
        let stroke = RaylibColor(0uy, 0uy, 0uy, 255uy)
        let fill = RaylibColor(127uy, 127uy, 127uy, 255uy)
        Graphics.DrawCircle(int x, int y, 48f, stroke)
        Graphics.DrawCircle(int x, int y, 44f, fill)

    let setup () = ()
    
    let config =
          { Width = width
            Height = height
            Title = "Sketch test"
            Draw = draw
            Setup = setup }
          
    sketch config
    
example4 ()