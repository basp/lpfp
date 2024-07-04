#r "nuget: Raylib-CSharp"

open System.Numerics
open Raylib_CSharp
open Raylib_CSharp.Colors
open Raylib_CSharp.Images

type Canvas = {
    /// The width of the canvas (in pixels).
    Width: int
    /// The height of the canvas (in pixels).
    Height: int
}

type Viewport = {
    /// The width of the viewport (in world space).
    Width: float32
    /// The height of the viewport (in world space).
    Height: float32
}

type CameraSettings = {
    /// The aspect ratio of the camera.
    AspectRatio: float32
    /// The height of the canvas (in pixels).
    CanvasHeight: int
    /// The height of the viewport (in world space).
    ViewportHeight: float32
    /// The focus distance (in world space).    
    FocusDistance: float32
}

type Camera = {
    /// The aspect ratio of the camera.
    AspectRatio: float32
    /// The dimensions of the canvas (in pixels).
    Canvas: Canvas
    /// The dimensions of the viewport (in world space).
    Viewport: Viewport
    /// The position of the camera (in world space).
    Position: Vector3    
    /// The horizontal stride (in world space) from pixel to pixel.
    PixelDU: Vector3
    /// The vertical stride (in world space) from pixel to pixel.
    PixelDV: Vector3
    /// The center of the upper left pixel (in world space).
    Pixel00: Vector3
}

type Ray = {
    /// The origin (starting point) of the ray (in world space).
    Origin: Vector3
    /// The direction of the ray (in world space).
    Direction: Vector3
}

let initCanvas aspectRatio canvasHeight =
    let canvasWidth =
        let width = float32 canvasHeight * aspectRatio
        match width |> int with
        | x when x < 1 -> 1
        | x -> x
    { Canvas.Width = canvasWidth
      Height = canvasHeight }    

let initViewport (canvas: Canvas) viewportHeight =
    let viewportWidth =
        // The aspect ratio we defined earlier is the *ideal*
        // aspect ratio but due to the integer conversion
        // we want to re-calculate the actual aspect ratio in
        // order to give the viewport the correct dimensions.
        let ar = float32 canvas.Width / float32 canvas.Height
        viewportHeight * ar
    { Viewport.Width = viewportWidth
      Height = viewportHeight }
    
let initCamera (settings: CameraSettings) =
    let canvas = initCanvas settings.AspectRatio settings.CanvasHeight
    let viewport = initViewport canvas settings.ViewportHeight
    let viewportU = Vector3(viewport.Width, 0f, 0f)
    let viewportV = Vector3(0f, -viewport.Height, 0f)
    let pixelDU = viewportU / (float32 canvas.Width)
    let pixelDV = viewportV / (float32 canvas.Height)
    let cameraPosition = Vector3.Zero
    let viewportUpperLeft =
        cameraPosition +
        Vector3(0f, 0f, settings.FocusDistance) -
        viewportU / 2f -
        viewportV / 2f
    let pixel00 = viewportUpperLeft + 0.5f * (pixelDU + pixelDV)
    { Camera.AspectRatio = settings.AspectRatio
      Canvas = canvas
      Viewport = viewport
      Position = cameraPosition
      PixelDU = pixelDU
      PixelDV = pixelDV
      Pixel00 = pixel00 }

let generateRay (x: int) (y: int) camera =
    let pixelCenter =
        camera.Pixel00 +
        (float32 x * camera.PixelDU) +
        (float32 y * camera.PixelDV)
    let rayDirection = pixelCenter - camera.Position
    { Origin = camera.Position
      Direction = rayDirection }
    
let rays camera =
    seq {
        for j in [0..(camera.Canvas.Height - 1)] do
            for i in [0..(camera.Canvas.Width - 1)] do
                generateRay i j camera                
    }
    
let rayColor ray =
    let unitDirection = Vector3.Normalize(ray.Direction)
    let a = 0.5f * (unitDirection.Y + 1f)
    (1f - a) * Vector3.One + a * Vector3(0.5f, 0.7f, 1f)
    
let normalizeColor (v: Vector3) =
    Vector4(v, 1f) |> Color.FromNormalized 
    
let example () =
    let camera =
        let settings =
          { CameraSettings.AspectRatio = 16f / 9f
            CanvasHeight = 200
            ViewportHeight = 2f
            FocusDistance = 1f }
        initCamera settings
    let image =
        let w = camera.Canvas.Width
        let h = camera.Canvas.Height
        Image.GenColor(w, h, Color.Black)
    for j in [0..(camera.Canvas.Height - 1)] do
        for i in [0..(camera.Canvas.Width - 1)] do
            let color =
                camera
                |> generateRay i j
                |> rayColor
                |> normalizeColor
            image.DrawPixel(i, j, color)    
    image
    
example ()
|> _.Export(@"d:\temp\test.png")