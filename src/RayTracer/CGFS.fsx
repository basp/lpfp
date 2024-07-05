#r "nuget: Raylib-CSharp"

open System.Numerics
open Raylib_CSharp.Images
open Raylib_CSharp.Colors

let normalizeColor color =
    Vector4(color, 1f)
    |> Color.FromNormalized

type Canvas(width, height) =
    let image = Image.GenColor(width, height, Color.Black)
    
    member _.Width = width
    
    member _.Height = height
    
    member _.PutPixel(x, y, color) =
        let sx = width / 2 + x
        let sy = height / 2 - y
        if sx < 0 || sx >= width then ()
        else if sy < 0 || sy >= height then ()
        else image.DrawPixel(sx, sy, normalizeColor color)
    
    member _.Save(path) = image.Export(path)
        
type Viewport = {
    Width: float32
    Height: float32
}

type Camera = {
    Canvas: Canvas
    Viewport: Viewport
    FocalDistance: float32
    PixelDU: float32
    PixelDV: float32
}

let initCamera
    (canvas: Canvas)
    (viewport: Viewport)
    focalDistance
    =
    let pixelDU = viewport.Width / float32 canvas.Width
    let pixelDV = viewport.Height / float32 canvas.Height
    { Camera.Canvas = canvas
      Viewport = viewport
      FocalDistance = focalDistance
      PixelDU = pixelDU
      PixelDV = pixelDV }
    
let canvasToViewport x y (camera: Camera) =
    let vx = x * camera.PixelDU
    let vy = y * camera.PixelDV
    let vz = camera.FocalDistance
    Vector3(vx, vy, vz)