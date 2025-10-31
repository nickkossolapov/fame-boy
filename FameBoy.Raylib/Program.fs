open FameBoy.Constants
open FameBoy.Raylib.RaylibBindings
open Raylib_cs

let scale = 4

Raylib.InitWindow (Screen.width * scale, Screen.height * scale, "Fame Boy")
Raylib.SetTargetFPS 60

let mutable screenTexture =
    Raylib.GenImageColor (Screen.width, Screen.height, Color.Black)
    |> Raylib.LoadTextureFromImage

let mapToColors = Array.map (fun _ -> Color (106, 183, 133))

let drawTexture = drawScaledTexture (float32 scale)

let draw (framebuffer: bool array) =
    framebuffer
    |> mapToColors
    |> beginDrawing
    |> updateTexture screenTexture
    |> drawTexture
    |> endDrawing

let testFramebuffer = Array.zeroCreate<bool> (Screen.width * Screen.height)

while (not (windowShouldClose ())) do
    draw testFramebuffer

Raylib.UnloadTexture screenTexture
Raylib.CloseWindow ()
