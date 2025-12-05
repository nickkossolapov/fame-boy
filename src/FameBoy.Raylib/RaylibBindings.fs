module FameBoy.Raylib.RaylibBindings

open System.Numerics
open Raylib_cs

let beginDrawing = Raylib.BeginDrawing

let endDrawing = Raylib.EndDrawing

let updateTexture texture (frameBuffer: Color array) =
    Raylib.UpdateTexture (texture, frameBuffer)

    texture

let drawScaledTexture (x, y) scale texture =
    Raylib.DrawTextureEx (texture, Vector2 (x, y), 0f, scale, Color.White)
    
let isKeyDown keyValue : bool =
    CBool.op_Implicit (Raylib.IsKeyDown keyValue)

let windowShouldClose () : bool =
    CBool.op_Implicit (Raylib.WindowShouldClose ())
