module FameBoy.Raylib.RaylibBindings

open System.Numerics
open Raylib_cs

let beginDrawing texture =
    Raylib.BeginDrawing ()

    texture

let endDrawing = Raylib.EndDrawing

let updateTexture texture (frameBuffer: Color array) =
    Raylib.UpdateTexture (texture, frameBuffer)

    texture

let drawScaledTexture scale texture =
    Raylib.DrawTextureEx (texture, Vector2 (0f, 0f), 0f, scale, Color.White)

let windowShouldClose () : bool =
    CBool.op_Implicit (Raylib.WindowShouldClose ())
