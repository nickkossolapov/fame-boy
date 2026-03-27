module FameBoy.Raylib.RaylibBindings

open System.Numerics
open Microsoft.FSharp.NativeInterop
open Raylib_cs


#nowarn "9" // suppress native interop warning for fixed and NativePtr.toVoidPtr

let beginDrawing = Raylib.BeginDrawing

let endDrawing = Raylib.EndDrawing

let updateTexture texture (frameBuffer: Color array) =
    Raylib.UpdateTexture(texture, frameBuffer)

    texture

let drawScaledTexture (x, y) scale texture =
    Raylib.DrawTextureEx(texture, Vector2(x, y), 0f, scale, Color.White)

let isKeyDown keyValue : bool =
    Raylib.IsKeyDown keyValue |> CBool.op_Implicit

let windowShouldClose () : bool =
    Raylib.WindowShouldClose() |> CBool.op_Implicit

let isAudioStreamProcessed stream : bool =
    Raylib.IsAudioStreamProcessed stream |> CBool.op_Implicit

let updateAudioStream stream (buffer: float32 array) =
    use pinned = fixed buffer

    Raylib.UpdateAudioStream(stream, NativePtr.toVoidPtr pinned, buffer.Length)
