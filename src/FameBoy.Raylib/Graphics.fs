module FameBoy.Raylib.Graphics

open FameBoy.Graphics.Ppu
open FameBoy.Hardware
open FameBoy.Memory
open FameBoy.Raylib
open FameBoy.Raylib.RaylibBindings
open FameBoy.Raylib.TileViewer
open FameBoy.Raylib.Utils.RateLimiting
open Raylib_cs

module GraphicsPipeline =
    let private mapSide = 256 // 32 tiles -> 32 * 8 pixels
    let private tilesHeight = 96 // 384 tiles -> 12 lines at 32 tiles per line * 8 pixels

    let mutable private screenTexture =
        Raylib.GenImageColor(Screen.width, Screen.height, Color.Black)
        |> Raylib.LoadTextureFromImage

    let mutable private mapTexture =
        Raylib.GenImageColor(mapSide, mapSide, Color.Black)
        |> Raylib.LoadTextureFromImage

    let mutable private tilesTexture =
        Raylib.GenImageColor(mapSide, tilesHeight, Color.Black)
        |> Raylib.LoadTextureFromImage

    let shades =
        [| Color(186, 218, 85)
           Color(130, 153, 59)
           Color(74, 87, 34)
           Color(19, 22, 8) |]

    let private mapToColors = Array.map (fun (s: Shade) -> shades[int (s)])

    let private backgroundFramebuffer =
        Array.create<Shade> (mapSide * mapSide) Shade.White

    let private tilesFramebuffer =
        Array.create<Shade> (mapSide * tilesHeight) Shade.White

    let private mapPos = (float32 ((Screen.width + 1) * Config.scale), 0f)

    let private tilePos =
        float32 ((Screen.width + 1) * Config.scale), float32 ((mapSide + 1) * Config.scale)

    let private dumpVram =
        rateLimitFunc 1000 (fun memory ->
            dumpBackground backgroundFramebuffer memory
            dumpTiles tilesFramebuffer memory)

    let loadFramebuffer pos texture (framebuffer: Shade array) =
        framebuffer
        |> mapToColors
        |> updateTexture texture
        |> drawScaledTexture pos (float32 Config.scale)

    let loadPpuFramebuffer = loadFramebuffer (0f, 0f) screenTexture
    let loadTilesFramebuffer = loadFramebuffer (0f, 0f) tilesTexture

    let loadDebugFramebuffers (memory: Memory) =
        dumpVram memory

        loadFramebuffer mapPos mapTexture backgroundFramebuffer
        loadFramebuffer tilePos tilesTexture tilesFramebuffer

    let close () =
        Raylib.UnloadTexture screenTexture
        Raylib.UnloadTexture mapTexture
        Raylib.UnloadTexture tilesTexture

        Raylib.CloseWindow()
