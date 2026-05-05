module FameBoy.Raylib.Graphics

open FameBoy.Ppu
open FameBoy.Hardware
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

    let mutable private screenTexture2 =
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

    let private mapCgbColors (framebuffer: FameBoy.Ppu.Color array) =
        framebuffer |> Array.map (fun c -> Color(int c.R, int c.G, int c.B, 255))

    let private backgroundFramebuffer =
        Array.create<Shade> (mapSide * mapSide) Shade.White

    let private tilesFramebuffer =
        Array.create<Shade> (mapSide * tilesHeight) Shade.White

    let private mapPos () =
        (float32 ((Screen.width + 1) * Config.scale), 0f)

    let private tilePos () =
        float32 ((Screen.width + 1) * Config.scale), float32 ((mapSide + 1) * Config.scale)

    let private dumpVram =
        rateLimitFunc 1000 (fun ppu ->
            dumpBackground backgroundFramebuffer ppu
            dumpTiles tilesFramebuffer ppu)

    /// Calculate destination rect for rendering with aspect ratio preservation
    let private getScreenRect () =
        let screenW = float32 (Raylib.GetScreenWidth())
        let screenH = float32 (Raylib.GetScreenHeight())
        let gameW = float32 Screen.width
        let gameH = float32 Screen.height

        if Config.fullscreen && not Config.linkMode then
            let scaleX = screenW / gameW
            let scaleY = screenH / gameH
            let scale = min scaleX scaleY
            let dstW = gameW * scale
            let dstH = gameH * scale
            let offsetX = (screenW - dstW) / 2f
            let offsetY = (screenH - dstH) / 2f
            (offsetX, offsetY, dstW, dstH)
        else
            (0f, 0f, gameW * float32 Config.scale, gameH * float32 Config.scale)

    /// Calculate dest rects for link mode (two side-by-side screens)
    let private getLinkScreenRects () =
        let screenW = float32 (Raylib.GetScreenWidth())
        let screenH = float32 (Raylib.GetScreenHeight())
        let gameW = float32 Screen.width
        let gameH = float32 Screen.height
        let gap = 4f // pixel gap between screens

        if Config.fullscreen then
            let halfW = (screenW - gap) / 2f
            let scaleX = halfW / gameW
            let scaleY = screenH / gameH
            let scale = min scaleX scaleY
            let dstW = gameW * scale
            let dstH = gameH * scale
            let offsetY = (screenH - dstH) / 2f
            let offsetX1 = (halfW - dstW) / 2f
            let offsetX2 = halfW + gap + (halfW - dstW) / 2f
            ((offsetX1, offsetY, dstW, dstH), (offsetX2, offsetY, dstW, dstH))
        else
            let scale = float32 Config.scale
            let dstW = gameW * scale
            let dstH = gameH * scale
            ((0f, 0f, dstW, dstH), (dstW + gap * scale, 0f, dstW, dstH))

    let private drawScreen texture =
        let (x, y, w, h) = getScreenRect ()
        drawScaledTextureRec texture Screen.width Screen.height x y w h

    let private drawScreenAt texture (x, y, w, h) =
        drawScaledTextureRec texture Screen.width Screen.height x y w h

    let loadFramebuffer pos texture (framebuffer: Shade array) =
        framebuffer
        |> mapToColors
        |> updateTexture texture
        |> drawScaledTexture pos (float32 Config.scale)

    let loadPpuFramebuffer (framebuffer: Shade array) =
        framebuffer
        |> mapToColors
        |> updateTexture screenTexture
        |> drawScreen

    let loadTilesFramebuffer = loadFramebuffer (0f, 0f) tilesTexture

    let loadColorFramebuffer (framebuffer: FameBoy.Ppu.Color array) =
        framebuffer
        |> mapCgbColors
        |> updateTexture screenTexture
        |> drawScreen

    /// Render P1 screen in link mode
    let loadPpuFramebufferP1 (framebuffer: Shade array) =
        let rect1, _ = getLinkScreenRects ()
        framebuffer |> mapToColors |> updateTexture screenTexture |> drawScreenAt <| rect1

    let loadColorFramebufferP1 (framebuffer: FameBoy.Ppu.Color array) =
        let rect1, _ = getLinkScreenRects ()
        framebuffer |> mapCgbColors |> updateTexture screenTexture |> drawScreenAt <| rect1

    /// Render P2 screen in link mode
    let loadPpuFramebufferP2 (framebuffer: Shade array) =
        let _, rect2 = getLinkScreenRects ()
        framebuffer |> mapToColors |> updateTexture screenTexture2 |> drawScreenAt <| rect2

    let loadColorFramebufferP2 (framebuffer: FameBoy.Ppu.Color array) =
        let _, rect2 = getLinkScreenRects ()
        framebuffer |> mapCgbColors |> updateTexture screenTexture2 |> drawScreenAt <| rect2

    let loadDebugFramebuffers (ppu: Ppu) =
        dumpVram ppu

        loadFramebuffer (mapPos ()) mapTexture backgroundFramebuffer
        loadFramebuffer (tilePos ()) tilesTexture tilesFramebuffer

    let close () =
        Raylib.UnloadTexture screenTexture
        Raylib.UnloadTexture screenTexture2
        Raylib.UnloadTexture mapTexture
        Raylib.UnloadTexture tilesTexture

        Raylib.CloseWindow()
