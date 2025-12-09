open System.Diagnostics
open System.IO
open FameBoy.Cpu.Execute
open FameBoy.Cpu.Opcodes
open FameBoy.Graphics.Ppu
open FameBoy.Hardware
open FameBoy.Joypad
open FameBoy.Memory
open FameBoy.Ppu.Debug
open FameBoy.Raylib.Joypad
open FameBoy.Raylib.RaylibBindings
open FameBoy.Startup
open Raylib_cs

let scale = 2

let enableDebugView = true

let width =
    if enableDebugView then
        Screen.width + 256 + 1
    else
        Screen.width

let height = if enableDebugView then 256 + 96 + 1 else Screen.height

Raylib.InitWindow (width * scale, height * scale, "Fame Boy")
Raylib.SetTargetFPS 60

module RateLimiting =
    let rateLimitFunc (time: int) (func: 'a -> unit) =
        let stopwatch = Stopwatch ()
        let mutable lastPrint = 0L

        fun p ->
            if not stopwatch.IsRunning then
                stopwatch.Start ()

            let now = stopwatch.ElapsedMilliseconds

            if now - lastPrint >= time then
                lastPrint <- now
                func p

open RateLimiting


module GraphicsPipeline =
    let private mapSide = 256 // 32 tiles -> 32 * 8 pixels
    let private tilesHeight = 96 // 384 tiles -> 12 lines at 32 tiles per line * 8 pixels

    let mutable private screenTexture =
        Raylib.GenImageColor (Screen.width, Screen.height, Color.Black) |> Raylib.LoadTextureFromImage

    let mutable private mapTexture =
        Raylib.GenImageColor (mapSide, mapSide, Color.Black) |> Raylib.LoadTextureFromImage

    let mutable private tilesTexture =
        Raylib.GenImageColor (mapSide, tilesHeight, Color.Black) |> Raylib.LoadTextureFromImage

    let private mapToColors =
        Array.map (function
            | White -> Color (186, 218, 85)
            | Light -> Color (130, 153, 59)
            | Dark -> Color (74, 87, 34)
            | Black -> Color (19, 22, 8))

    let private backgroundFramebuffer = Array.create<Shade> (mapSide * mapSide) White
    let private tilesFramebuffer = Array.create<Shade> (mapSide * tilesHeight) White

    let private mapPos = (float32 ((Screen.width + 1) * scale), 0f)
    let private tilePos = float32 ((Screen.width + 1) * scale), float32 ((mapSide + 1) * scale)

    let private dumpVram =
        rateLimitFunc 1000 (fun memory ->
            dumpBackground backgroundFramebuffer memory
            dumpTiles tilesFramebuffer memory)

    let loadFramebuffer pos texture (framebuffer: Shade array) =
        framebuffer |> mapToColors |> updateTexture texture |> drawScaledTexture pos (float32 scale)

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

        Raylib.CloseWindow ()

open GraphicsPipeline

let mcyclesPerSec = 1000 / 60

let printLastFrameTime = rateLimitFunc 1000 (fun () -> printfn $"{1f / Raylib.GetFrameTime ()}")

let printBits =
    rateLimitFunc 1000 (fun (s: uint8) -> printfn $"{System.Convert.ToString(s, 2).PadLeft (8, '0')}")

// let bytes = File.ReadAllBytes "D:/gb/tetris.gb"
let bytes = File.ReadAllBytes "/Users/nickkossolapov/dev/gb/tetris.gb"
// let bytes = File.ReadAllBytes "/Users/nickkossolapov/dev/gb/dr mario.gb"

let memory = createMemory bytes
let cpu = createDmgCpu memory
let ppu = createPpu memory


while (not (windowShouldClose ())) do
    // TODO: have a better frame time counter
    let mutable counter = int ((Raylib.GetFrameTime ()) * 1000000f)
    
    // It's faster to do this inside the nester loop below
    // TODO investigate why
    // applyJoypadState (getJoypadState ()) memory

    printLastFrameTime ()

    while (counter > 0) do
        let cpuCycles = stepCpu cpu
        counter <- counter - cpuCycles

        applyJoypadState (getJoypadState ()) memory

        let ppuSteps = cpuCycles * 4

        for _ in 0..ppuSteps do
            stepPpu ppu

    beginDrawing ()
    loadPpuFramebuffer ppu.Framebuffer

    if enableDebugView then
        loadDebugFramebuffers memory

    endDrawing ()

close ()
