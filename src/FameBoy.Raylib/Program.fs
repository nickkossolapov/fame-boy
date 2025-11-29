open System.Collections
open System.Diagnostics
open System.IO
open FameBoy.Cpu.Execute
open FameBoy.Cpu.Opcodes
open FameBoy.Cpu.State
open FameBoy.Ppu
open FameBoy.Hardware
open FameBoy.Memory
open FameBoy.Raylib.RaylibBindings
open FameBoy.Startup
open Raylib_cs

let scale = 4

Raylib.InitWindow (Screen.width * scale, Screen.height * scale, "Fame Boy")
Raylib.SetTargetFPS 60

let mutable screenTexture =
    Raylib.GenImageColor (Screen.width, Screen.height, Color.Black)
    |> Raylib.LoadTextureFromImage

let mapToColors =
    Array.map (fun b -> if b then Color (186, 218, 85) else Color (74, 87, 34))

let drawTexture = drawScaledTexture (float32 scale)

let draw (framebuffer: bool array) =
    framebuffer
    |> mapToColors
    |> beginDrawing
    |> updateTexture screenTexture
    |> drawTexture
    |> endDrawing

let testFramebuffer = Array.zeroCreate<bool> (Screen.width * Screen.height)

let mcyclesPerSec = 1000 / 60

let private stopwatch = Stopwatch ()
let mutable private lastPrint = 0L

let print (msg: string) =
    if not stopwatch.IsRunning then
        stopwatch.Start ()

    let now = stopwatch.ElapsedMilliseconds

    if now - lastPrint >= 1000L then
        printfn $"{msg}"
        lastPrint <- now


let renderTile x y loc (memory: Memory) =
    for row in 0..7 do
        let addr = loc + (row * 2)
        let left = memory.Array[addr]
        let right = memory.Array[(addr + 1)]

        for col in 0..7 do
            let bit = 7 - col
            let leftBit = left >>> bit &&& 1uy
            let rightBit = (right >>> bit &&& 1uy) <<< 1
            let net = leftBit ||| rightBit
            
            let bufferPos = ((y + row) * Screen.width) + (x + col)
            
            testFramebuffer[bufferPos] <- net > 0uy

let drawBackground (memory: Memory) =
    let start = 
        if memory[Registers.Lcdc] &&& 0b1000uy <> 0uy 
        then 0x9C00 
        else 0x9800
    
    let getLoc byte =
        if memory[Registers.Lcdc] &&& 0b10000uy <> 0uy then
            0x10us * (uint16 byte) + 0x8000us
        else
            0x10us * uint16 (int8 byte) + 0x9000us

    for row in 0..31 do
        for col in 0..31 do
            let mapIndex = start + (row * 32) + col
            let tileIndex = getLoc memory.Array[mapIndex]

            renderTile (col * 8) (row * 8) (int tileIndex) memory

let drawTiles (memory: Memory) =    
    for row in 0..11 do // 384 total tiles -> 384/32 = 12 rows
        for col in 0..31 do
            let mapIndex = 0x8000 + ((row * 32) + col) * 16
    
            renderTile (col * 8) (row * 8) (int mapIndex) memory

let runGameBoy () =
    // let bytes = File.ReadAllBytes "D:/gb/bootroms/dmg_boot.bin"
    let bytes = File.ReadAllBytes "D:/gb/tests/cpu_instrs.gb"

    let memory = createMemory bytes
    // Array.blit headerBitmapCheck 0 memory.Array 0x104 headerBitmapCheck.Length

    // let cpu = createCpu memory
    let cpu = createDmgCpu memory
    let ppu = createPpu memory

    while (not (windowShouldClose ())) do

        let mutable counter = 16666 // int ((Raylib.GetFrameTime ()) * 1000000f)
        
        print $"{Raylib.GetFrameTime ()}"

        while (counter > 0) do
            let instr = fetchAndDecode cpu.Memory cpu.Pc

            // if cpu.Pc = 0x40us then if Debugger.IsAttached then Debugger.Break()

            // let vram = memory.Array[0x8800..0x97ff]
            let tileMaps = memory.Array[0x9800..0x9bff]

            let cpuCycles = execute cpu instr
            counter <- counter - cpuCycles

            let ppuSteps = cpuCycles * 4

            // for _ in 0..ppuSteps do
            stepPpu ppu

        // drawTiles memory
        drawBackground memory
        draw testFramebuffer

runGameBoy ()

Raylib.UnloadTexture screenTexture
Raylib.CloseWindow ()
