open System.IO
open FameBoy.Cpu.Execute
open FameBoy.Cpu.State
open FameBoy.Graphics.Ppu
open FameBoy.Joypad
open FameBoy.Memory
open FameBoy.Raylib
open FameBoy.Raylib.Graphics.GraphicsPipeline
open FameBoy.Raylib.Joypad
open FameBoy.Raylib.RaylibBindings
open FameBoy.Raylib.Utils.RateLimiting
open FameBoy.Startup
open FameBoy.Timer
open Raylib_cs

Raylib.InitWindow (Config.width * Config.scale, Config.height * Config.scale, "Fame Boy")
Raylib.SetTargetFPS 60


let mcyclesPerSec = 1000 / 60

let printLastFrameTime = rateLimitFunc 1000 (fun () -> printfn $"{1f / Raylib.GetFrameTime ()}")

let printCpuState = rateLimitFunc 1000 (fun (cpu: Cpu) -> printfn $"{cpu.getState ()}")

let printBits =
    rateLimitFunc 1000 (fun (s: uint8) -> printfn $"{System.Convert.ToString(s, 2).PadLeft (8, '0')}")

// let bytes = File.ReadAllBytes "D:/gb/tetris.gb"
// let bytes = File.ReadAllBytes "/Users/nickkossolapov/dev/gb/tetris.gb"
// let bytes = File.ReadAllBytes "/Users/nickkossolapov/dev/gb/dr mario.gb"
let bytes =
    File.ReadAllBytes "/Users/nickkossolapov/dev/gb/blargg/cpu_instrs/individual/02-interrupts.gb"

let timer = createTimer ()
let memory = createMemory bytes
let cpu = createDmgCpu memory
let ppu = createPpu memory



while (not (windowShouldClose ())) do
    // TODO: have a better frame time counter
    let mutable counter = 16666

    // It's faster to do this inside the nester loop below
    // TODO investigate why
    // applyJoypadState (getJoypadState ()) memory

    printLastFrameTime ()

    while (counter > 0) do
        let cpuCycles = stepCpu cpu
        counter <- counter - cpuCycles

        applyJoypadState (getJoypadState ()) memory
        stepTimers timer memory

        let ppuSteps = cpuCycles * 4

        for _ in 0..ppuSteps do
            stepPpu ppu

    beginDrawing ()
    loadPpuFramebuffer ppu.Framebuffer

    if Config.enableDebugView then
        loadDebugFramebuffers memory

    endDrawing ()

close ()
