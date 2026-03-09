open System.IO
open FameBoy.Emulator
open FameBoy.Joypad
open FameBoy.Raylib
open FameBoy.Raylib.Graphics.GraphicsPipeline
open FameBoy.Raylib.Joypad
open FameBoy.Raylib.RaylibBindings
open FameBoy.Raylib.Utils.RateLimiting
open Raylib_cs

Raylib.InitWindow(Config.width * Config.scale, Config.height * Config.scale, "Fame Boy")
Raylib.SetTargetFPS 60

let mcyclesPerSec = 1000 / 60

let printBits =
    rateLimitFunc 1000 (fun (s: uint8) -> printfn $"{System.Convert.ToString(s, 2).PadLeft(8, '0')}")

let frameTimesDuration = 120 // frame
let frameTimes = Array.create frameTimesDuration 60f
let mutable frameIndex = 0

let printTotalFrameTime =
    rateLimitFunc 1000 (fun () ->
        let avg = Array.average frameTimes
        printfn $"avg: %.4f{1f / avg} | last frame: %.4f{1f / Raylib.GetFrameTime()}")

let bytes = File.ReadAllBytes "D:/gb/tetris.gb"

let mutable joypadState: JoypadState =
    { Up = false
      Down = false
      Left = false
      Right = false
      A = false
      B = false
      Start = false
      Select = false }

let struct (frameBuffer, memory, stepEmulator) = createEmulator bytes (fun () -> joypadState)

while (not (windowShouldClose ())) do
    // TODO: have a better frame time counter
    let mutable counter = 16666
    frameTimes[frameIndex] <- Raylib.GetFrameTime()
    frameIndex <- (frameIndex + 1) % frameTimesDuration

    joypadState <- getJoypadState ()

    printTotalFrameTime ()

    while (counter > 0) do
        let cpuCycles = stepEmulator ()
        counter <- counter - cpuCycles

    beginDrawing ()
    loadPpuFramebuffer frameBuffer

    if Config.enableDebugView then
        loadDebugFramebuffers memory

    endDrawing ()

close ()
