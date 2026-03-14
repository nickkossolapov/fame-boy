open System
open System.IO
open FameBoy.Emulator
open FameBoy.Hardware
open FameBoy.Joypad
open FameBoy.Raylib
open FameBoy.Raylib.Graphics.GraphicsPipeline
open FameBoy.Raylib.Joypad
open FameBoy.Raylib.RaylibBindings
open Raylib_cs

let args = Environment.GetCommandLineArgs()

if args.Length <> 2 then
    eprintfn "Usage: fame-boy <rom-file>"
    exit 1

let romPath = args[1]

if not (File.Exists romPath) then
    eprintfn $"File not found: %s{romPath}"
    exit 1

Raylib.InitWindow(Config.width * Config.scale, Config.height * Config.scale, "Fame Boy")

let icon = Raylib.LoadImage("icon.png")
Raylib.SetWindowIcon(icon)
Raylib.UnloadImage(icon)

Raylib.SetTargetFPS 120

let bytes = File.ReadAllBytes romPath

let mutable joypadState: JoypadState =
    { Up = false
      Down = false
      Left = false
      Right = false
      A = false
      B = false
      Start = false
      Select = false }

let frameBuffer, memory, stepEmulator = createEmulator bytes (fun () -> joypadState)

let targetCyclesPerMs = float32 cpuFrequency
let maxCyclesPerFrame = float32 cpuFrequency / 60f // So if the emulator can't reach 60 FPS it won't drown itself in instructions
let mutable accumulator = 0f

while (not (windowShouldClose ())) do
    let cycles = Math.Min(targetCyclesPerMs * Raylib.GetFrameTime(), maxCyclesPerFrame)
    accumulator <- accumulator + cycles

    joypadState <- getJoypadState ()


    while (accumulator > 0f) do
        let cpuCycles = stepEmulator () |> float32
        accumulator <- accumulator - cpuCycles

    beginDrawing ()
    loadPpuFramebuffer frameBuffer

    if Config.enableDebugView then
        loadDebugFramebuffers memory

    endDrawing ()

close ()
