open System
open System.IO
open FameBoy.Apu
open FameBoy.Emulator
open FameBoy.Hardware
open FameBoy.Joypad
open FameBoy.Raylib
open FameBoy.Raylib.Graphics.GraphicsPipeline
open FameBoy.Raylib.Joypad
open FameBoy.Raylib.RaylibBindings
open Raylib_cs

let args = Environment.GetCommandLineArgs()

if args.Length < 2 || args.Length > 3 then
    eprintfn "Usage: fame-boy <rom-file> [scale - optional, default 4]"
    exit 1

let romPath = args[1]

if args.Length = 3 then
    match Int32.TryParse(args[2]) with
    | true, s when s > 0 -> Config.scale <- s
    | _ ->
        eprintfn $"Invalid scale value: %s{args[2]} (must be a positive integer)"
        exit 1

if not (File.Exists romPath) then
    eprintfn $"File not found: %s{romPath}"
    exit 1

Raylib.InitWindow(Config.width * Config.scale, Config.height * Config.scale, "Fame Boy")

let icon = Raylib.LoadImage("icon.png")
Raylib.SetWindowIcon(icon)
Raylib.UnloadImage(icon)

Raylib.SetTargetFPS 60


// TODO maybe create an audio file?

Raylib.InitAudioDevice()
Raylib.SetAudioStreamBufferSizeDefault(bufferSize)
let audioBuffer = Array.zeroCreate<float32> bufferSize
let audioStream = Raylib.LoadAudioStream(uint32 samplingRate, 32u, 1u)

Raylib.PlayAudioStream audioStream

let tryQueueAudio (apu: Apu) =
    while isAudioStreamProcessed audioStream do
        for i in 0 .. bufferSize - 1 do
            if apu.ReadHead < apu.WriteHead then
                audioBuffer[i] <- apu.RingBuffer[apu.ReadHead % ringBufferSize]
                apu.ReadHead <- apu.ReadHead + 1
            else
                audioBuffer[i] <- 0f

        updateAudioStream audioStream audioBuffer

// audio end


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

let ppu, apu, stepEmulator, applyJoypadState =
    createEmulator bytes (fun () -> joypadState)

let targetCyclesPerMs = float32 cpuFrequency
let maxCyclesPerFrame = float32 cpuFrequency / 60f // So if the emulator can't reach 60 FPS it won't drown itself in instructions
let mutable accumulator = 0f

while (not (windowShouldClose ())) do
    let cycles = Math.Min(targetCyclesPerMs * Raylib.GetFrameTime(), maxCyclesPerFrame)
    accumulator <- accumulator + cycles

    let joypadState = getJoypadState ()
    joypadState |> applyJoypadState

    if joypadState.Up then
        apu.TestFrequency <- apu.TestFrequency + 5f

    if joypadState.Down then
        apu.TestFrequency <- apu.TestFrequency - 5f

    while (accumulator > 0f) do
        let cpuCycles = stepEmulator () |> float32
        accumulator <- accumulator - cpuCycles

    tryQueueAudio apu

    beginDrawing ()
    loadPpuFramebuffer ppu.Framebuffer

    if Config.enableDebugView then
        loadDebugFramebuffers ppu

    endDrawing ()

close ()
