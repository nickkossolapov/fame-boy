open System
open Browser
open Browser.Types
open Fable.Core
open Fable.Core.JsInterop
open FameBoy.Apu
open FameBoy.Emulator
open FameBoy.Hardware
open FameBoy.Web.Joypad

type private IResponse =
    abstract arrayBuffer: unit -> JS.Promise<JS.ArrayBuffer>

[<Global>]
let private fetch (url: string) : JS.Promise<IResponse> = jsNative

initOnScreenButtons ()

let fileUploadButton = document.getElementById "rom-file"
let screenCanvas = document.getElementById "screen" :?> HTMLCanvasElement

screenCanvas.width <- Screen.width
screenCanvas.height <- Screen.height

let ctx = screenCanvas.getContext "2d" :?> CanvasRenderingContext2D
let imageData = ctx.createImageData (Screen.width, Screen.height)

let shades =
    [| (186uy, 218uy, 85uy)
       (130uy, 153uy, 59uy)
       (74uy, 87uy, 34uy)
       (19uy, 22uy, 8uy) |]

let loadImageData emulatorFramebuffer =
    let len = Array.length emulatorFramebuffer - 1

    for i in 0..len do
        let r, g, b = shades[int emulatorFramebuffer[i]]
        let j = i * 4

        imageData.data[j] <- r
        imageData.data[j + 1] <- g
        imageData.data[j + 2] <- b
        imageData.data[j + 3] <- 255uy

let targetCyclesPerMs = float cpuFrequency / 1000.0
let maxCyclesPerFrame = float cpuFrequency / 60.0 // So if the emulator can't reach 60 FPS it won't drown itself in instructions
let mutable currentAnimationFrame = None

module private Audio =
    [<Literal>]
    let audioSamplingRate = 48000

    [<Literal>]
    let audioBufferSize = 2048

    [<Emit("new AudioContext({sampleRate: $0})")>]
    let private createAudioContext (_: int) : obj = jsNative

    [<Emit("$0.resume()")>]
    let private resumeContext (ctx: obj) : JS.Promise<unit> = jsNative

    [<Emit("$0.state")>]
    let private contextState (ctx: obj) : string = jsNative

    [<Emit("$0.createBuffer($1, $2, $3)")>]
    let private createBuffer (ctx: obj) (channels: int) (length: int) (sampleRate: int) : obj = jsNative

    [<Emit("$0.getChannelData($1)")>]
    let private getChannelData (buffer: obj) (channel: int) : float32 array = jsNative

    [<Emit("$0.duration")>]
    let private bufferDuration (buffer: obj) : float = jsNative

    [<Emit("$0.createBufferSource()")>]
    let private createBufferSource (ctx: obj) : obj = jsNative

    [<Emit("$0.buffer = $1")>]
    let private setBuffer (source: obj) (buffer: obj) : unit = jsNative

    [<Emit("$0.connect($1.destination)")>]
    let private connectToDestination (source: obj) (ctx: obj) : unit = jsNative

    [<Emit("$0.start($1)")>]
    let private startSource (source: obj) (time: float) : unit = jsNative

    [<Emit("$0.currentTime")>]
    let private currentTime (ctx: obj) : float = jsNative

    let mutable private audioCtx: obj option = None
    let mutable audioInitialized = false
    let private audioBuffer = Array.zeroCreate<float32> audioBufferSize
    let mutable nextPlayTime = 0.0

    let initAudio () =
        let ctx = createAudioContext audioSamplingRate

        audioCtx <- Some ctx

    let tryResumeAudio () =
        match audioCtx with
        | Some ctx when contextState ctx = "suspended" -> resumeContext ctx |> ignore
        | _ -> ()

    let tryQueueAudio (apu: Apu) =
        match audioCtx with
        | Some ctx ->
            let now = currentTime ctx

            if nextPlayTime < now then
                nextPlayTime <- now

            // Queue buffers to stay ahead of playback by ~50ms
            while nextPlayTime - now < 0.05 do
                readResampledBuffer apu audioBuffer audioSamplingRate

                let buffer = createBuffer ctx 1 audioBufferSize audioSamplingRate
                let channelData = getChannelData buffer 0

                for i = 0 to audioBufferSize - 1 do
                    channelData[i] <- audioBuffer[i]

                let source = createBufferSource ctx
                setBuffer source buffer
                connectToDestination source ctx
                startSource source nextPlayTime
                nextPlayTime <- nextPlayTime + bufferDuration buffer
        | None -> ()

open Audio

let startEmulator bytes =
    currentAnimationFrame |> Option.iter window.cancelAnimationFrame

    let ppu, apu, stepEmulator, applyJoypadState =
        createEmulator bytes 8192 getJoypadState

    let mutable accumulator = 0.0

    if not audioInitialized then
        audioInitialized <- true
        initAudio ()

    nextPlayTime <- 0.0

    let draw () =
        loadImageData ppu.Framebuffer
        ctx.putImageData (imageData, 0, 0)

    let rec runEmulator (last: float) (timestamp: float) =
        let dt = timestamp - last
        let cycles = Math.Min(targetCyclesPerMs * dt, maxCyclesPerFrame)
        accumulator <- accumulator + cycles

        getJoypadState () |> applyJoypadState

        while accumulator > 0 do
            let mCycles = float (stepEmulator ())
            accumulator <- accumulator - mCycles

        tryQueueAudio apu

        draw ()
        currentAnimationFrame <- window.requestAnimationFrame (runEmulator timestamp) |> Some

    currentAnimationFrame <- window.requestAnimationFrame (runEmulator 0) |> Some

let onFileLoaded (ev: Event) =
    let input = ev.target :?> HTMLInputElement
    let files = input.files

    if not (isNull files) && files.length > 0 then
        let file = files.[0]
        let reader = FileReader.Create()

        reader.onload <-
            fun _ ->
                let arrayBuffer = reader.result :?> JS.ArrayBuffer
                let uint8Array = JS.Constructors.Uint8Array.Create(arrayBuffer)
                let bytes: byte array = Array.init (int uint8Array.length) (fun i -> uint8Array[i])

                startEmulator bytes

        reader.readAsArrayBuffer file

fileUploadButton.addEventListener ("change", onFileLoaded)

let scaleSelector = document.querySelectorAll "input[name='scale']"

for i in 0 .. int scaleSelector.length - 1 do
    let input = scaleSelector.[i] :?> HTMLInputElement

    input.addEventListener ("change", fun _ -> document.documentElement?style?setProperty ("--s", input.value))

document.addEventListener ("click", fun _ -> tryResumeAudio ())
document.addEventListener ("keydown", fun _ -> tryResumeAudio ())

let loadDefaultRom () =
    async {
        let! response = fetch "tobudx.gb" |> Async.AwaitPromise
        let! arrayBuffer = response.arrayBuffer () |> Async.AwaitPromise
        let uint8Array = JS.Constructors.Uint8Array.Create(arrayBuffer)
        let bytes: byte array = Array.init (int uint8Array.length) (fun i -> uint8Array[i])

        startEmulator bytes
    }
    |> Async.StartImmediate

loadDefaultRom ()
