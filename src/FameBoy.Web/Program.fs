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
let startOverlay = document.getElementById "start-overlay"

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
let maxCyclesPerFrame = float cpuFrequency / 60.0
let mutable currentAnimationFrame = None

module private Audio =
    [<Literal>]
    let audioSamplingRate = 48000

    [<Literal>]
    let audioBufferSize = 2048

    [<Literal>]
    let defaultVolume = 0.6

    [<Emit("new AudioContext({sampleRate: $0})")>]
    let private createAudioContext (_: int) : obj = jsNative

    [<Emit("$0.createGain()")>]
    let private createGain (ctx: obj) : obj = jsNative

    [<Emit("$0.gain.value = $1")>]
    let private setGainValue (gain: obj) (value: float) : unit = jsNative

    [<Emit("$0.connect($1)")>]
    let private connectNode (source: obj) (dest: obj) : unit = jsNative

    [<Emit("$0.destination")>]
    let private destination (ctx: obj) : obj = jsNative

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

    [<Emit("$0.start($1)")>]
    let private startSource (source: obj) (time: float) : unit = jsNative

    [<Emit("$0.currentTime")>]
    let private currentTime (ctx: obj) : float = jsNative

    let mutable private audioCtx: obj option = None
    let mutable private gainNode: obj option = None
    let mutable audioInitialized = false
    let mutable private userMuted = false
    let mutable private suppressed = false
    let private audioBuffer = Array.zeroCreate<float32> audioBufferSize
    let mutable nextPlayTime = 0.0

    let private applyGain () =
        match gainNode with
        | Some gain ->
            let shouldMute = userMuted || suppressed
            setGainValue gain (if shouldMute then 0.0 else defaultVolume)
        | None -> ()

    let initAudio () =
        let ctx = createAudioContext audioSamplingRate
        let gain = createGain ctx
        setGainValue gain defaultVolume
        connectNode gain (destination ctx)
        audioCtx <- Some ctx
        gainNode <- Some gain

    let toggleMute () =
        userMuted <- not userMuted
        applyGain ()
        userMuted

    let private frameWindowSize = 20
    let private frameHistory = Array.create frameWindowSize false
    let mutable private frameIndex = 0
    let mutable private badCount = 0

    // Sliding window mute. If browser is out of focus, audio slows down and isn't great
    let reportFrameTime (dt: float) =
        let isBad = dt > 25.0
        let wasBad = frameHistory[frameIndex]

        frameHistory[frameIndex] <- isBad
        frameIndex <- (frameIndex + 1) % frameWindowSize

        if isBad && not wasBad then
            badCount <- badCount + 1
        elif not isBad && wasBad then
            badCount <- badCount - 1

        let shouldSuppress = badCount >= 4

        if shouldSuppress <> suppressed then
            suppressed <- shouldSuppress
            applyGain ()

    let tryQueueAudio (apu: Apu) =
        match audioCtx, gainNode with
        | Some ctx, Some gain ->
            let now = currentTime ctx

            if nextPlayTime < now then
                nextPlayTime <- now

            while nextPlayTime - now < 0.05 do
                readResampledBuffer apu audioBuffer audioSamplingRate

                let buffer = createBuffer ctx 1 audioBufferSize audioSamplingRate
                let channelData = getChannelData buffer 0

                for i = 0 to audioBufferSize - 1 do
                    channelData[i] <- audioBuffer[i]

                let source = createBufferSource ctx
                setBuffer source buffer
                connectNode source gain
                startSource source nextPlayTime
                nextPlayTime <- nextPlayTime + bufferDuration buffer
        | _ -> ()

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

        reportFrameTime dt
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

let muteButton = document.getElementById "mute-button"
let muteIconOn = document.getElementById "mute-icon-on"
let muteIconOff = document.getElementById "mute-icon-off"

muteButton.addEventListener (
    "click",
    fun _ ->
        let isMuted = toggleMute ()
        muteIconOn?classList?toggle ("hidden", isMuted)
        muteIconOff?classList?toggle ("hidden", not isMuted)
)

// Pre-fetch the default ROM, then wait for user interaction to start
let mutable private defaultRomBytes: byte array option = None
let mutable private defaultRomStarted = false

let private onFirstInteraction (_: Event) =
    startOverlay?classList?add "hidden"

    if not defaultRomStarted then
        defaultRomStarted <- true

        match defaultRomBytes with
        | Some bytes -> startEmulator bytes
        | None -> ()

let loadDefaultRom () =
    async {
        let! response = fetch "tobudx.gb" |> Async.AwaitPromise
        let! arrayBuffer = response.arrayBuffer () |> Async.AwaitPromise
        let uint8Array = JS.Constructors.Uint8Array.Create(arrayBuffer)
        let bytes: byte array = Array.init (int uint8Array.length) (fun i -> uint8Array[i])

        defaultRomBytes <- Some bytes

        document.addEventListener ("click", onFirstInteraction)
        document.addEventListener ("keydown", onFirstInteraction)
    }
    |> Async.StartImmediate

loadDefaultRom ()
