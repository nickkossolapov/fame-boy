open System
open Browser
open Browser.Types
open Fable.Core
open Fable.Core.JsInterop
open FameBoy.Emulator
open FameBoy.Hardware
open FameBoy.Web.Audio
open FameBoy.Web.Joypad
open FameBoy.Web.JsBindings


let private getElement id =
    match document.getElementById id with
    | null -> failwith $"Element '{id}' not found"
    | el -> el

let getJoypadState = initJoypad ()

let frameDrivenParam =
    match URLSearchParams.Create(window.location.search).get("frame-driven") with
     | Some "false" -> false
     | Some _ -> true
     | _ -> false

let screenCanvas = getElement "screen" :?> HTMLCanvasElement
let startOverlay = getElement "start-overlay"
let fpsCounter = getElement "fps-counter"
let fileUploadButton = getElement "rom-file"

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

let mutable currentAnimationFrame = None

let private showOverlayError (message: string) =
    startOverlay?innerHTML <- message
    startOverlay?classList?remove "hidden"

let startEmulator bytes =
    currentAnimationFrame |> Option.iter window.cancelAnimationFrame

    startOverlay?classList?add "hidden"

    let ppu, apu, stepEmulator, applyJoypadState =
        try
            createEmulator bytes 4096 getJoypadState
        with ex ->
            showOverlayError "Error!<br>Invalid ROM"
            raise ex

    ensureInitialized ()
    resetPlayback ()

    let draw () =
        loadImageData ppu.Framebuffer
        ctx.putImageData (imageData, 0, 0)
    
    let targetCyclesPerMs = float cpuFrequency / 1000.0
    let maxCyclesPerFrame = float cpuFrequency / 60.0
    let mutable accumulator = 0.0

    let fpsWindowSize = 30
    let fpsHistory = Array.zeroCreate<float> fpsWindowSize
    let mutable fpsIndex = 0
    let mutable fpsFrameCount = 0
    let mutable lastFpsLogTime = 0.0

    let rec runEmulator (last: float) (timestamp: float) =
        let dt = timestamp - last

        getJoypadState () |> applyJoypadState
        
        let frameDriven = frameDrivenParam || isUserMuted ()

        if frameDriven then
            let cycles = Math.Min(targetCyclesPerMs * dt, maxCyclesPerFrame)
            accumulator <- accumulator + cycles
            
            while accumulator > 0 do
                let mCycles = float (stepEmulator ())
                accumulator <- accumulator - mCycles
 
        tryQueueAudio apu stepEmulator frameDriven

        reportFrameTime dt

        fpsHistory[fpsIndex] <- dt
        fpsIndex <- (fpsIndex + 1) % fpsWindowSize
        fpsFrameCount <- min (fpsFrameCount + 1) fpsWindowSize

        if timestamp - lastFpsLogTime >= 500.0 then
            let mutable total = 0.0

            for i = 0 to fpsFrameCount - 1 do
                total <- total + fpsHistory[i]

            let avgDt = total / float fpsFrameCount
            let fps = 1000.0 / avgDt
            fpsCounter.textContent <- $"%.0f{fps}"
            lastFpsLogTime <- timestamp

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

                try
                    startEmulator bytes
                with _ ->
                    ()

        reader.readAsArrayBuffer file

fileUploadButton.addEventListener ("change", onFileLoaded)

let scaleSelector = document.querySelectorAll "input[name='scale']"

for i in 0 .. int scaleSelector.length - 1 do
    let input = scaleSelector.[i] :?> HTMLInputElement

    input.addEventListener ("change", fun _ -> document.documentElement?style?setProperty ("--s", input.value))

let muteButton = getElement "mute-button"
let muteIconOn = getElement "mute-icon-on"
let muteIconOff = getElement "mute-icon-off"

muteButton.addEventListener (
    "click",
    fun _ ->
        let isMuted = toggleMute ()
        muteIconOn?classList?toggle ("hidden", isMuted)
        muteIconOff?classList?toggle ("hidden", not isMuted)
)

// Pre-fetch the default ROM, then wait for user interaction to start
// User interaction on the page is needed to start Web Audio
let mutable private defaultRomBytes: byte array option = None
let mutable private defaultRomStarted = false

let private onFirstInteraction (_: Event) =
    if not defaultRomStarted then
        defaultRomStarted <- true

        match defaultRomBytes with
        | Some bytes -> startEmulator bytes
        | None -> ()

let loadDefaultRom () =
    async {
        try
            let! response = fetch "tobudx.gb" |> Async.AwaitPromise
            let! arrayBuffer = response.arrayBuffer () |> Async.AwaitPromise
            let uint8Array = JS.Constructors.Uint8Array.Create(arrayBuffer)
            let bytes: byte array = Array.init (int uint8Array.length) (fun i -> uint8Array[i])

            defaultRomBytes <- Some bytes

            document.addEventListener ("click", onFirstInteraction)
            document.addEventListener ("keydown", onFirstInteraction)
        with _ ->
            showOverlayError "Error!<br>Couldn't load demo ROM"
    }
    |> Async.StartImmediate

loadDefaultRom ()
