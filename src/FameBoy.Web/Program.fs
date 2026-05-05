#if !FABLE_COMPILER
eprintfn "This project is a Fable (F#-to-JavaScript) web app and cannot be run with 'dotnet run'."
eprintfn "To start the web frontend, run:"
eprintfn "  cd src/FameBoy.Web"
eprintfn "  npm run dev"
eprintfn ""
eprintfn "For the desktop version, use:"
eprintfn "  dotnet run --project src/FameBoy.Raylib -- <rom-file>"
exit 1
#endif

open System
open Browser
open Browser.Types
open Fable.Core
open Fable.Core.JsInterop
open FameBoy.Apu
open FameBoy.Emulator
open FameBoy.Hardware
open FameBoy.Ppu
open FameBoy.Serial
open FameBoy.Web.Audio
open FameBoy.Web.Joypad
open FameBoy.Web.JsBindings


let private getElement id =
    match document.getElementById id with
    | null -> failwith $"Element '{id}' not found"
    | el -> el

let getJoypadState = initJoypad ()
let getJoypadState2 = initJoypadP2 ()

let frameDrivenParam =
    match URLSearchParams.Create(window.location.search).get("frame-driven") with
     | Some "false" -> false
     | Some _ -> true
     | _ -> false

let linkModeParam =
    match URLSearchParams.Create(window.location.search).get("link") with
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

// P2 canvas for link mode
let screen2Canvas =
    if linkModeParam then
        let el = getElement "screen2" :?> HTMLCanvasElement
        el.width <- Screen.width
        el.height <- Screen.height
        Some el
    else
        None

let ctx2 =
    screen2Canvas |> Option.map (fun c -> c.getContext "2d" :?> CanvasRenderingContext2D)

let imageData2 =
    ctx2 |> Option.map (fun c -> c.createImageData (Screen.width, Screen.height))

let shades =
    [| (186uy, 218uy, 85uy)
       (130uy, 153uy, 59uy)
       (74uy, 87uy, 34uy)
       (19uy, 22uy, 8uy) |]

let loadImageData (ppu: Ppu) (imgData: ImageData) =
    let len = Array.length ppu.Framebuffer - 1

    if ppu.IoController.CgbMode then
        for i in 0..len do
            let color = ppu.ColorFramebuffer[i]
            let j = i * 4

            imgData.data[j] <- color.R
            imgData.data[j + 1] <- color.G
            imgData.data[j + 2] <- color.B
            imgData.data[j + 3] <- 255uy
    else
        for i in 0..len do
            let r, g, b = shades[int ppu.Framebuffer[i]]
            let j = i * 4

            imgData.data[j] <- r
            imgData.data[j + 1] <- g
            imgData.data[j + 2] <- b
            imgData.data[j + 3] <- 255uy

let mutable currentAnimationFrame = None

let private showOverlayError (message: string) =
    startOverlay?innerHTML <- message
    startOverlay?classList?remove "hidden"

let startEmulator bytes =
    currentAnimationFrame |> Option.iter window.cancelAnimationFrame

    startOverlay?classList?add "hidden"

    let ppu1, apu1, serial1, io1, stepEmulator1, applyJoypadState1 =
        try
            createEmulator bytes 4096 getJoypadState
        with ex ->
            showOverlayError "Error!<br>Invalid ROM"
            raise ex

    // In link mode, create a second emulator instance
    let linkState =
        if linkModeParam then
            let ppu2, _apu2, serial2, io2, stepEmulator2, applyJoypadState2 =
                createEmulator bytes 4096 getJoypadState2

            Some (ppu2, serial2, io2, stepEmulator2, applyJoypadState2)
        else
            None

    ensureInitialized ()
    resetPlayback ()

    let draw () =
        loadImageData ppu1 imageData
        ctx.putImageData (imageData, 0, 0)

        match linkState, ctx2, imageData2 with
        | Some (ppu2, _, _, _, _), Some c2, Some img2 ->
            loadImageData ppu2 img2
            c2.putImageData (img2, 0, 0)
        | _ -> ()
    
    let targetCyclesPerMs = float cpuFrequency / 1000.0
    let maxCyclesPerFrame = float cpuFrequency / 60.0
    let mutable accumulator = 0.0
    let mutable cycles1 = 0.0
    let mutable cycles2 = 0.0

    let fpsWindowSize = 30
    let fpsHistory = Array.zeroCreate<float> fpsWindowSize
    let mutable fpsIndex = 0
    let mutable fpsFrameCount = 0
    let mutable lastFpsLogTime = 0.0

    let rec runEmulator (last: float) (timestamp: float) =
        let dt = timestamp - last

        getJoypadState () |> applyJoypadState1

        match linkState with
        | Some (_, _, _, _, applyJoypadState2) ->
            getJoypadState2 () |> applyJoypadState2
        | None -> ()
        
        let frameDriven = frameDrivenParam || isUserMuted ()

        if frameDriven then
            let cycles = Math.Min(targetCyclesPerMs * dt, maxCyclesPerFrame)
            accumulator <- accumulator + cycles
            
            while accumulator > 0 do
                let mCycles = float (stepEmulator1 ())
                cycles1 <- cycles1 + mCycles

                match linkState with
                | Some (_, serial2, io2, stepEmulator2, _) ->
                    while cycles2 < cycles1 do
                        cycles2 <- cycles2 + float (stepEmulator2 ())
                    exchangeSerial serial1 io1 serial2 io2
                | None -> ()

                accumulator <- accumulator - mCycles
 
        let stepWithLink () =
            let c = stepEmulator1 ()
            cycles1 <- cycles1 + float c

            match linkState with
            | Some (_, serial2, io2, stepEmulator2, _) ->
                while cycles2 < cycles1 do
                    cycles2 <- cycles2 + float (stepEmulator2 ())
                exchangeSerial serial1 io1 serial2 io2
            | None -> ()

            c

        tryQueueAudio apu1 stepWithLink frameDriven

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

// Show/hide link mode UI
if linkModeParam then
    match document.getElementById "link-container" with
    | null -> ()
    | el -> el?classList?remove "hidden"

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
