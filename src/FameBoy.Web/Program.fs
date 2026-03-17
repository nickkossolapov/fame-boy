open System
open Browser
open Browser.Types
open Fable.Core
open FameBoy.Emulator
open FameBoy.Hardware
open FameBoy.Web.Joypad

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


let startEmulator bytes =
    currentAnimationFrame |> Option.iter window.cancelAnimationFrame

    let ppu, stepEmulator, applyJoypadState = createEmulator bytes getJoypadState
    let mutable accumulator = 0.0

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
