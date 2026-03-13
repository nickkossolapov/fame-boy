open Browser
open Browser.Types
open Fable.Core
open FameBoy.Emulator
open FameBoy.Hardware
open FameBoy.Joypad

let fileUploadButton = document.getElementById "rom-file"
let screenCanvas = document.getElementById "screen" :?> HTMLCanvasElement

screenCanvas.width <- Screen.width
screenCanvas.height <- Screen.height

let ctx = screenCanvas.getContext "2d" :?> CanvasRenderingContext2D
let imageData = ctx.createImageData (Screen.width, Screen.height)

let shades =
    [ (186uy, 218uy, 85uy)
      (130uy, 153uy, 59uy)
      (74uy, 87uy, 34uy)
      (19uy, 22uy, 8uy) ]

let loadImageData emulatorFramebuffer =
    let len = Array.length emulatorFramebuffer - 1

    for i in 0..len do
        let r, g, b = shades[int (emulatorFramebuffer[i])]
        let j = i * 4

        imageData.data[j] <- r
        imageData.data[j + 1] <- g
        imageData.data[j + 2] <- b
        imageData.data[j + 3] <- 255uy

let mutable joypadState: JoypadState =
    { Up = false
      Down = false
      Left = false
      Right = false
      A = false
      B = false
      Start = false
      Select = false }

let mutable logNow = 0
let targetMCyclesPerMs = 1048.576
let mutable accumulator = 0.0


let startEmulator bytes =
    let struct (frameBuffer, _, stepEmulator) =
        createEmulator bytes (fun () -> joypadState)

    let draw () =
        loadImageData frameBuffer
        ctx.putImageData (imageData, 0, 0)

    let rec runEmulator (last: float) (timestamp: float) =
        logNow <- logNow + 1
        let dt = timestamp - last
        let cycles = targetMCyclesPerMs * dt
        accumulator <- accumulator + cycles

        if logNow = 60 then
            console.log (1000.0 / dt)
            logNow <- 0

        while accumulator > 0 do
            let mCycles = float (stepEmulator ())
            accumulator <- accumulator - mCycles

        draw ()
        window.requestAnimationFrame (runEmulator timestamp) |> ignore

    runEmulator 0 0

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
