open Browser
open Browser.Types
open Fable.Core

let fileUploadButton = document.getElementById "rom-file"

fileUploadButton.addEventListener ("change", fun (a) -> printfn $"{a}")



let onFileLoaded (ev: Event) =
    let input = ev.target :?> HTMLInputElement
    let files = input.files

    if not (isNull files) && files.length > 0 then
        let file = files.[0]
        let reader = FileReader.Create()

        reader.onload <-
            fun _ ->
                let arrayBuffer = reader.result :?> JS.ArrayBuffer
                let bytes = JS.Constructors.Uint8Array.Create(arrayBuffer)
                printfn $"Loaded {bytes.length} bytes"

        reader.readAsArrayBuffer file

fileUploadButton.addEventListener ("change", onFileLoaded)

// let bytes =

// let mutable joypadState: JoypadState =
//     { Up = false
//       Down = false
//       Left = false
//       Right = false
//       A = false
//       B = false
//       Start = false
//       Select = false }
//
// let struct (frameBuffer, memory, stepEmulator) = createEmulator bytes (fun () -> joypadState)
