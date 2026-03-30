module FameBoy.Benchmark.Web.Program

open Fable.Core
open Fable.Core.JsInterop
open FameBoy.Emulator
open FameBoy.Hardware
open FameBoy.Joypad

let joypadState =
    { Up = false
      Down = false
      Left = false
      Right = false
      A = false
      B = false
      Start = false
      Select = false }

[<Import("readFileSync", "node:fs")>]
let readFileSync (path: string) : JS.Uint8Array = jsNative

[<Import("fileURLToPath", "node:url")>]
let fileURLToPath (url: string) : string = jsNative

[<Emit("performance.now()")>]
let performanceNow () : float = jsNative

let readRom (path: string) : uint8 array =
    let buffer = readFileSync path
    let arr = Array.zeroCreate (int buffer?length)

    for i in 0 .. arr.Length - 1 do
        arr[i] <- !!buffer?i

    arr

let mCyclesPerFrame = 17_556.0

let runBenchmark name filename cycles iterations =
    let bytes = readRom filename
    let mutable totalMs = 0.0

    for _ in 1..iterations do
        let _, _, stepEmulator, _ = createEmulator bytes 4096 (fun () -> joypadState)
        let mutable remaining = cycles

        let startTime = performanceNow ()

        while remaining > 0 do
            remaining <- remaining - stepEmulator ()

        let endTime = performanceNow ()
        totalMs <- totalMs + (endTime - startTime)

    let meanMs = totalMs / float iterations
    let totalFrames = float cycles / mCyclesPerFrame
    let fps = totalFrames / (meanMs / 1_000.0)

    name, meanMs, fps

let benchmarks =
    [ "Flag", "flag.gb", 5, 3
      "Roboto", "roboto.gb", 100, 2
      "Merken", "merken.gb", 100, 2 ]

[<EntryPoint>]
let main _ =
    let resourceDir: string =
        fileURLToPath (emitJsExpr () "new URL('../FameBoy.Benchmark/Resources/', import.meta.url).href")

    printfn "Warmup..."
    let warmupBytes = readRom $"{resourceDir}flag.gb"
    let _, _, warmupStep, _ = createEmulator warmupBytes 4096 (fun () -> joypadState)
    let mutable w = cpuFrequency

    while w > 0 do
        w <- w - warmupStep ()

    let results =
        benchmarks
        |> List.map (fun (name, filename, multiplier, iterations) ->
            let cycles = multiplier * cpuFrequency
            runBenchmark name $"{resourceDir}{filename}" cycles iterations)

    printfn ""
    printfn "| Method | FPS | Mean |"
    printfn "|--------|-----|------|"

    for name, meanMs, fps in results do
        if meanMs >= 1000.0 then
            printfn $"| {name} | %.1f{fps} | %.1f{meanMs / 1000.0} s |"
        else
            printfn $"| {name} | %.1f{fps} | %.1f{meanMs} ms |"

    printfn ""
    0
