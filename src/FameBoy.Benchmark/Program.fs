open System.IO
open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Columns
open BenchmarkDotNet.Configs
open BenchmarkDotNet.Reports
open BenchmarkDotNet.Running
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

let runBenchmark filename cycles =
    let bytes = Path.Combine("Resources", filename) |> File.ReadAllBytes

    let _, _, stepEmulator = createEmulator bytes (fun () -> joypadState)
    let mutable remaining = cycles

    while remaining > 0 do
        remaining <- remaining - stepEmulator ()

let benchmarkCycles =
    [ "Flag", 5; "Roboto", 100; "Merken", 100 ]
    |> List.map (fun (name, value) -> name, value * cpuFrequency)
    |> Map.ofList

type EmulatorComparison() =
    [<WarmupCount(5)>]
    [<IterationCount(15)>]
    [<Benchmark>]
    member _.Flag() =
        runBenchmark "flag.gb" benchmarkCycles["Flag"] // short and cyclical

    [<WarmupCount(3)>]
    [<IterationCount(5)>]
    [<Benchmark>]
    member _.Roboto() =
        runBenchmark "roboto.gb" benchmarkCycles["Roboto"] // longer running with various types of rendering, no MBC

    [<WarmupCount(3)>]
    [<IterationCount(5)>]
    [<Benchmark>]
    member _.Merken() =
        runBenchmark "merken.gb" benchmarkCycles["Merken"] // longer running with MBC


let mCyclesPerFrame = 17_556.0

type FpsColumn() =
    interface IColumn with
        member this.Id = "FPS"
        member this.ColumnName = "FPS"
        member this.IsDefault(_, _) = false
        member this.IsAvailable _ = true
        member this.AlwaysShow = true
        member this.Category = ColumnCategory.Statistics
        member this.PriorityInCategory = -1
        member this.IsNumeric = true
        member this.UnitType = UnitType.Dimensionless
        member this.Legend = "Frames per second with same CPU cycle/s as DMG at 60 Hz"

        member this.GetValue(summary, benchmarkCase, _style) =
            (this :> IColumn).GetValue(summary, benchmarkCase)

        member this.GetValue(summary: Summary, benchmarkCase) =
            let name = benchmarkCase.Descriptor.WorkloadMethod.Name

            match summary[benchmarkCase], benchmarkCycles.TryGetValue(name) with
            | report, (true, cycles) when report <> null ->
                let totalFrames = float cycles / mCyclesPerFrame
                let fps = totalFrames / (report.ResultStatistics.Mean / 1_000_000_000.0)

                $"%.1f{fps}"
            | _ -> "N/A"

[<EntryPoint>]
let Main _ =
    let config = ManualConfig.Create(DefaultConfig.Instance).AddColumn(FpsColumn())

    BenchmarkRunner.Run(typeof<EmulatorComparison>, config) |> ignore

    0
