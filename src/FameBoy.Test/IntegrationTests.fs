module FameBoy.Test.IntegrationTests

open System.IO
open NUnit.Framework
open FameBoy.Emulator
open FameBoy.Joypad


let private defaultJoypadState: JoypadState =
    { Up = false
      Down = false
      Left = false
      Right = false
      A = false
      B = false
      Start = false
      Select = false }

[<Test>]
let ``dmg-acid2 framebuffer matches expected output after 150000 CPU cycles`` () =
    let rom = Path.Combine("Resources", "dmg-acid2.gb") |> File.ReadAllBytes
    let ppu, _, stepEmulator, _ = createEmulator rom (fun () -> defaultJoypadState)

    for _ in 0..150000 do
        stepEmulator () |> ignore

    let actual = ppu.Framebuffer |> Array.map byte
    let expected = Path.Combine("Resources", "dmg-acid2.bin") |> File.ReadAllBytes

    Assert.That(actual, Is.EqualTo(expected))

[<Test>]
let ``cpu_instrs framebuffer matches expected output after 25000000 CPU cycles`` () =
    let rom = Path.Combine("Resources", "cpu_instrs.gb") |> File.ReadAllBytes
    let ppu, _, stepEmulator, _ = createEmulator rom (fun () -> defaultJoypadState)

    for _ in 0..25000000 do
        stepEmulator () |> ignore

    let actual = ppu.Framebuffer |> Array.map byte
    let expected = Path.Combine("Resources", "cpu_instrs.bin") |> File.ReadAllBytes

    Assert.That(actual, Is.EqualTo(expected))
