module FameBoy.Test.TimerTests

open FameBoy.Hardware
open FameBoy.IoController
open FameBoy.Memory
open FameBoy.Test.TestHelpers
open FameBoy.Timer
open NUnit.Framework

let private setupTimer () =
    let memory = createTestMemory [||]
    let timer = createTimer ()
    timer, memory.IoController

let private stepN (timer: TimerState) (io: IoController) n =
    for _ in 1..n do
        stepTimers timer io

[<Test>]
let ``Timer increments TIMA every 4 M-cycles with clock select 01`` () =
    let timer, io = setupTimer ()

    io.Registers[Io.Tac] <- 0x05uy // enabled, clock select 01
    io.Registers[Io.Tima] <- 0uy

    // After 4 M-cycles, TIMA should have incremented once
    stepN timer io 4
    Assert.That(io.Registers[Io.Tima], Is.EqualTo 1uy)

    // After 4 more, TIMA should be 2
    stepN timer io 4
    Assert.That(io.Registers[Io.Tima], Is.EqualTo 2uy)

[<Test>]
let ``Timer increments TIMA every 16 M-cycles with clock select 10`` () =
    let timer, io = setupTimer ()

    io.Registers[Io.Tac] <- 0x06uy // enabled, clock select 10
    io.Registers[Io.Tima] <- 0uy

    stepN timer io 15
    Assert.That(io.Registers[Io.Tima], Is.EqualTo 0uy)

    stepN timer io 1
    Assert.That(io.Registers[Io.Tima], Is.EqualTo 1uy)

[<Test>]
let ``Timer does not increment when disabled`` () =
    let timer, io = setupTimer ()

    io.Registers[Io.Tac] <- 0x01uy // disabled, clock select 01
    io.Registers[Io.Tima] <- 0uy

    stepN timer io 100
    Assert.That(io.Registers[Io.Tima], Is.EqualTo 0uy)

[<Test>]
let ``Timer sets interrupt flag on TIMA overflow`` () =
    let timer, io = setupTimer ()

    io.Registers[Io.Tac] <- 0x05uy // enabled, clock select 01 (every 4 M-cycles)
    io.Registers[Io.Tima] <- 0xFFuy
    io.Registers[Io.If] <- 0uy

    // After 4 M-cycles TIMA should overflow from 0xFF to 0x00
    stepN timer io 4
    Assert.That(io.Registers[Io.If] &&& 0x04uy, Is.Not.EqualTo 0uy, "Timer interrupt flag should be set")

[<Test>]
let ``Timer reloads TMA after overflow`` () =
    let timer, io = setupTimer ()

    io.Registers[Io.Tac] <- 0x05uy // enabled, clock select 01
    io.Registers[Io.Tima] <- 0xFFuy
    io.Registers[Io.Tma] <- 0x42uy
    io.Registers[Io.If] <- 0uy

    // 4 M-cycles: TIMA overflows, sets HasTimerOverflowed
    stepN timer io 4

    // Next M-cycle: TMA is loaded into TIMA
    stepN timer io 1
    Assert.That(io.Registers[Io.Tima], Is.EqualTo 0x42uy)

[<Test>]
let ``Blargg test 4 scenario - timer overflow after 1024 M-cycles with TIMA starting at 0`` () =
    let timer, io = setupTimer ()

    // Reproduce Blargg test #4 setup
    io.Registers[Io.Tac] <- 0x05uy // enabled, clock select 01 (every 4 M-cycles)
    io.Registers[Io.Tima] <- 0uy
    io.Registers[Io.If] <- 0uy

    // After 1023 M-cycles, TIMA should NOT have overflowed
    // 1023 / 4 = 255 full increments, TIMA = 255 = 0xFF, no overflow yet
    stepN timer io 1023
    Assert.That(io.Registers[Io.If] &&& 0x04uy, Is.EqualTo 0uy, "Timer interrupt should NOT be set before 1024 M-cycles")

    // The 1024th M-cycle causes the 256th increment: TIMA goes 0xFF -> 0x00 (overflow)
    stepN timer io 1
    Assert.That(io.Registers[Io.If] &&& 0x04uy, Is.Not.EqualTo 0uy, "Timer interrupt SHOULD be set at exactly 1024 M-cycles")

[<Test>]
let ``DIV register increments at correct rate`` () =
    let timer, io = setupTimer ()

    let divFrequency = 1048576 / 16384 // = 64 M-cycles
    io.Registers[Io.Div] <- 0uy

    stepN timer io (divFrequency - 1)
    Assert.That(io.Registers[Io.Div], Is.EqualTo 0uy)

    stepN timer io 1
    Assert.That(io.Registers[Io.Div], Is.EqualTo 1uy)
