module FameBoy.Test.TimerTests

open FameBoy.Hardware
open FameBoy.Memory
open FameBoy.Timer
open NUnit.Framework

let private setupTimer () =
    let memory = createTestMemory [||]
    let timer = createTimer ()
    timer, memory

let private stepN (timer: TimerState) (memory: Memory) n =
    for _ in 1..n do
        stepTimers timer memory

[<Test>]
let ``Timer increments TIMA every 4 M-cycles with clock select 01`` () =
    let timer, memory = setupTimer ()

    memory[IoRegisters.Tac] <- 0x05uy // enabled, clock select 01
    memory[IoRegisters.Tima] <- 0uy

    // After 4 M-cycles, TIMA should have incremented once
    stepN timer memory 4
    Assert.That(memory[IoRegisters.Tima], Is.EqualTo 1uy)

    // After 4 more, TIMA should be 2
    stepN timer memory 4
    Assert.That(memory[IoRegisters.Tima], Is.EqualTo 2uy)

[<Test>]
let ``Timer increments TIMA every 16 M-cycles with clock select 10`` () =
    let timer, memory = setupTimer ()

    memory[IoRegisters.Tac] <- 0x06uy // enabled, clock select 10
    memory[IoRegisters.Tima] <- 0uy

    stepN timer memory 15
    Assert.That(memory[IoRegisters.Tima], Is.EqualTo 0uy)

    stepN timer memory 1
    Assert.That(memory[IoRegisters.Tima], Is.EqualTo 1uy)

[<Test>]
let ``Timer does not increment when disabled`` () =
    let timer, memory = setupTimer ()

    memory[IoRegisters.Tac] <- 0x01uy // disabled, clock select 01
    memory[IoRegisters.Tima] <- 0uy

    stepN timer memory 100
    Assert.That(memory[IoRegisters.Tima], Is.EqualTo 0uy)

[<Test>]
let ``Timer sets interrupt flag on TIMA overflow`` () =
    let timer, memory = setupTimer ()

    memory[IoRegisters.Tac] <- 0x05uy // enabled, clock select 01 (every 4 M-cycles)
    memory[IoRegisters.Tima] <- 0xFFuy
    memory[IoRegisters.If] <- 0uy

    // After 4 M-cycles TIMA should overflow from 0xFF to 0x00
    stepN timer memory 4
    Assert.That(memory[IoRegisters.If] &&& 0x04uy, Is.Not.EqualTo 0uy, "Timer interrupt flag should be set")

[<Test>]
let ``Timer reloads TMA after overflow`` () =
    let timer, memory = setupTimer ()

    memory[IoRegisters.Tac] <- 0x05uy // enabled, clock select 01
    memory[IoRegisters.Tima] <- 0xFFuy
    memory[IoRegisters.Tma] <- 0x42uy
    memory[IoRegisters.If] <- 0uy

    // 4 M-cycles: TIMA overflows, sets HasTimerOverflowed
    stepN timer memory 4

    // Next M-cycle: TMA is loaded into TIMA
    stepN timer memory 1
    Assert.That(memory[IoRegisters.Tima], Is.EqualTo 0x42uy)

[<Test>]
let ``Blargg test 4 scenario - timer overflow after 1024 M-cycles with TIMA starting at 0`` () =
    let timer, memory = setupTimer ()

    // Reproduce Blargg test #4 setup
    memory[IoRegisters.Tac] <- 0x05uy // enabled, clock select 01 (every 4 M-cycles)
    memory[IoRegisters.Tima] <- 0uy
    memory[IoRegisters.If] <- 0uy

    // After 1023 M-cycles, TIMA should NOT have overflowed
    // 1023 / 4 = 255 full increments, TIMA = 255 = 0xFF, no overflow yet
    stepN timer memory 1023
    Assert.That(memory[IoRegisters.If] &&& 0x04uy, Is.EqualTo 0uy, "Timer interrupt should NOT be set before 1024 M-cycles")

    // The 1024th M-cycle causes the 256th increment: TIMA goes 0xFF -> 0x00 (overflow)
    stepN timer memory 1
    Assert.That(memory[IoRegisters.If] &&& 0x04uy, Is.Not.EqualTo 0uy, "Timer interrupt SHOULD be set at exactly 1024 M-cycles")

[<Test>]
let ``DIV register increments at correct rate`` () =
    let timer, memory = setupTimer ()

    let divFrequency = 1048576 / 16384 // = 64 M-cycles
    memory.IoRegisters[IoRegisterOffsets.Div] <- 0uy

    stepN timer memory (divFrequency - 1)
    Assert.That(memory[IoRegisters.Div], Is.EqualTo 0uy)

    stepN timer memory 1
    Assert.That(memory[IoRegisters.Div], Is.EqualTo 1uy)
