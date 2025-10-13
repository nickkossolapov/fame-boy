module FameBoy.Test.CpuTests.LoadTests

open FameBoy.Cpu.Execute
open FameBoy.Cpu.Opcodes
open FameBoy.Cpu.State
open NUnit.Framework

[<Test>]
let ``Load 16-bit register - ld sp,n16`` () =
    // Setup
    let opcode = 0x31uy
    let length = 3
    let cpu = createCpu [||]

    cpu.Pc <- 0x100
    cpu.Memory.[0x100] <- opcode
    cpu.Memory[0x101] <- 0xFEuy
    cpu.Memory[0x102] <- 0xFFuy

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (cpu.Pc, Is.EqualTo (0x100 + length))
    Assert.That (cpu.Sp, Is.EqualTo 0xFEFFus)

[<Test>]
let ``Load from accumulator (indirect HL, decrement) - ld [hld],a`` () =
    // Setup
    let opcode = 0x32uy
    let length = 1
    let cpu = createCpu [||]

    cpu.Pc <- 0x100
    cpu.Registers.A <- 0xABuy
    cpu.Registers.setHL 0x1234us
    cpu.Memory[0x100] <- opcode

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That(cpu.Pc, Is.EqualTo(0x100 + length))
    Assert.That(cpu.Memory[0x1234], Is.EqualTo(0xABuy))
    Assert.That(cpu.Registers.getHL(), Is.EqualTo(0x1233us))
