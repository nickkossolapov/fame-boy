module FameBoy.Test.InstructionTests.LoadTests

open FameBoy.Cpu.Execute
open FameBoy.Cpu.Opcodes
open FameBoy.Cpu.State
open FameBoy.Cpu.Instructions
open NUnit.Framework

[<Test>]
let ``Load 16-bit register - ld sp,n16`` () =
    // Setup
    let opcode = 0x31uy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100
    cpu.Memory[0x100] <- opcode
    cpu.Memory[0x101] <- 0xFFuy
    cpu.Memory[0x102] <- 0xFEuy

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 3)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 3))

    Assert.That (cpu.Sp, Is.EqualTo 0xFEFFus)

[<Test>]
let ``Load from accumulator (indirect HL, decrement) - ld [hld],a`` () =
    // Setup
    let opcode = 0x32uy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100
    cpu.Registers.A <- 0xABuy
    cpu.Registers.setHL 0xFE00us
    cpu.Memory[0x100] <- opcode

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 1)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 2))

    Assert.That (cpu.Memory[0xFE00], Is.EqualTo 0xABuy)
    Assert.That (cpu.Registers.getHL (), Is.EqualTo 0xFDFFus)

[<Test>]
let ``Load 8-bit register (immediate) - ld c,n8`` () =
    // Setup
    let opcode = 0x0Euy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100
    cpu.Memory[0x100] <- opcode
    cpu.Memory[0x101] <- 0x42uy

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 2)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 2))

    Assert.That (cpu.Registers.C, Is.EqualTo 0x42uy)

[<Test>]
let ``Load from accumulator (indirect 0xFF00+C) - ldh [c],a`` () =
    // Setup
    let opcode = 0xE2uy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100
    cpu.Registers.A <- 0xABuy
    cpu.Registers.C <- 0x42uy
    cpu.Memory[0x100] <- opcode

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 1)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 2))

    Assert.That (cpu.Memory[0xFF42], Is.EqualTo 0xABuy)
