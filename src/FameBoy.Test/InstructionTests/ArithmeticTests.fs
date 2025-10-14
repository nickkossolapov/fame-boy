module FameBoy.Test.InstructionTests.ArithmeticTests


open FameBoy.Cpu.Execute
open FameBoy.Cpu.Instructions
open FameBoy.Cpu.Opcodes
open FameBoy.Cpu.State
open NUnit.Framework

[<Test>]
let ``Increment 8-bit register (no zero) - inc c`` () =
    // Setup
    let opcode = 0x0Cuy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100
    cpu.Registers.C <- 0x0Fuy // will become 0x10
    cpu.setFlag Carry true // ensure carry unaffected
    cpu.setFlag Zero true // will be cleared
    cpu.setFlag Subtract true // will be cleared
    cpu.setFlag HalfCarry false // will be set due to low nibble overflow
    cpu.Memory[0x100] <- opcode

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 1)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 1))

    Assert.That (cpu.Registers.C, Is.EqualTo 0x10uy)
    Assert.That (cpu.getFlag Flag.Zero, Is.False)
    Assert.That (cpu.getFlag Flag.Subtract, Is.False)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.True)
    Assert.That (cpu.getFlag Flag.Carry, Is.True)

[<Test>]
let ``Increment 8-bit register (wrap to zero) - inc c`` () =
    // Setup
    let opcode = 0x0Cuy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100
    cpu.Registers.C <- 0xFFuy // will wrap to 0x00
    cpu.setFlag Carry false // ensure carry unaffected
    cpu.setFlag Zero false // will be set
    cpu.setFlag Subtract true // will be cleared
    cpu.setFlag HalfCarry false // will be set (0xF -> 0x0)
    cpu.Memory[0x100] <- opcode

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 1)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 1))

    Assert.That (cpu.Registers.C, Is.EqualTo 0x00uy)
    Assert.That (cpu.getFlag Flag.Zero, Is.True)
    Assert.That (cpu.getFlag Flag.Subtract, Is.False)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.True)
    Assert.That (cpu.getFlag Flag.Carry, Is.False)
