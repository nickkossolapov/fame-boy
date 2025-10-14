module FameBoy.Test.InstructionTests.BitwiseTests

open FameBoy.Cpu.Execute
open FameBoy.Cpu.Instructions
open FameBoy.Cpu.Opcodes
open FameBoy.Cpu.State
open NUnit.Framework

[<Test>]
let ``Test bit 7 of H register - bit 7,h`` () =
    // Setup
    let cpu = createCpu [||]
    cpu.Pc <- 0x100
    cpu.Memory[0x100] <- 0xCBuy
    cpu.Memory[0x101] <- 0x7Cuy // CB7C = BIT 7,H
    cpu.Registers.H <- 0b10000000uy // bit 7 set
    cpu.setFlag Flag.Zero true
    cpu.setFlag Flag.Subtract true
    cpu.setFlag Flag.HalfCarry false

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 2)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 2))

    Assert.That (cpu.getFlag Flag.Zero, Is.False)
    Assert.That (cpu.getFlag Flag.Subtract, Is.False)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.True)

[<Test>]
let ``Test bit 7 of H register, bit not set - bit 7,h`` () =
    // Setup
    let cpu = createCpu [||]
    cpu.Pc <- 0x100
    cpu.Memory[0x100] <- 0xCBuy
    cpu.Memory[0x101] <- 0x7Cuy // CB7C = BIT 7,H
    cpu.Registers.H <- 0b00000000uy // bit 7 not set
    cpu.setFlag Flag.Zero false
    cpu.setFlag Flag.Subtract true
    cpu.setFlag Flag.HalfCarry false

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 2)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 2))

    Assert.That (cpu.getFlag Flag.Zero, Is.True)
    Assert.That (cpu.getFlag Flag.Subtract, Is.False)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.True)
