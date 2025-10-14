module FameBoy.Test.InstructionTests.LogicTests

open FameBoy.Cpu.Execute
open FameBoy.Cpu.Opcodes
open FameBoy.Cpu.State
open FameBoy.Cpu.Instructions
open NUnit.Framework

[<Test>]
let ``Bitwise XOR A with itself - xor a`` () =
    // Setup
    let cpu = createCpu [||]
    cpu.Pc <- 0x100
    cpu.Memory[0x100] <- 0xAFuy
    cpu.Registers.A <- 0xABuy

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That(instr.Length, Is.EqualTo 1)
    Assert.That(instr.MCycles, Is.EqualTo (Fixed 1))
    
    Assert.That(cpu.Pc, Is.EqualTo 0x101)
    Assert.That(cpu.Registers.A, Is.EqualTo 0uy)
    Assert.That(cpu.getFlag Flag.Zero, Is.True)
    Assert.That(cpu.getFlag Flag.Subtract, Is.False)
    Assert.That(cpu.getFlag Flag.HalfCarry, Is.False)
    Assert.That(cpu.getFlag Flag.Carry, Is.False)
