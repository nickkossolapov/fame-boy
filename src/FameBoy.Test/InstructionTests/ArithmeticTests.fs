module FameBoy.Test.InstructionTests.ArithmeticTests


open FameBoy.Cpu.Execute
open FameBoy.Cpu.Instructions
open FameBoy.Cpu.Opcodes
open FameBoy.Cpu.State
open NUnit.Framework


[<Test>]
let ``Add 8-bit register to A (no carry, no half-carry, no zero) - add b`` () =
    // Setup
    let opcode = 0x80uy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Registers.A <- 0b00010010uy
    cpu.Registers.B <- 0b00100011uy
    cpu.setFlag Flag.Zero true
    cpu.setFlag Flag.Subtract true
    cpu.setFlag Flag.HalfCarry true
    cpu.setFlag Flag.Carry true
    cpu.Memory[0x100us] <- opcode

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 1)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 1))

    Assert.That (cpu.Registers.A, Is.EqualTo (0b00010010uy + 0b00100011uy))
    Assert.That (cpu.getFlag Flag.Zero, Is.False)
    Assert.That (cpu.getFlag Flag.Subtract, Is.False)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.False)
    Assert.That (cpu.getFlag Flag.Carry, Is.False)

[<Test>]
let ``Add 8-bit register to A (result zero) - add b`` () =
    // Setup
    let opcode = 0x80uy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Registers.A <- 0b00000000uy
    cpu.Registers.B <- 0b00000000uy
    cpu.setFlag Flag.Zero false
    cpu.setFlag Flag.Subtract true
    cpu.setFlag Flag.HalfCarry true
    cpu.setFlag Flag.Carry true
    cpu.Memory[0x100us] <- opcode

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 1)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 1))

    Assert.That (cpu.Registers.A, Is.EqualTo 0b00000000uy)
    Assert.That (cpu.getFlag Flag.Zero, Is.True)
    Assert.That (cpu.getFlag Flag.Subtract, Is.False)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.False)
    Assert.That (cpu.getFlag Flag.Carry, Is.False)

[<Test>]
let ``Add 8-bit register to A (half-carry, no carry) - add b`` () =
    // Setup
    let opcode = 0x80uy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Registers.A <- 0b00001111uy
    cpu.Registers.B <- 0b00000001uy
    cpu.setFlag Flag.Zero true
    cpu.setFlag Flag.Subtract true
    cpu.setFlag Flag.HalfCarry false
    cpu.setFlag Flag.Carry true
    cpu.Memory[0x100us] <- opcode

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 1)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 1))

    Assert.That (cpu.Registers.A, Is.EqualTo 0b00010000uy)
    Assert.That (cpu.getFlag Flag.Zero, Is.False)
    Assert.That (cpu.getFlag Flag.Subtract, Is.False)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.True)
    Assert.That (cpu.getFlag Flag.Carry, Is.False)

[<Test>]
let ``Add 8-bit register to A (carry, no half-carry) - add b`` () =
    // Setup
    let opcode = 0x80uy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Registers.A <- 0b11110000uy
    cpu.Registers.B <- 0b11110000uy
    cpu.setFlag Flag.Zero true
    cpu.setFlag Flag.Subtract true
    cpu.setFlag Flag.HalfCarry false
    cpu.setFlag Flag.Carry false
    cpu.Memory[0x100us] <- opcode

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 1)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 1))

    Assert.That (cpu.Registers.A, Is.EqualTo 0b11100000uy)
    Assert.That (cpu.getFlag Flag.Zero, Is.False)
    Assert.That (cpu.getFlag Flag.Subtract, Is.False)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.True)
    Assert.That (cpu.getFlag Flag.Carry, Is.True)
