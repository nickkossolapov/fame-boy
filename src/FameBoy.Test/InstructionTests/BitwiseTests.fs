module FameBoy.Test.InstructionTests.BitwiseTests

open FameBoy.Cpu.Execute
open FameBoy.Cpu.Instructions
open FameBoy.Cpu.Opcodes
open FameBoy.Cpu.State
open NUnit.Framework

let twoBitPrefix = 0xCBuy

[<Test>]
let ``Test bit 7 of H register - bit 7,h`` () =
    // Setup
    let opcode = 0x7Cuy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Memory[0x100us] <- twoBitPrefix
    cpu.Memory[0x101us] <- opcode
    cpu.Registers.H <- 0b10000000uy
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
    let opcode = 0x7Cuy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Memory[0x100us] <- twoBitPrefix
    cpu.Memory[0x101us] <- opcode
    cpu.Registers.H <- 0b00000000uy
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

[<Test>]
let ``Rotate left register C - rl c`` () =
    // Setup
    let opcode = 0x11uy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Memory[0x100us] <- twoBitPrefix
    cpu.Memory[0x101us] <- opcode
    cpu.Registers.C <- 0b10000001uy
    cpu.setFlag Flag.Carry false
    cpu.setFlag Flag.Zero true
    cpu.setFlag Flag.Subtract true
    cpu.setFlag Flag.HalfCarry true

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 2)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 2))

    Assert.That (cpu.Registers.C, Is.EqualTo 0b00000010uy)
    Assert.That (cpu.getFlag Flag.Zero, Is.False)
    Assert.That (cpu.getFlag Flag.Subtract, Is.False)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.False)
    Assert.That (cpu.getFlag Flag.Carry, Is.True)

[<Test>]
let ``Rotate left register C, result is zero - rl c`` () =
    // Setup
    let opcode = 0x11uy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Memory[0x100us] <- twoBitPrefix
    cpu.Memory[0x101us] <- opcode
    cpu.Registers.C <- 0b10000000uy
    cpu.setFlag Flag.Carry false
    cpu.setFlag Flag.Zero false
    cpu.setFlag Flag.Subtract true
    cpu.setFlag Flag.HalfCarry true

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 2)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 2))

    Assert.That (cpu.Registers.C, Is.EqualTo 0b00000000uy)
    Assert.That (cpu.getFlag Flag.Zero, Is.True)
    Assert.That (cpu.getFlag Flag.Subtract, Is.False)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.False)
    Assert.That (cpu.getFlag Flag.Carry, Is.True)


[<Test>]
let ``Rotate left register C, no carry - rl c`` () =
    // Setup
    let opcode = 0x11uy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Memory[0x100us] <- twoBitPrefix
    cpu.Memory[0x101us] <- opcode
    cpu.Registers.C <- 0b00000001uy
    cpu.setFlag Flag.Carry true
    cpu.setFlag Flag.Zero false
    cpu.setFlag Flag.Subtract true
    cpu.setFlag Flag.HalfCarry true

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 2)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 2))

    Assert.That (cpu.Registers.C, Is.EqualTo 0b00000011uy)
    Assert.That (cpu.getFlag Flag.Zero, Is.False)
    Assert.That (cpu.getFlag Flag.Subtract, Is.False)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.False)
    Assert.That (cpu.getFlag Flag.Carry, Is.False)

[<Test>]
let ``Rotate left accumulator - rla`` () =
    // Setup
    let opcode = 0x17uy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Memory[0x100us] <- opcode
    cpu.Registers.A <- 0b10000001uy
    cpu.setFlag Flag.Carry false
    cpu.setFlag Flag.Zero true
    cpu.setFlag Flag.Subtract true
    cpu.setFlag Flag.HalfCarry true

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 1)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 1))

    Assert.That (cpu.Registers.A, Is.EqualTo 0b00000010uy)
    Assert.That (cpu.getFlag Flag.Zero, Is.False)
    Assert.That (cpu.getFlag Flag.Subtract, Is.False)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.False)
    Assert.That (cpu.getFlag Flag.Carry, Is.True)

[<Test>]
let ``Rotate left accumulator, result is zero - rla`` () =
    // Setup
    let opcode = 0x17uy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Memory[0x100us] <- opcode
    cpu.Registers.A <- 0b10000000uy
    cpu.setFlag Flag.Carry false
    cpu.setFlag Flag.Zero true
    cpu.setFlag Flag.Subtract true
    cpu.setFlag Flag.HalfCarry true

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 1)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 1))

    Assert.That (cpu.Registers.A, Is.EqualTo 0b00000000uy)
    Assert.That (cpu.getFlag Flag.Zero, Is.False) // RLA always resets Z flag
    Assert.That (cpu.getFlag Flag.Subtract, Is.False)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.False)
    Assert.That (cpu.getFlag Flag.Carry, Is.True)

[<Test>]
let ``Rotate left accumulator, no carry - rla`` () =
    // Setup
    let opcode = 0x17uy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Memory[0x100us] <- opcode
    cpu.Registers.A <- 0b00000001uy
    cpu.setFlag Flag.Carry true
    cpu.setFlag Flag.Zero true
    cpu.setFlag Flag.Subtract true
    cpu.setFlag Flag.HalfCarry true

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 1)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 1))

    Assert.That (cpu.Registers.A, Is.EqualTo 0b00000011uy)
    Assert.That (cpu.getFlag Flag.Zero, Is.False)
    Assert.That (cpu.getFlag Flag.Subtract, Is.False)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.False)
    Assert.That (cpu.getFlag Flag.Carry, Is.False)
