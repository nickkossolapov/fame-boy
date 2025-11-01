module FameBoy.Test.InstructionTests.LogicTests

open FameBoy.Cpu.Execute
open FameBoy.Cpu.Opcodes
open FameBoy.Cpu.State
open FameBoy.Cpu.Instructions
open FameBoy.Memory
open NUnit.Framework


[<Test>]
let ``Bitwise AND A with B - and b`` () =
    // Setup
    let opcode = 0xA0uy
    let cpu = createCpu (createMemory [||])
    cpu.Pc <- 0x100us
    cpu.Memory[0x100us] <- opcode
    cpu.Registers.A <- 0x5Auy
    cpu.Registers.B <- 0x3Fuy

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 1)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 1))

    Assert.That (cpu.Registers.A, Is.EqualTo 0x1Auy)
    Assert.That (cpu.getFlag Flag.Zero, Is.False)
    Assert.That (cpu.getFlag Flag.Subtract, Is.False)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.True)
    Assert.That (cpu.getFlag Flag.Carry, Is.False)

[<Test>]
let ``Bitwise AND A with B (zero result) - and b`` () =
    // Setup
    let opcode = 0xA0uy
    let cpu = createCpu (createMemory [||])
    cpu.Pc <- 0x100us
    cpu.Memory[0x100us] <- opcode
    cpu.Registers.A <- 0x5Auy
    cpu.Registers.B <- 0xA5uy

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 1)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 1))

    Assert.That (cpu.Registers.A, Is.EqualTo 0x00uy)
    Assert.That (cpu.getFlag Flag.Zero, Is.True)
    Assert.That (cpu.getFlag Flag.Subtract, Is.False)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.True)
    Assert.That (cpu.getFlag Flag.Carry, Is.False)

[<Test>]
let ``Bitwise OR A with (HL) - or (hl)`` () =
    // Setup
    let opcode = 0xB6uy
    let cpu = createCpu (createMemory [||])
    cpu.Pc <- 0x100us
    cpu.Memory[0x100us] <- opcode
    cpu.Registers.A <- 0x5Auy
    cpu.Registers.HL <- 0xC050us
    cpu.Memory[0xC050us] <- 0x0Fuy

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 1)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 2))

    Assert.That (cpu.Registers.A, Is.EqualTo 0x5Fuy)
    Assert.That (cpu.getFlag Flag.Zero, Is.False)
    Assert.That (cpu.getFlag Flag.Subtract, Is.False)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.False)
    Assert.That (cpu.getFlag Flag.Carry, Is.False)

[<Test>]
let ``Bitwise OR A with (HL) (zero result) - or (hl)`` () =
    // Setup
    let opcode = 0xB6uy
    let cpu = createCpu (createMemory [||])
    cpu.Pc <- 0x100us
    cpu.Memory[0x100us] <- opcode
    cpu.Registers.A <- 0x00uy
    cpu.Registers.HL <- 0xC050us
    cpu.Memory[0xC050us] <- 0x00uy

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 1)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 2))

    Assert.That (cpu.Registers.A, Is.EqualTo 0x00uy)
    Assert.That (cpu.getFlag Flag.Zero, Is.True)
    Assert.That (cpu.getFlag Flag.Subtract, Is.False)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.False)
    Assert.That (cpu.getFlag Flag.Carry, Is.False)

[<Test>]
let ``Bitwise XOR A with n - xor n`` () =
    // Setup
    let opcode = 0xEEuy
    let cpu = createCpu (createMemory [||])
    cpu.Pc <- 0x100us
    cpu.Memory[0x100us] <- opcode
    cpu.Memory[0x101us] <- 0x3Fuy
    cpu.Registers.A <- 0x5Auy

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 2)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 2))

    Assert.That (cpu.Registers.A, Is.EqualTo 0x65uy)
    Assert.That (cpu.getFlag Flag.Zero, Is.False)
    Assert.That (cpu.getFlag Flag.Subtract, Is.False)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.False)
    Assert.That (cpu.getFlag Flag.Carry, Is.False)

[<Test>]
let ``Bitwise XOR A with n (zero result) - xor n`` () =
    // Setup
    let opcode = 0xEEuy
    let cpu = createCpu (createMemory [||])
    cpu.Pc <- 0x100us
    cpu.Memory[0x100us] <- opcode
    cpu.Memory[0x101us] <- 0x5Auy
    cpu.Registers.A <- 0x5Auy

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 2)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 2))

    Assert.That (cpu.Registers.A, Is.EqualTo 0x00uy)
    Assert.That (cpu.getFlag Flag.Zero, Is.True)
    Assert.That (cpu.getFlag Flag.Subtract, Is.False)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.False)
    Assert.That (cpu.getFlag Flag.Carry, Is.False)

[<Test>]
let ``Complement carry flag (carry initially true) - ccf`` () =
    // Setup
    let opcode = 0x3Fuy
    let cpu = createCpu (createMemory [||])
    cpu.Pc <- 0x100us
    cpu.Memory[0x100us] <- opcode
    cpu.setFlag Flag.Carry true
    cpu.setFlag Flag.Subtract true // should be cleared
    cpu.setFlag Flag.HalfCarry true // should be cleared
    cpu.setFlag Flag.Zero false // should be unaffected

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 1)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 1))

    Assert.That (cpu.getFlag Flag.Carry, Is.False)
    Assert.That (cpu.getFlag Flag.Subtract, Is.False)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.False)
    Assert.That (cpu.getFlag Flag.Zero, Is.False) // Unaffected

[<Test>]
let ``Complement carry flag (carry initially false) - ccf`` () =
    // Setup
    let opcode = 0x3Fuy
    let cpu = createCpu (createMemory [||])
    cpu.Pc <- 0x100us
    cpu.Memory[0x100us] <- opcode
    cpu.setFlag Flag.Carry false
    cpu.setFlag Flag.Subtract true // should be cleared
    cpu.setFlag Flag.HalfCarry true // should be cleared
    cpu.setFlag Flag.Zero true // should be unaffected

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 1)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 1))

    Assert.That (cpu.getFlag Flag.Carry, Is.True)
    Assert.That (cpu.getFlag Flag.Subtract, Is.False)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.False)
    Assert.That (cpu.getFlag Flag.Zero, Is.True) // Unaffected

[<Test>]
let ``Set carry flag - scf`` () =
    // Setup
    let opcode = 0x37uy
    let cpu = createCpu (createMemory [||])
    cpu.Pc <- 0x100us
    cpu.Memory[0x100us] <- opcode
    cpu.setFlag Flag.Carry false // should be set
    cpu.setFlag Flag.Subtract true // should be cleared
    cpu.setFlag Flag.HalfCarry true // should be cleared
    cpu.setFlag Flag.Zero true // should be unaffected

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 1)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 1))

    Assert.That (cpu.getFlag Flag.Carry, Is.True)
    Assert.That (cpu.getFlag Flag.Subtract, Is.False)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.False)
    Assert.That (cpu.getFlag Flag.Zero, Is.True) // Unaffected

[<Test>]
let ``Decimal adjust accumulator - daa`` () =
    // Setup
    let opcode = 0x27uy
    let cpu = createCpu (createMemory [||])
    cpu.Pc <- 0x100us
    cpu.Memory[0x100us] <- opcode
    cpu.Registers.A <- 0x1Auy
    cpu.setFlag Flag.Subtract false
    cpu.setFlag Flag.HalfCarry false
    cpu.setFlag Flag.Carry false

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 1)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 1))

    Assert.That (cpu.Registers.A, Is.EqualTo 0x20uy)
    Assert.That (cpu.getFlag Flag.Zero, Is.False)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.False)
    Assert.That (cpu.getFlag Flag.Carry, Is.False)

[<Test>]
let ``Complement accumulator - cpl`` () =
    // Setup
    let opcode = 0x2Fuy
    let cpu = createCpu (createMemory [||])
    cpu.Pc <- 0x100us
    cpu.Memory[0x100us] <- opcode
    cpu.Registers.A <- 0x5Auy
    cpu.setFlag Flag.Zero false // should be unaffected
    cpu.setFlag Flag.Carry true // should be unaffected

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That(instr.Length, Is.EqualTo 1)
    Assert.That(instr.MCycles, Is.EqualTo(Fixed 1))

    Assert.That(cpu.Registers.A, Is.EqualTo 0xA5uy)
    Assert.That(cpu.getFlag Flag.Subtract, Is.True)
    Assert.That(cpu.getFlag Flag.HalfCarry, Is.True)
    Assert.That(cpu.getFlag Flag.Zero, Is.False) // Unaffected
    Assert.That(cpu.getFlag Flag.Carry, Is.True) // Unaffected
