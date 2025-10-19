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
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.False)
    Assert.That (cpu.getFlag Flag.Carry, Is.True)

[<Test>]
let ``Add with carry from (HL) to A (no carry, no half-carry, no zero) - adc (hl)`` () =
    // Setup
    let opcode = 0x8Euy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Registers.HL <- 0xC000us
    cpu.Registers.A <- 0b00010010uy
    cpu.Memory[0xC000us] <- 0b00100011uy
    cpu.setFlag Flag.Zero true
    cpu.setFlag Flag.Subtract true
    cpu.setFlag Flag.HalfCarry true
    cpu.setFlag Flag.Carry false
    cpu.Memory[0x100us] <- opcode

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 1)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 2))

    Assert.That (cpu.Registers.A, Is.EqualTo 0b00110101uy)
    Assert.That (cpu.getFlag Flag.Zero, Is.False)
    Assert.That (cpu.getFlag Flag.Subtract, Is.False)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.False)
    Assert.That (cpu.getFlag Flag.Carry, Is.False)

[<Test>]
let ``Add with carry from (HL) to A (result zero) - adc (hl)`` () =
    // Setup
    let opcode = 0x8Euy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Registers.HL <- 0xC000us
    cpu.Registers.A <- 0b00000000uy
    cpu.Memory[0xC000us] <- 0b00000000uy
    cpu.setFlag Flag.Zero false
    cpu.setFlag Flag.Subtract true
    cpu.setFlag Flag.HalfCarry true
    cpu.setFlag Flag.Carry false
    cpu.Memory[0x100us] <- opcode

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 1)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 2))

    Assert.That (cpu.Registers.A, Is.EqualTo 0b00000000uy)
    Assert.That (cpu.getFlag Flag.Zero, Is.True)
    Assert.That (cpu.getFlag Flag.Subtract, Is.False)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.False)
    Assert.That (cpu.getFlag Flag.Carry, Is.False)

[<Test>]
let ``Add with carry from (HL) to A (half-carry) - adc (hl)`` () =
    // Setup
    let opcode = 0x8Euy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Registers.HL <- 0xC000us
    cpu.Registers.A <- 0b00001111uy
    cpu.Memory[0xC000us] <- 0b00000001uy
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
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 2))

    Assert.That (cpu.Registers.A, Is.EqualTo 0b00010000uy)
    Assert.That (cpu.getFlag Flag.Zero, Is.False)
    Assert.That (cpu.getFlag Flag.Subtract, Is.False)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.True)
    Assert.That (cpu.getFlag Flag.Carry, Is.False)

[<Test>]
let ``Add with carry from (HL) to A (carry) - adc (hl)`` () =
    // Setup
    let opcode = 0x8Euy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Registers.HL <- 0xC000us
    cpu.Registers.A <- 0b11110000uy
    cpu.Memory[0xC000us] <- 0b00010000uy
    cpu.setFlag Flag.Zero true
    cpu.setFlag Flag.Subtract true
    cpu.setFlag Flag.HalfCarry true
    cpu.setFlag Flag.Carry false
    cpu.Memory[0x100us] <- opcode

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 1)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 2))

    Assert.That (cpu.Registers.A, Is.EqualTo 0b00000000uy)
    Assert.That (cpu.getFlag Flag.Zero, Is.True)
    Assert.That (cpu.getFlag Flag.Subtract, Is.False)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.False)
    Assert.That (cpu.getFlag Flag.Carry, Is.True)

[<Test>]
let ``Add with carry from (HL) to A (with initial carry) - adc (hl)`` () =
    // Setup
    let opcode = 0x8Euy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Registers.HL <- 0xC000us
    cpu.Registers.A <- 0b00010010uy
    cpu.Memory[0xC000us] <- 0b00100011uy
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
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 2))

    Assert.That (cpu.Registers.A, Is.EqualTo 0b00110110uy)
    Assert.That (cpu.getFlag Flag.Zero, Is.False)
    Assert.That (cpu.getFlag Flag.Subtract, Is.False)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.False)
    Assert.That (cpu.getFlag Flag.Carry, Is.False)

[<Test>]
let ``Subtract immediate from A (no borrow, no half-borrow, no zero) - sub n`` () =
    // Setup
    let opcode = 0xD6uy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Registers.A <- 0x35uy
    cpu.Memory[0x100us] <- opcode
    cpu.Memory[0x101us] <- 0x12uy
    cpu.setFlag Flag.Zero true
    cpu.setFlag Flag.Subtract false
    cpu.setFlag Flag.HalfCarry true
    cpu.setFlag Flag.Carry true

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 2)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 2))

    Assert.That (cpu.Registers.A, Is.EqualTo 0x23uy)
    Assert.That (cpu.getFlag Flag.Zero, Is.False)
    Assert.That (cpu.getFlag Flag.Subtract, Is.True)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.False)
    Assert.That (cpu.getFlag Flag.Carry, Is.False)

[<Test>]
let ``Subtract immediate from A (result zero) - sub n`` () =
    // Setup
    let opcode = 0xD6uy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Registers.A <- 0x35uy
    cpu.Memory[0x100us] <- opcode
    cpu.Memory[0x101us] <- 0x35uy
    cpu.setFlag Flag.Zero false
    cpu.setFlag Flag.Subtract false
    cpu.setFlag Flag.HalfCarry true
    cpu.setFlag Flag.Carry true

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 2)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 2))

    Assert.That (cpu.Registers.A, Is.EqualTo 0x00uy)
    Assert.That (cpu.getFlag Flag.Zero, Is.True)
    Assert.That (cpu.getFlag Flag.Subtract, Is.True)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.False)
    Assert.That (cpu.getFlag Flag.Carry, Is.False)

[<Test>]
let ``Subtract immediate from A (half-borrow) - sub n`` () =
    // Setup
    let opcode = 0xD6uy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Registers.A <- 0x30uy
    cpu.Memory[0x100us] <- opcode
    cpu.Memory[0x101us] <- 0x01uy
    cpu.setFlag Flag.Zero true
    cpu.setFlag Flag.Subtract false
    cpu.setFlag Flag.HalfCarry false
    cpu.setFlag Flag.Carry true

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 2)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 2))

    Assert.That (cpu.Registers.A, Is.EqualTo 0x2Fuy)
    Assert.That (cpu.getFlag Flag.Zero, Is.False)
    Assert.That (cpu.getFlag Flag.Subtract, Is.True)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.True)
    Assert.That (cpu.getFlag Flag.Carry, Is.False)

[<Test>]
let ``Subtract immediate from A (borrow) - sub n`` () =
    // Setup
    let opcode = 0xD6uy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Registers.A <- 0x35uy
    cpu.Memory[0x100us] <- opcode
    cpu.Memory[0x101us] <- 0x40uy
    cpu.setFlag Flag.Zero true
    cpu.setFlag Flag.Subtract false
    cpu.setFlag Flag.HalfCarry true
    cpu.setFlag Flag.Carry false

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 2)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 2))

    Assert.That (cpu.Registers.A, Is.EqualTo 0xF5uy)
    Assert.That (cpu.getFlag Flag.Zero, Is.False)
    Assert.That (cpu.getFlag Flag.Subtract, Is.True)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.False)
    Assert.That (cpu.getFlag Flag.Carry, Is.True)

[<Test>]
let ``Subtract with carry from B to A (no borrow, no half-borrow, no zero) - sbc b`` () =
    // Setup
    let opcode = 0x98uy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Registers.A <- 0x35uy
    cpu.Registers.B <- 0x12uy
    cpu.setFlag Flag.Zero true
    cpu.setFlag Flag.Subtract false
    cpu.setFlag Flag.HalfCarry true
    cpu.setFlag Flag.Carry false
    cpu.Memory[0x100us] <- opcode

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 1)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 1))

    Assert.That (cpu.Registers.A, Is.EqualTo 0x23uy)
    Assert.That (cpu.getFlag Flag.Zero, Is.False)
    Assert.That (cpu.getFlag Flag.Subtract, Is.True)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.False)
    Assert.That (cpu.getFlag Flag.Carry, Is.False)

[<Test>]
let ``Subtract with carry from B to A (result zero) - sbc b`` () =
    // Setup
    let opcode = 0x98uy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Registers.A <- 0x35uy
    cpu.Registers.B <- 0x35uy
    cpu.setFlag Flag.Zero false
    cpu.setFlag Flag.Subtract false
    cpu.setFlag Flag.HalfCarry true
    cpu.setFlag Flag.Carry false
    cpu.Memory[0x100us] <- opcode

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 1)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 1))

    Assert.That (cpu.Registers.A, Is.EqualTo 0x00uy)
    Assert.That (cpu.getFlag Flag.Zero, Is.True)
    Assert.That (cpu.getFlag Flag.Subtract, Is.True)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.False)
    Assert.That (cpu.getFlag Flag.Carry, Is.False)

[<Test>]
let ``Subtract with carry from B to A (half-borrow) - sbc b`` () =
    // Setup
    let opcode = 0x98uy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Registers.A <- 0x30uy
    cpu.Registers.B <- 0x01uy
    cpu.setFlag Flag.Zero true
    cpu.setFlag Flag.Subtract false
    cpu.setFlag Flag.HalfCarry false
    cpu.setFlag Flag.Carry false
    cpu.Memory[0x100us] <- opcode

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 1)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 1))

    Assert.That (cpu.Registers.A, Is.EqualTo 0x2Fuy)
    Assert.That (cpu.getFlag Flag.Zero, Is.False)
    Assert.That (cpu.getFlag Flag.Subtract, Is.True)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.True)
    Assert.That (cpu.getFlag Flag.Carry, Is.False)

[<Test>]
let ``Subtract with carry from B to A (borrow) - sbc b`` () =
    // Setup
    let opcode = 0x98uy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Registers.A <- 0x35uy
    cpu.Registers.B <- 0x40uy
    cpu.setFlag Flag.Zero true
    cpu.setFlag Flag.Subtract false
    cpu.setFlag Flag.HalfCarry true
    cpu.setFlag Flag.Carry false
    cpu.Memory[0x100us] <- opcode

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 1)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 1))

    Assert.That (cpu.Registers.A, Is.EqualTo 0xF5uy)
    Assert.That (cpu.getFlag Flag.Zero, Is.False)
    Assert.That (cpu.getFlag Flag.Subtract, Is.True)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.False)
    Assert.That (cpu.getFlag Flag.Carry, Is.True)

[<Test>]
let ``Subtract with carry from B to A (with initial carry) - sbc b`` () =
    // Setup
    let opcode = 0x98uy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Registers.A <- 0x35uy
    cpu.Registers.B <- 0x12uy
    cpu.setFlag Flag.Zero true
    cpu.setFlag Flag.Subtract false
    cpu.setFlag Flag.HalfCarry true
    cpu.setFlag Flag.Carry true
    cpu.Memory[0x100us] <- opcode

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 1)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 1))

    Assert.That (cpu.Registers.A, Is.EqualTo 0x22uy)
    Assert.That (cpu.getFlag Flag.Zero, Is.False)
    Assert.That (cpu.getFlag Flag.Subtract, Is.True)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.False)
    Assert.That (cpu.getFlag Flag.Carry, Is.False)

[<Test>]
let ``Compare A with (HL) (A > (HL)) - cp (hl)`` () =
    // Setup
    let opcode = 0xBEuy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Registers.HL <- 0xC000us
    cpu.Registers.A <- 0x35uy
    cpu.Memory[0xC000us] <- 0x12uy
    cpu.setFlag Flag.Zero true
    cpu.setFlag Flag.Subtract false
    cpu.setFlag Flag.HalfCarry true
    cpu.setFlag Flag.Carry true
    cpu.Memory[0x100us] <- opcode

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 1)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 2))

    Assert.That (cpu.Registers.A, Is.EqualTo 0x35uy)
    Assert.That (cpu.getFlag Flag.Zero, Is.False)
    Assert.That (cpu.getFlag Flag.Subtract, Is.True)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.False)
    Assert.That (cpu.getFlag Flag.Carry, Is.False)

[<Test>]
let ``Compare A with (HL) (A == (HL)) - cp (hl)`` () =
    // Setup
    let opcode = 0xBEuy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Registers.HL <- 0xC000us
    cpu.Registers.A <- 0x35uy
    cpu.Memory[0xC000us] <- 0x35uy
    cpu.setFlag Flag.Zero false
    cpu.setFlag Flag.Subtract false
    cpu.setFlag Flag.HalfCarry true
    cpu.setFlag Flag.Carry true
    cpu.Memory[0x100us] <- opcode

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 1)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 2))

    Assert.That (cpu.Registers.A, Is.EqualTo 0x35uy)
    Assert.That (cpu.getFlag Flag.Zero, Is.True)
    Assert.That (cpu.getFlag Flag.Subtract, Is.True)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.False)
    Assert.That (cpu.getFlag Flag.Carry, Is.False)

[<Test>]
let ``Compare A with (HL) (half-borrow) - cp (hl)`` () =
    // Setup
    let opcode = 0xBEuy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Registers.HL <- 0xC000us
    cpu.Registers.A <- 0x30uy
    cpu.Memory[0xC000us] <- 0x01uy
    cpu.setFlag Flag.Zero true
    cpu.setFlag Flag.Subtract false
    cpu.setFlag Flag.HalfCarry false
    cpu.setFlag Flag.Carry true
    cpu.Memory[0x100us] <- opcode

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 1)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 2))

    Assert.That (cpu.Registers.A, Is.EqualTo 0x30uy)
    Assert.That (cpu.getFlag Flag.Zero, Is.False)
    Assert.That (cpu.getFlag Flag.Subtract, Is.True)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.True)
    Assert.That (cpu.getFlag Flag.Carry, Is.False)

[<Test>]
let ``Compare A with (HL) (borrow) - cp (hl)`` () =
    // Setup
    let opcode = 0xBEuy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Registers.HL <- 0xC000us
    cpu.Registers.A <- 0x35uy
    cpu.Memory[0xC000us] <- 0x40uy
    cpu.setFlag Flag.Zero true
    cpu.setFlag Flag.Subtract false
    cpu.setFlag Flag.HalfCarry true
    cpu.setFlag Flag.Carry false
    cpu.Memory[0x100us] <- opcode

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 1)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 2))

    Assert.That (cpu.Registers.A, Is.EqualTo 0x35uy)
    Assert.That (cpu.getFlag Flag.Zero, Is.False)
    Assert.That (cpu.getFlag Flag.Subtract, Is.True)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.False)
    Assert.That (cpu.getFlag Flag.Carry, Is.True)

[<Test>]
let ``Increment 8-bit register (no zero, no half-carry) - inc b`` () =
    // Setup
    let opcode = 0x04uy // INC B
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Registers.B <- 0x12uy
    cpu.setFlag Flag.Zero true
    cpu.setFlag Flag.Subtract true
    cpu.setFlag Flag.HalfCarry true
    cpu.setFlag Flag.Carry true // Carry flag should not be affected
    cpu.Memory[0x100us] <- opcode

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 1)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 1))

    Assert.That (cpu.Registers.B, Is.EqualTo 0x13uy)
    Assert.That (cpu.getFlag Flag.Zero, Is.False)
    Assert.That (cpu.getFlag Flag.Subtract, Is.False)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.False)
    Assert.That (cpu.getFlag Flag.Carry, Is.True) // Not affected

[<Test>]
let ``Increment 8-bit register (result zero) - inc b`` () =
    // Setup
    let opcode = 0x04uy // INC B
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Registers.B <- 0xFFuy
    cpu.setFlag Flag.Zero false
    cpu.setFlag Flag.Subtract true
    cpu.setFlag Flag.HalfCarry false
    cpu.setFlag Flag.Carry false // Carry flag should not be affected
    cpu.Memory[0x100us] <- opcode

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 1)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 1))

    Assert.That (cpu.Registers.B, Is.EqualTo 0x00uy)
    Assert.That (cpu.getFlag Flag.Zero, Is.True)
    Assert.That (cpu.getFlag Flag.Subtract, Is.False)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.True)
    Assert.That (cpu.getFlag Flag.Carry, Is.False) // Not affected

[<Test>]
let ``Increment 8-bit register (half-carry) - inc b`` () =
    // Setup
    let opcode = 0x04uy // INC B
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Registers.B <- 0x0Fuy
    cpu.setFlag Flag.Zero true
    cpu.setFlag Flag.Subtract true
    cpu.setFlag Flag.HalfCarry false
    cpu.setFlag Flag.Carry true // Carry flag should not be affected
    cpu.Memory[0x100us] <- opcode

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 1)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 1))

    Assert.That (cpu.Registers.B, Is.EqualTo 0x10uy)
    Assert.That (cpu.getFlag Flag.Zero, Is.False)
    Assert.That (cpu.getFlag Flag.Subtract, Is.False)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.True)
    Assert.That (cpu.getFlag Flag.Carry, Is.True) // Not affected

[<Test>]
let ``Increment 8-bit register (all flags) - inc a`` () =
    // Setup
    let opcode = 0x3Cuy // INC A
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Registers.A <- 0xFFuy
    cpu.setFlag Flag.Zero false
    cpu.setFlag Flag.Subtract true
    cpu.setFlag Flag.HalfCarry false
    cpu.setFlag Flag.Carry false // Carry flag should not be affected
    cpu.Memory[0x100us] <- opcode

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
    Assert.That (cpu.getFlag Flag.Carry, Is.False) // Not affected

[<Test>]
let ``Increment (HL) (no zero, no half-carry) - inc (hl)`` () =
    // Setup
    let opcode = 0x34uy // INC (HL)
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Registers.HL <- 0xC000us
    cpu.Memory[0xC000us] <- 0x12uy
    cpu.setFlag Flag.Zero true
    cpu.setFlag Flag.Subtract true
    cpu.setFlag Flag.HalfCarry true
    cpu.setFlag Flag.Carry true // Carry flag should not be affected
    cpu.Memory[0x100us] <- opcode

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 1)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 3))

    Assert.That (cpu.Memory[0xC000us], Is.EqualTo 0x13uy)
    Assert.That (cpu.getFlag Flag.Zero, Is.False)
    Assert.That (cpu.getFlag Flag.Subtract, Is.False)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.False)
    Assert.That (cpu.getFlag Flag.Carry, Is.True) // Not affected

[<Test>]
let ``Increment (HL) (result zero) - inc (hl)`` () =
    // Setup
    let opcode = 0x34uy // INC (HL)
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Registers.HL <- 0xC000us
    cpu.Memory[0xC000us] <- 0xFFuy
    cpu.setFlag Flag.Zero false
    cpu.setFlag Flag.Subtract true
    cpu.setFlag Flag.HalfCarry false
    cpu.setFlag Flag.Carry false // Carry flag should not be affected
    cpu.Memory[0x100us] <- opcode

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 1)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 3))

    Assert.That (cpu.Memory[0xC000us], Is.EqualTo 0x00uy)
    Assert.That (cpu.getFlag Flag.Zero, Is.True)
    Assert.That (cpu.getFlag Flag.Subtract, Is.False)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.True)
    Assert.That (cpu.getFlag Flag.Carry, Is.False) // Not affected

[<Test>]
let ``Increment (HL) (half-carry) - inc (hl)`` () =
    // Setup
    let opcode = 0x34uy // INC (HL)
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Registers.HL <- 0xC000us
    cpu.Memory[0xC000us] <- 0x0Fuy
    cpu.setFlag Flag.Zero true
    cpu.setFlag Flag.Subtract true
    cpu.setFlag Flag.HalfCarry false
    cpu.setFlag Flag.Carry true // Carry flag should not be affected
    cpu.Memory[0x100us] <- opcode

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 1)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 3))

    Assert.That (cpu.Memory[0xC000us], Is.EqualTo 0x10uy)
    Assert.That (cpu.getFlag Flag.Zero, Is.False)
    Assert.That (cpu.getFlag Flag.Subtract, Is.False)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.True)
    Assert.That (cpu.getFlag Flag.Carry, Is.True) // Not affected

[<Test>]
let ``Decrement 8-bit register (no zero, no half-carry) - dec b`` () =
    // Setup
    let opcode = 0x05uy // DEC B
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Registers.B <- 0x13uy
    cpu.setFlag Flag.Zero true
    cpu.setFlag Flag.Subtract false
    cpu.setFlag Flag.HalfCarry true
    cpu.setFlag Flag.Carry true // Carry flag should not be affected
    cpu.Memory[0x100us] <- opcode

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 1)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 1))

    Assert.That (cpu.Registers.B, Is.EqualTo 0x12uy)
    Assert.That (cpu.getFlag Flag.Zero, Is.False)
    Assert.That (cpu.getFlag Flag.Subtract, Is.True)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.False)
    Assert.That (cpu.getFlag Flag.Carry, Is.True) // Not affected

[<Test>]
let ``Decrement 8-bit register (result zero) - dec b`` () =
    // Setup
    let opcode = 0x05uy // DEC B
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Registers.B <- 0x01uy
    cpu.setFlag Flag.Zero false
    cpu.setFlag Flag.Subtract false
    cpu.setFlag Flag.HalfCarry true
    cpu.setFlag Flag.Carry false // Carry flag should not be affected
    cpu.Memory[0x100us] <- opcode

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 1)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 1))

    Assert.That (cpu.Registers.B, Is.EqualTo 0x00uy)
    Assert.That (cpu.getFlag Flag.Zero, Is.True)
    Assert.That (cpu.getFlag Flag.Subtract, Is.True)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.False)
    Assert.That (cpu.getFlag Flag.Carry, Is.False) // Not affected

[<Test>]
let ``Decrement 8-bit register (half-carry) - dec b`` () =
    // Setup
    let opcode = 0x05uy // DEC B
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Registers.B <- 0x10uy
    cpu.setFlag Flag.Zero true
    cpu.setFlag Flag.Subtract false
    cpu.setFlag Flag.HalfCarry false
    cpu.setFlag Flag.Carry true // Carry flag should not be affected
    cpu.Memory[0x100us] <- opcode

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 1)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 1))

    Assert.That (cpu.Registers.B, Is.EqualTo 0x0Fuy)
    Assert.That (cpu.getFlag Flag.Zero, Is.False)
    Assert.That (cpu.getFlag Flag.Subtract, Is.True)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.True)
    Assert.That (cpu.getFlag Flag.Carry, Is.True) // Not affected

[<Test>]
let ``Decrement 8-bit register (from 0x00) - dec a`` () =
    // Setup
    let opcode = 0x3Duy // DEC A
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Registers.A <- 0x00uy
    cpu.setFlag Flag.Zero false
    cpu.setFlag Flag.Subtract false
    cpu.setFlag Flag.HalfCarry false
    cpu.setFlag Flag.Carry false // Carry flag should not be affected
    cpu.Memory[0x100us] <- opcode

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 1)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 1))

    Assert.That (cpu.Registers.A, Is.EqualTo 0xFFuy)
    Assert.That (cpu.getFlag Flag.Zero, Is.False)
    Assert.That (cpu.getFlag Flag.Subtract, Is.True)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.True)
    Assert.That (cpu.getFlag Flag.Carry, Is.False) // Not affected

[<Test>]
let ``Decrement value at address HL (no zero, no half-carry) - dec (hl)`` () =
    // Setup
    let opcode = 0x35uy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Registers.HL <- 0xC000us
    cpu.Memory[0x100us] <- opcode
    cpu.Memory[0xC000us] <- 0x51uy
    cpu.setFlag Flag.Zero true
    cpu.setFlag Flag.Subtract false
    cpu.setFlag Flag.HalfCarry true
    cpu.setFlag Flag.Carry true

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 1)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 3))

    Assert.That (cpu.Memory[0xC000us], Is.EqualTo 0x50uy)
    Assert.That (cpu.getFlag Flag.Zero, Is.False)
    Assert.That (cpu.getFlag Flag.Subtract, Is.True)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.False)
    Assert.That (cpu.getFlag Flag.Carry, Is.True) // Unchanged

[<Test>]
let ``Decrement value at address HL (result zero) - dec (hl)`` () =
    // Setup
    let opcode = 0x35uy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Registers.HL <- 0xC000us
    cpu.Memory[0x100us] <- opcode
    cpu.Memory[0xC000us] <- 0x01uy
    cpu.setFlag Flag.Zero false
    cpu.setFlag Flag.Subtract false
    cpu.setFlag Flag.HalfCarry true
    cpu.setFlag Flag.Carry false

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 1)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 3))

    Assert.That (cpu.Memory[0xC000us], Is.EqualTo 0x00uy)
    Assert.That (cpu.getFlag Flag.Zero, Is.True)
    Assert.That (cpu.getFlag Flag.Subtract, Is.True)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.False)
    Assert.That (cpu.getFlag Flag.Carry, Is.False) // Unchanged

[<Test>]
let ``Decrement value at address HL (half-carry) - dec (hl)`` () =
    // Setup
    let opcode = 0x35uy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Registers.HL <- 0xC000us
    cpu.Memory[0x100us] <- opcode
    cpu.Memory[0xC000us] <- 0x10uy
    cpu.setFlag Flag.Zero true
    cpu.setFlag Flag.Subtract false
    cpu.setFlag Flag.HalfCarry false
    cpu.setFlag Flag.Carry true

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 1)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 3))

    Assert.That (cpu.Memory[0xC000us], Is.EqualTo 0x0Fuy)
    Assert.That (cpu.getFlag Flag.Zero, Is.False)
    Assert.That (cpu.getFlag Flag.Subtract, Is.True)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.True)
    Assert.That (cpu.getFlag Flag.Carry, Is.True) // Unchanged

[<Test>]
let ``Decrement value at address HL (wrap around) - dec (hl)`` () =
    // Setup
    let opcode = 0x35uy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Registers.HL <- 0xC000us
    cpu.Memory[0x100us] <- opcode
    cpu.Memory[0xC000us] <- 0x00uy
    cpu.setFlag Flag.Zero true
    cpu.setFlag Flag.Subtract false
    cpu.setFlag Flag.HalfCarry false
    cpu.setFlag Flag.Carry false

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 1)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 3))

    Assert.That (cpu.Memory[0xC000us], Is.EqualTo 0xFFuy)
    Assert.That (cpu.getFlag Flag.Zero, Is.False)
    Assert.That (cpu.getFlag Flag.Subtract, Is.True)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.True)
    Assert.That (cpu.getFlag Flag.Carry, Is.False) // Unchanged

[<Test>]
let ``Increment 16-bit register BC - inc bc`` () =
    // Setup
    let opcode = 0x03uy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Registers.BC <- 0x1234us
    cpu.Memory[0x100us] <- opcode

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 1)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 2))
    Assert.That (cpu.Registers.BC, Is.EqualTo 0x1235us)

[<Test>]
let ``Increment 16-bit register BC (wrap around) - inc bc`` () =
    // Setup
    let opcode = 0x03uy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Registers.BC <- 0xFFFFus
    cpu.Memory[0x100us] <- opcode

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 1)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 2))
    Assert.That (cpu.Registers.BC, Is.EqualTo 0x0000us)


[<Test>]
let ``Decrement 16-bit register BC - dec bc`` () =
    // Setup
    let opcode = 0x0Buy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Registers.BC <- 0x1234us
    cpu.Memory[0x100us] <- opcode

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 1)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 2))
    Assert.That (cpu.Registers.BC, Is.EqualTo 0x1233us)

[<Test>]
let ``Decrement 16-bit register BC (wrap around) - dec bc`` () =
    // Setup
    let opcode = 0x0Buy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Registers.BC <- 0x0000us
    cpu.Memory[0x100us] <- opcode

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 1)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 2))
    Assert.That (cpu.Registers.BC, Is.EqualTo 0xFFFFus)

[<Test>]
let ``Add 16-bit register BC to HL (no carry, no half-carry) - add hl,bc`` () =
    // Setup
    let opcode = 0x09uy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Registers.HL <- 0x1234us
    cpu.Registers.BC <- 0x0102us
    cpu.Memory[0x100us] <- opcode
    cpu.setFlag Flag.Subtract true
    cpu.setFlag Flag.HalfCarry false
    cpu.setFlag Flag.Carry false

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 1)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 2))
    Assert.That (cpu.Registers.HL, Is.EqualTo 0x1336us)
    Assert.That (cpu.getFlag Flag.Subtract, Is.False)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.False)
    Assert.That (cpu.getFlag Flag.Carry, Is.False)

[<Test>]
let ``Add 16-bit register BC to HL (half carry) - add hl,bc`` () =
    // Setup
    let opcode = 0x09uy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Registers.HL <- 0x0FFFus
    cpu.Registers.BC <- 0x0001us
    cpu.Memory[0x100us] <- opcode
    cpu.setFlag Flag.Subtract true
    cpu.setFlag Flag.HalfCarry false
    cpu.setFlag Flag.Carry true

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 1)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 2))
    Assert.That (cpu.Registers.HL, Is.EqualTo 0x1000us)
    Assert.That (cpu.getFlag Flag.Subtract, Is.False)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.True)
    Assert.That (cpu.getFlag Flag.Carry, Is.False)

[<Test>]
let ``Add 16-bit register BC to HL (carry) - add hl,bc`` () =
    // Setup
    let opcode = 0x09uy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Registers.HL <- 0xFFFFus
    cpu.Registers.BC <- 0x0001us
    cpu.Memory[0x100us] <- opcode
    cpu.setFlag Flag.Subtract true
    cpu.setFlag Flag.HalfCarry true
    cpu.setFlag Flag.Carry false

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 1)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 2))
    Assert.That (cpu.Registers.HL, Is.EqualTo 0x0000us)
    Assert.That (cpu.getFlag Flag.Subtract, Is.False)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.True)
    Assert.That (cpu.getFlag Flag.Carry, Is.True)

[<Test>]
let ``Add 16-bit register BC to HL (half carry and carry) - add hl,bc`` () =
    // Setup
    let opcode = 0x09uy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Registers.HL <- 0x0FFFus
    cpu.Registers.BC <- 0xF001us
    cpu.Memory[0x100us] <- opcode
    cpu.setFlag Flag.Subtract true
    cpu.setFlag Flag.HalfCarry false
    cpu.setFlag Flag.Carry false

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 1)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 2))
    Assert.That (cpu.Registers.HL, Is.EqualTo 0x0000us)
    Assert.That (cpu.getFlag Flag.Subtract, Is.False)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.True)
    Assert.That (cpu.getFlag Flag.Carry, Is.True)

[<Test>]
let ``Add signed immediate to SP (positive, no carry) - add sp, e`` () =
    // Setup
    let opcode = 0xE8uy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Sp <- 0x1234us
    cpu.Memory[0x100us] <- opcode
    cpu.Memory[0x101us] <- 0x10uy // e = 16
    cpu.setFlag Flag.Zero true
    cpu.setFlag Flag.Subtract true
    cpu.setFlag Flag.HalfCarry true
    cpu.setFlag Flag.Carry true

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 2)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 4))
    Assert.That (cpu.Sp, Is.EqualTo 0x1244us)
    Assert.That (cpu.getFlag Flag.Zero, Is.False)
    Assert.That (cpu.getFlag Flag.Subtract, Is.False)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.False)
    Assert.That (cpu.getFlag Flag.Carry, Is.False)

[<Test>]
let ``Add signed immediate to SP (positive, half carry) - add sp, e`` () =
    // Setup
    let opcode = 0xE8uy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Sp <- 0x120Fus
    cpu.Memory[0x100us] <- opcode
    cpu.Memory[0x101us] <- 0x01uy // e = 1
    cpu.setFlag Flag.Zero true
    cpu.setFlag Flag.Subtract true
    cpu.setFlag Flag.HalfCarry false
    cpu.setFlag Flag.Carry true

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 2)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 4))
    Assert.That (cpu.Sp, Is.EqualTo 0x1210us)
    Assert.That (cpu.getFlag Flag.Zero, Is.False)
    Assert.That (cpu.getFlag Flag.Subtract, Is.False)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.True)
    Assert.That (cpu.getFlag Flag.Carry, Is.False)

[<Test>]
let ``Add signed immediate to SP (positive, carry) - add sp, e`` () =
    // Setup
    let opcode = 0xE8uy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Sp <- 0x12FFus
    cpu.Memory[0x100us] <- opcode
    cpu.Memory[0x101us] <- 0x01uy // e = 1
    cpu.setFlag Flag.Zero true
    cpu.setFlag Flag.Subtract true
    cpu.setFlag Flag.HalfCarry false
    cpu.setFlag Flag.Carry false

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 2)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 4))
    Assert.That (cpu.Sp, Is.EqualTo 0x1300us)
    Assert.That (cpu.getFlag Flag.Zero, Is.False)
    Assert.That (cpu.getFlag Flag.Subtract, Is.False)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.True)
    Assert.That (cpu.getFlag Flag.Carry, Is.True)


[<Test>]
let ``Add signed immediate to SP (negative, no borrow) - add sp, e`` () =
    // Setup
    let opcode = 0xE8uy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Sp <- 0x1200us
    cpu.Memory[0x100us] <- opcode
    cpu.Memory[0x101us] <- 0xFFuy
    cpu.setFlag Flag.Zero true
    cpu.setFlag Flag.Subtract true
    cpu.setFlag Flag.HalfCarry true
    cpu.setFlag Flag.Carry true

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 2)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 4))
    Assert.That (cpu.Sp, Is.EqualTo 0x11FFus)
    Assert.That (cpu.getFlag Flag.Zero, Is.False)
    Assert.That (cpu.getFlag Flag.Subtract, Is.False)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.False)
    Assert.That (cpu.getFlag Flag.Carry, Is.False)

[<Test>]
let ``Add signed immediate to SP (negative, half borrow) - add sp, e`` () =
    // Setup
    let opcode = 0xE8uy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Sp <- 0x1204us
    cpu.Memory[0x100us] <- opcode
    cpu.Memory[0x101us] <- 0xEFuy
    cpu.setFlag Flag.Zero true
    cpu.setFlag Flag.Subtract true
    cpu.setFlag Flag.HalfCarry false
    cpu.setFlag Flag.Carry false

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 2)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 4))
    Assert.That (cpu.Sp, Is.EqualTo 0x11F3us)
    Assert.That (cpu.getFlag Flag.Zero, Is.False)
    Assert.That (cpu.getFlag Flag.Subtract, Is.False)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.True)
    Assert.That (cpu.getFlag Flag.Carry, Is.False)

[<Test>]
let ``Add signed immediate to SP (negative, carry, no half borrow) - add sp, e`` () =
    // Setup
    let opcode = 0xE8uy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Sp <- 0x1210us
    cpu.Memory[0x100us] <- opcode
    cpu.Memory[0x101us] <- 0xFFuy
    cpu.setFlag Flag.Zero true
    cpu.setFlag Flag.Subtract true
    cpu.setFlag Flag.HalfCarry false
    cpu.setFlag Flag.Carry false

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 2)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 4))
    Assert.That (cpu.Sp, Is.EqualTo 0x120Fus)
    Assert.That (cpu.getFlag Flag.Zero, Is.False)
    Assert.That (cpu.getFlag Flag.Subtract, Is.False)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.False)
    Assert.That (cpu.getFlag Flag.Carry, Is.True)

[<Test>]
let ``Add signed immediate to SP (negative, carry) - add sp, e`` () =
    // Setup
    let opcode = 0xE8uy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Sp <- 0x1201us
    cpu.Memory[0x100us] <- opcode
    cpu.Memory[0x101us] <- 0xFFuy
    cpu.setFlag Flag.Zero true
    cpu.setFlag Flag.Subtract true
    cpu.setFlag Flag.HalfCarry true
    cpu.setFlag Flag.Carry true

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 2)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 4))
    Assert.That (cpu.Sp, Is.EqualTo 0x1200us)
    Assert.That (cpu.getFlag Flag.Zero, Is.False)
    Assert.That (cpu.getFlag Flag.Subtract, Is.False)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.True)
    Assert.That (cpu.getFlag Flag.Carry, Is.True)
