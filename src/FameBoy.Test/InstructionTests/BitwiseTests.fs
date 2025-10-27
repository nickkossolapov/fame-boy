module FameBoy.Test.InstructionTests.BitwiseTests

open FameBoy.Cpu.Execute
open FameBoy.Cpu.Instructions
open FameBoy.Cpu.Opcodes
open FameBoy.Cpu.State
open NUnit.Framework

let twoBitPrefix = 0xCBuy


[<Test>]
let ``Rotate left circular accumulator with bit 7 set - rlca`` () =
    // Setup
    let opcode = 0x07uy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Memory[0x100us] <- opcode
    cpu.Registers.A <- 0b10101010uy
    cpu.setFlag Flag.Zero true
    cpu.setFlag Flag.Subtract true
    cpu.setFlag Flag.HalfCarry true
    cpu.setFlag Flag.Carry false

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 1)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 1))

    Assert.That (cpu.Registers.A, Is.EqualTo 0b01010101uy)
    Assert.That (cpu.getFlag Flag.Zero, Is.False)
    Assert.That (cpu.getFlag Flag.Subtract, Is.False)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.False)
    Assert.That (cpu.getFlag Flag.Carry, Is.True)

[<Test>]
let ``Rotate left circular accumulator with bit 7 not set - rlca`` () =
    // Setup
    let opcode = 0x07uy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Memory[0x100us] <- opcode
    cpu.Registers.A <- 0b01010101uy
    cpu.setFlag Flag.Zero true
    cpu.setFlag Flag.Subtract true
    cpu.setFlag Flag.HalfCarry true
    cpu.setFlag Flag.Carry true

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 1)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 1))

    Assert.That (cpu.Registers.A, Is.EqualTo 0b10101010uy)
    Assert.That (cpu.getFlag Flag.Zero, Is.False)
    Assert.That (cpu.getFlag Flag.Subtract, Is.False)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.False)
    Assert.That (cpu.getFlag Flag.Carry, Is.False)

[<Test>]
let ``Rotate left circular accumulator all ones - rlca`` () =
    // Setup
    let opcode = 0x07uy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Memory[0x100us] <- opcode
    cpu.Registers.A <- 0b11111111uy
    cpu.setFlag Flag.Zero true
    cpu.setFlag Flag.Subtract true
    cpu.setFlag Flag.HalfCarry true
    cpu.setFlag Flag.Carry false

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 1)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 1))

    Assert.That (cpu.Registers.A, Is.EqualTo 0b11111111uy)
    Assert.That (cpu.getFlag Flag.Zero, Is.False)
    Assert.That (cpu.getFlag Flag.Subtract, Is.False)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.False)
    Assert.That (cpu.getFlag Flag.Carry, Is.True)

[<Test>]
let ``Rotate left circular accumulator all zeros - rlca`` () =
    // Setup
    let opcode = 0x07uy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Memory[0x100us] <- opcode
    cpu.Registers.A <- 0b00000000uy
    cpu.setFlag Flag.Zero true
    cpu.setFlag Flag.Subtract true
    cpu.setFlag Flag.HalfCarry true
    cpu.setFlag Flag.Carry true

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 1)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 1))

    Assert.That (cpu.Registers.A, Is.EqualTo 0b00000000uy)
    Assert.That (cpu.getFlag Flag.Zero, Is.False)
    Assert.That (cpu.getFlag Flag.Subtract, Is.False)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.False)
    Assert.That (cpu.getFlag Flag.Carry, Is.False)

[<Test>]
let ``Rotate right circular accumulator with bit 0 set - rrca`` () =
    // Setup
    let opcode = 0x0Fuy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Memory[0x100us] <- opcode
    cpu.Registers.A <- 0b10101011uy
    cpu.setFlag Flag.Zero true
    cpu.setFlag Flag.Subtract true
    cpu.setFlag Flag.HalfCarry true
    cpu.setFlag Flag.Carry false

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 1)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 1))

    Assert.That (cpu.Registers.A, Is.EqualTo 0b11010101uy)
    Assert.That (cpu.getFlag Flag.Zero, Is.False)
    Assert.That (cpu.getFlag Flag.Subtract, Is.False)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.False)
    Assert.That (cpu.getFlag Flag.Carry, Is.True)

[<Test>]
let ``Rotate right circular accumulator with bit 0 not set - rrca`` () =
    // Setup
    let opcode = 0x0Fuy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Memory[0x100us] <- opcode
    cpu.Registers.A <- 0b10101010uy
    cpu.setFlag Flag.Zero true
    cpu.setFlag Flag.Subtract true
    cpu.setFlag Flag.HalfCarry true
    cpu.setFlag Flag.Carry true

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 1)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 1))

    Assert.That (cpu.Registers.A, Is.EqualTo 0b01010101uy)
    Assert.That (cpu.getFlag Flag.Zero, Is.False)
    Assert.That (cpu.getFlag Flag.Subtract, Is.False)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.False)
    Assert.That (cpu.getFlag Flag.Carry, Is.False)

[<Test>]
let ``Rotate right circular accumulator all ones - rrca`` () =
    // Setup
    let opcode = 0x0Fuy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Memory[0x100us] <- opcode
    cpu.Registers.A <- 0b11111111uy
    cpu.setFlag Flag.Zero true
    cpu.setFlag Flag.Subtract true
    cpu.setFlag Flag.HalfCarry true
    cpu.setFlag Flag.Carry false

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 1)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 1))

    Assert.That (cpu.Registers.A, Is.EqualTo 0b11111111uy)
    Assert.That (cpu.getFlag Flag.Zero, Is.False)
    Assert.That (cpu.getFlag Flag.Subtract, Is.False)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.False)
    Assert.That (cpu.getFlag Flag.Carry, Is.True)

[<Test>]
let ``Rotate right circular accumulator all zeros - rrca`` () =
    // Setup
    let opcode = 0x0Fuy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Memory[0x100us] <- opcode
    cpu.Registers.A <- 0b00000000uy
    cpu.setFlag Flag.Zero true
    cpu.setFlag Flag.Subtract true
    cpu.setFlag Flag.HalfCarry true
    cpu.setFlag Flag.Carry true

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 1)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 1))

    Assert.That (cpu.Registers.A, Is.EqualTo 0b00000000uy)
    Assert.That (cpu.getFlag Flag.Zero, Is.False)
    Assert.That (cpu.getFlag Flag.Subtract, Is.False)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.False)
    Assert.That (cpu.getFlag Flag.Carry, Is.False)

[<Test>]
let ``Rotate left accumulator with bit 7 set and carry set - rla`` () =
    // Setup
    let opcode = 0x17uy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Memory[0x100us] <- opcode
    cpu.Registers.A <- 0b10101010uy
    cpu.setFlag Flag.Zero true
    cpu.setFlag Flag.Subtract true
    cpu.setFlag Flag.HalfCarry true
    cpu.setFlag Flag.Carry true

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 1)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 1))

    Assert.That (cpu.Registers.A, Is.EqualTo 0b01010101uy)
    Assert.That (cpu.getFlag Flag.Zero, Is.False)
    Assert.That (cpu.getFlag Flag.Subtract, Is.False)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.False)
    Assert.That (cpu.getFlag Flag.Carry, Is.True)

[<Test>]
let ``Rotate left accumulator with bit 7 set and carry clear - rla`` () =
    // Setup
    let opcode = 0x17uy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Memory[0x100us] <- opcode
    cpu.Registers.A <- 0b10101010uy
    cpu.setFlag Flag.Zero true
    cpu.setFlag Flag.Subtract true
    cpu.setFlag Flag.HalfCarry true
    cpu.setFlag Flag.Carry false

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 1)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 1))

    Assert.That (cpu.Registers.A, Is.EqualTo 0b01010100uy)
    Assert.That (cpu.getFlag Flag.Zero, Is.False)
    Assert.That (cpu.getFlag Flag.Subtract, Is.False)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.False)
    Assert.That (cpu.getFlag Flag.Carry, Is.True)

[<Test>]
let ``Rotate left accumulator with bit 7 clear and carry set - rla`` () =
    // Setup
    let opcode = 0x17uy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Memory[0x100us] <- opcode
    cpu.Registers.A <- 0b01010101uy
    cpu.setFlag Flag.Zero true
    cpu.setFlag Flag.Subtract true
    cpu.setFlag Flag.HalfCarry true
    cpu.setFlag Flag.Carry true

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 1)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 1))

    Assert.That (cpu.Registers.A, Is.EqualTo 0b10101011uy)
    Assert.That (cpu.getFlag Flag.Zero, Is.False)
    Assert.That (cpu.getFlag Flag.Subtract, Is.False)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.False)
    Assert.That (cpu.getFlag Flag.Carry, Is.False)

[<Test>]
let ``Rotate left accumulator with bit 7 clear and carry clear - rla`` () =
    // Setup
    let opcode = 0x17uy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Memory[0x100us] <- opcode
    cpu.Registers.A <- 0b01010101uy
    cpu.setFlag Flag.Zero true
    cpu.setFlag Flag.Subtract true
    cpu.setFlag Flag.HalfCarry true
    cpu.setFlag Flag.Carry false

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 1)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 1))

    Assert.That (cpu.Registers.A, Is.EqualTo 0b10101010uy)
    Assert.That (cpu.getFlag Flag.Zero, Is.False)
    Assert.That (cpu.getFlag Flag.Subtract, Is.False)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.False)
    Assert.That (cpu.getFlag Flag.Carry, Is.False)

[<Test>]
let ``Rotate left accumulator all zeros with carry set - rla`` () =
    // Setup
    let opcode = 0x17uy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Memory[0x100us] <- opcode
    cpu.Registers.A <- 0b00000000uy
    cpu.setFlag Flag.Zero true
    cpu.setFlag Flag.Subtract true
    cpu.setFlag Flag.HalfCarry true
    cpu.setFlag Flag.Carry true

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 1)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 1))

    Assert.That (cpu.Registers.A, Is.EqualTo 0b00000001uy)
    Assert.That (cpu.getFlag Flag.Zero, Is.False)
    Assert.That (cpu.getFlag Flag.Subtract, Is.False)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.False)
    Assert.That (cpu.getFlag Flag.Carry, Is.False)

[<Test>]
let ``Rotate left accumulator all zeros with carry clear - rla`` () =
    // Setup
    let opcode = 0x17uy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Memory[0x100us] <- opcode
    cpu.Registers.A <- 0b00000000uy
    cpu.setFlag Flag.Zero true
    cpu.setFlag Flag.Subtract true
    cpu.setFlag Flag.HalfCarry true
    cpu.setFlag Flag.Carry false

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 1)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 1))

    Assert.That (cpu.Registers.A, Is.EqualTo 0b00000000uy)
    Assert.That (cpu.getFlag Flag.Zero, Is.False)
    Assert.That (cpu.getFlag Flag.Subtract, Is.False)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.False)
    Assert.That (cpu.getFlag Flag.Carry, Is.False)

[<Test>]
let ``Rotate right accumulator with bit 0 set and carry set - rra`` () =
    // Setup
    let opcode = 0x1Fuy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Memory[0x100us] <- opcode
    cpu.Registers.A <- 0b10101011uy
    cpu.setFlag Flag.Zero true
    cpu.setFlag Flag.Subtract true
    cpu.setFlag Flag.HalfCarry true
    cpu.setFlag Flag.Carry true

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 1)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 1))

    Assert.That (cpu.Registers.A, Is.EqualTo 0b11010101uy)
    Assert.That (cpu.getFlag Flag.Zero, Is.False)
    Assert.That (cpu.getFlag Flag.Subtract, Is.False)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.False)
    Assert.That (cpu.getFlag Flag.Carry, Is.True)

[<Test>]
let ``Rotate right accumulator with bit 0 set and carry clear - rra`` () =
    // Setup
    let opcode = 0x1Fuy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Memory[0x100us] <- opcode
    cpu.Registers.A <- 0b10101011uy
    cpu.setFlag Flag.Zero true
    cpu.setFlag Flag.Subtract true
    cpu.setFlag Flag.HalfCarry true
    cpu.setFlag Flag.Carry false

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 1)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 1))

    Assert.That (cpu.Registers.A, Is.EqualTo 0b01010101uy)
    Assert.That (cpu.getFlag Flag.Zero, Is.False)
    Assert.That (cpu.getFlag Flag.Subtract, Is.False)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.False)
    Assert.That (cpu.getFlag Flag.Carry, Is.True)

[<Test>]
let ``Rotate right accumulator with bit 0 clear and carry set - rra`` () =
    // Setup
    let opcode = 0x1Fuy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Memory[0x100us] <- opcode
    cpu.Registers.A <- 0b10101010uy
    cpu.setFlag Flag.Zero true
    cpu.setFlag Flag.Subtract true
    cpu.setFlag Flag.HalfCarry true
    cpu.setFlag Flag.Carry true

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 1)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 1))

    Assert.That (cpu.Registers.A, Is.EqualTo 0b11010101uy)
    Assert.That (cpu.getFlag Flag.Zero, Is.False)
    Assert.That (cpu.getFlag Flag.Subtract, Is.False)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.False)
    Assert.That (cpu.getFlag Flag.Carry, Is.False)

[<Test>]
let ``Rotate right accumulator with bit 0 clear and carry clear - rra`` () =
    // Setup
    let opcode = 0x1Fuy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Memory[0x100us] <- opcode
    cpu.Registers.A <- 0b10101010uy
    cpu.setFlag Flag.Zero true
    cpu.setFlag Flag.Subtract true
    cpu.setFlag Flag.HalfCarry true
    cpu.setFlag Flag.Carry false

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 1)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 1))

    Assert.That (cpu.Registers.A, Is.EqualTo 0b01010101uy)
    Assert.That (cpu.getFlag Flag.Zero, Is.False)
    Assert.That (cpu.getFlag Flag.Subtract, Is.False)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.False)
    Assert.That (cpu.getFlag Flag.Carry, Is.False)

[<Test>]
let ``Rotate right accumulator all zeros with carry set - rra`` () =
    // Setup
    let opcode = 0x1Fuy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Memory[0x100us] <- opcode
    cpu.Registers.A <- 0b00000000uy
    cpu.setFlag Flag.Zero true
    cpu.setFlag Flag.Subtract true
    cpu.setFlag Flag.HalfCarry true
    cpu.setFlag Flag.Carry true

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 1)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 1))

    Assert.That (cpu.Registers.A, Is.EqualTo 0b10000000uy)
    Assert.That (cpu.getFlag Flag.Zero, Is.False)
    Assert.That (cpu.getFlag Flag.Subtract, Is.False)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.False)
    Assert.That (cpu.getFlag Flag.Carry, Is.False)

[<Test>]
let ``Rotate right accumulator all zeros with carry clear sets zero flag - rra`` () =
    // Setup
    let opcode = 0x1Fuy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Memory[0x100us] <- opcode
    cpu.Registers.A <- 0b00000000uy
    cpu.setFlag Flag.Zero true
    cpu.setFlag Flag.Subtract true
    cpu.setFlag Flag.HalfCarry true
    cpu.setFlag Flag.Carry false

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 1)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 1))

    Assert.That (cpu.Registers.A, Is.EqualTo 0b00000000uy)
    Assert.That (cpu.getFlag Flag.Zero, Is.False)
    Assert.That (cpu.getFlag Flag.Subtract, Is.False)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.False)
    Assert.That (cpu.getFlag Flag.Carry, Is.False)

[<Test>]
let ``Rotate left circular register B with bit 7 set - rlc b`` () =
    // Setup
    let opcode = 0x00uy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Memory[0x100us] <- twoBitPrefix
    cpu.Memory[0x101us] <- opcode
    cpu.Registers.B <- 0b10101010uy
    cpu.setFlag Flag.Zero true
    cpu.setFlag Flag.Subtract true
    cpu.setFlag Flag.HalfCarry true
    cpu.setFlag Flag.Carry false

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 2)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 2))

    Assert.That (cpu.Registers.B, Is.EqualTo 0b01010101uy)
    Assert.That (cpu.getFlag Flag.Zero, Is.False)
    Assert.That (cpu.getFlag Flag.Subtract, Is.False)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.False)
    Assert.That (cpu.getFlag Flag.Carry, Is.True)

[<Test>]
let ``Rotate left circular register C with bit 7 not set - rlc c`` () =
    // Setup
    let opcode = 0x01uy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Memory[0x100us] <- twoBitPrefix
    cpu.Memory[0x101us] <- opcode
    cpu.Registers.C <- 0b01010101uy
    cpu.setFlag Flag.Zero true
    cpu.setFlag Flag.Subtract true
    cpu.setFlag Flag.HalfCarry true
    cpu.setFlag Flag.Carry true

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 2)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 2))

    Assert.That (cpu.Registers.C, Is.EqualTo 0b10101010uy)
    Assert.That (cpu.getFlag Flag.Zero, Is.False)
    Assert.That (cpu.getFlag Flag.Subtract, Is.False)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.False)
    Assert.That (cpu.getFlag Flag.Carry, Is.False)

[<Test>]
let ``Rotate left circular register D all ones - rlc d`` () =
    // Setup
    let opcode = 0x02uy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Memory[0x100us] <- twoBitPrefix
    cpu.Memory[0x101us] <- opcode
    cpu.Registers.D <- 0b11111111uy
    cpu.setFlag Flag.Zero true
    cpu.setFlag Flag.Subtract true
    cpu.setFlag Flag.HalfCarry true
    cpu.setFlag Flag.Carry false

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 2)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 2))

    Assert.That (cpu.Registers.D, Is.EqualTo 0b11111111uy)
    Assert.That (cpu.getFlag Flag.Zero, Is.False)
    Assert.That (cpu.getFlag Flag.Subtract, Is.False)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.False)
    Assert.That (cpu.getFlag Flag.Carry, Is.True)

[<Test>]
let ``Rotate left circular register E all zeros sets zero flag - rlc e`` () =
    // Setup
    let opcode = 0x03uy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Memory[0x100us] <- twoBitPrefix
    cpu.Memory[0x101us] <- opcode
    cpu.Registers.E <- 0b00000000uy
    cpu.setFlag Flag.Zero false
    cpu.setFlag Flag.Subtract true
    cpu.setFlag Flag.HalfCarry true
    cpu.setFlag Flag.Carry true

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 2)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 2))

    Assert.That (cpu.Registers.E, Is.EqualTo 0b00000000uy)
    Assert.That (cpu.getFlag Flag.Zero, Is.True)
    Assert.That (cpu.getFlag Flag.Subtract, Is.False)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.False)
    Assert.That (cpu.getFlag Flag.Carry, Is.False)

[<Test>]
let ``Rotate left circular register H with bit 7 set - rlc h`` () =
    // Setup
    let opcode = 0x04uy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Memory[0x100us] <- twoBitPrefix
    cpu.Memory[0x101us] <- opcode
    cpu.Registers.H <- 0b11000011uy
    cpu.setFlag Flag.Zero true
    cpu.setFlag Flag.Subtract true
    cpu.setFlag Flag.HalfCarry true
    cpu.setFlag Flag.Carry false

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 2)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 2))

    Assert.That (cpu.Registers.H, Is.EqualTo 0b10000111uy)
    Assert.That (cpu.getFlag Flag.Zero, Is.False)
    Assert.That (cpu.getFlag Flag.Subtract, Is.False)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.False)
    Assert.That (cpu.getFlag Flag.Carry, Is.True)

[<Test>]
let ``Rotate left circular register L with bit 7 not set - rlc l`` () =
    // Setup
    let opcode = 0x05uy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Memory[0x100us] <- twoBitPrefix
    cpu.Memory[0x101us] <- opcode
    cpu.Registers.L <- 0b01100011uy
    cpu.setFlag Flag.Zero true
    cpu.setFlag Flag.Subtract true
    cpu.setFlag Flag.HalfCarry true
    cpu.setFlag Flag.Carry true

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 2)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 2))

    Assert.That (cpu.Registers.L, Is.EqualTo 0b11000110uy)
    Assert.That (cpu.getFlag Flag.Zero, Is.False)
    Assert.That (cpu.getFlag Flag.Subtract, Is.False)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.False)
    Assert.That (cpu.getFlag Flag.Carry, Is.False)

[<Test>]
let ``Rotate left circular register A with bit 7 set - rlc a`` () =
    // Setup
    let opcode = 0x07uy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Memory[0x100us] <- twoBitPrefix
    cpu.Memory[0x101us] <- opcode
    cpu.Registers.A <- 0b10001111uy
    cpu.setFlag Flag.Zero true
    cpu.setFlag Flag.Subtract true
    cpu.setFlag Flag.HalfCarry true
    cpu.setFlag Flag.Carry false

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 2)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 2))

    Assert.That (cpu.Registers.A, Is.EqualTo 0b00011111uy)
    Assert.That (cpu.getFlag Flag.Zero, Is.False)
    Assert.That (cpu.getFlag Flag.Subtract, Is.False)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.False)
    Assert.That (cpu.getFlag Flag.Carry, Is.True)

[<Test>]
let ``Rotate right circular memory at HL with bit 0 set - rrc (hl)`` () =
    // Setup
    let opcode = 0x0Euy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Memory[0x100us] <- twoBitPrefix
    cpu.Memory[0x101us] <- opcode
    cpu.Registers.HL <- 0xC000us
    cpu.Memory[0xC000us] <- 0b10101011uy
    cpu.setFlag Flag.Zero true
    cpu.setFlag Flag.Subtract true
    cpu.setFlag Flag.HalfCarry true
    cpu.setFlag Flag.Carry false

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 2)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 4))

    Assert.That (cpu.Memory[0xC000us], Is.EqualTo 0b11010101uy)
    Assert.That (cpu.getFlag Flag.Zero, Is.False)
    Assert.That (cpu.getFlag Flag.Subtract, Is.False)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.False)
    Assert.That (cpu.getFlag Flag.Carry, Is.True)

[<Test>]
let ``Rotate right circular memory at HL with bit 0 not set - rrc (hl)`` () =
    // Setup
    let opcode = 0x0Euy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Memory[0x100us] <- twoBitPrefix
    cpu.Memory[0x101us] <- opcode
    cpu.Registers.HL <- 0xC000us
    cpu.Memory[0xC000us] <- 0b10101010uy
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

    Assert.That (cpu.Memory[0xC000us], Is.EqualTo 0b01010101uy)
    Assert.That (cpu.getFlag Flag.Zero, Is.False)
    Assert.That (cpu.getFlag Flag.Subtract, Is.False)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.False)
    Assert.That (cpu.getFlag Flag.Carry, Is.False)

[<Test>]
let ``Rotate right circular memory at HL all ones - rrc (hl)`` () =
    // Setup
    let opcode = 0x0Euy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Memory[0x100us] <- twoBitPrefix
    cpu.Memory[0x101us] <- opcode
    cpu.Registers.HL <- 0xC000us
    cpu.Memory[0xC000us] <- 0b11111111uy
    cpu.setFlag Flag.Zero true
    cpu.setFlag Flag.Subtract true
    cpu.setFlag Flag.HalfCarry true
    cpu.setFlag Flag.Carry false

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 2)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 4))

    Assert.That (cpu.Memory[0xC000us], Is.EqualTo 0b11111111uy)
    Assert.That (cpu.getFlag Flag.Zero, Is.False)
    Assert.That (cpu.getFlag Flag.Subtract, Is.False)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.False)
    Assert.That (cpu.getFlag Flag.Carry, Is.True)

[<Test>]
let ``Rotate right circular memory at HL all zeros sets zero flag - rrc (hl)`` () =
    // Setup
    let opcode = 0x0Euy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Memory[0x100us] <- twoBitPrefix
    cpu.Memory[0x101us] <- opcode
    cpu.Registers.HL <- 0xC000us
    cpu.Memory[0xC000us] <- 0b00000000uy
    cpu.setFlag Flag.Zero false
    cpu.setFlag Flag.Subtract true
    cpu.setFlag Flag.HalfCarry true
    cpu.setFlag Flag.Carry true

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 2)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 4))

    Assert.That (cpu.Memory[0xC000us], Is.EqualTo 0b00000000uy)
    Assert.That (cpu.getFlag Flag.Zero, Is.True)
    Assert.That (cpu.getFlag Flag.Subtract, Is.False)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.False)
    Assert.That (cpu.getFlag Flag.Carry, Is.False)

[<Test>]
let ``Rotate right memory at HL with different address - rrc (hl)`` () =
    // Setup
    let opcode = 0x0Euy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Memory[0x100us] <- twoBitPrefix
    cpu.Memory[0x101us] <- opcode
    cpu.Registers.HL <- 0xD123us
    cpu.Memory[0xD123us] <- 0b11001100uy
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

    Assert.That (cpu.Memory[0xD123us], Is.EqualTo 0b01100110uy)
    Assert.That (cpu.getFlag Flag.Zero, Is.False)
    Assert.That (cpu.getFlag Flag.Subtract, Is.False)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.False)
    Assert.That (cpu.getFlag Flag.Carry, Is.False)

[<Test>]
let ``Rotate left register B with bit 7 set and carry set - rl b`` () =
    // Setup
    let opcode = 0x10uy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Memory[0x100us] <- twoBitPrefix
    cpu.Memory[0x101us] <- opcode
    cpu.Registers.B <- 0b10101010uy
    cpu.setFlag Flag.Zero true
    cpu.setFlag Flag.Subtract true
    cpu.setFlag Flag.HalfCarry true
    cpu.setFlag Flag.Carry true

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 2)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 2))

    Assert.That (cpu.Registers.B, Is.EqualTo 0b01010101uy)
    Assert.That (cpu.getFlag Flag.Zero, Is.False)
    Assert.That (cpu.getFlag Flag.Subtract, Is.False)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.False)
    Assert.That (cpu.getFlag Flag.Carry, Is.True)

[<Test>]
let ``Rotate left register D with bit 7 clear and carry set - rl d`` () =
    // Setup
    let opcode = 0x12uy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Memory[0x100us] <- twoBitPrefix
    cpu.Memory[0x101us] <- opcode
    cpu.Registers.D <- 0b01010101uy
    cpu.setFlag Flag.Zero true
    cpu.setFlag Flag.Subtract true
    cpu.setFlag Flag.HalfCarry true
    cpu.setFlag Flag.Carry true

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 2)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 2))

    Assert.That (cpu.Registers.D, Is.EqualTo 0b10101011uy)
    Assert.That (cpu.getFlag Flag.Zero, Is.False)
    Assert.That (cpu.getFlag Flag.Subtract, Is.False)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.False)
    Assert.That (cpu.getFlag Flag.Carry, Is.False)

[<Test>]
let ``Rotate left register E with bit 7 clear and carry clear - rl e`` () =
    // Setup
    let opcode = 0x13uy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Memory[0x100us] <- twoBitPrefix
    cpu.Memory[0x101us] <- opcode
    cpu.Registers.E <- 0b01010101uy
    cpu.setFlag Flag.Zero true
    cpu.setFlag Flag.Subtract true
    cpu.setFlag Flag.HalfCarry true
    cpu.setFlag Flag.Carry false

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 2)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 2))

    Assert.That (cpu.Registers.E, Is.EqualTo 0b10101010uy)
    Assert.That (cpu.getFlag Flag.Zero, Is.False)
    Assert.That (cpu.getFlag Flag.Subtract, Is.False)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.False)
    Assert.That (cpu.getFlag Flag.Carry, Is.False)

[<Test>]
let ``Rotate left register A with bit 7 set and carry set - rl a`` () =
    // Setup
    let opcode = 0x17uy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Memory[0x100us] <- twoBitPrefix
    cpu.Memory[0x101us] <- opcode
    cpu.Registers.A <- 0b11001100uy
    cpu.setFlag Flag.Zero true
    cpu.setFlag Flag.Subtract true
    cpu.setFlag Flag.HalfCarry true
    cpu.setFlag Flag.Carry true

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 2)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 2))

    Assert.That (cpu.Registers.A, Is.EqualTo 0b10011001uy)
    Assert.That (cpu.getFlag Flag.Zero, Is.False)
    Assert.That (cpu.getFlag Flag.Subtract, Is.False)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.False)
    Assert.That (cpu.getFlag Flag.Carry, Is.True)

[<Test>]
let ``Rotate left register A results in zero sets zero flag - rl a`` () =
    // Setup
    let opcode = 0x17uy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Memory[0x100us] <- twoBitPrefix
    cpu.Memory[0x101us] <- opcode
    cpu.Registers.A <- 0b10000000uy
    cpu.setFlag Flag.Zero false
    cpu.setFlag Flag.Subtract true
    cpu.setFlag Flag.HalfCarry true
    cpu.setFlag Flag.Carry false

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 2)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 2))

    Assert.That (cpu.Registers.A, Is.EqualTo 0b00000000uy)
    Assert.That (cpu.getFlag Flag.Zero, Is.True)
    Assert.That (cpu.getFlag Flag.Subtract, Is.False)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.False)
    Assert.That (cpu.getFlag Flag.Carry, Is.True)

[<Test>]
let ``Rotate right memory at HL with bit 0 set and carry set - rr (hl)`` () =
    // Setup
    let opcode = 0x1Euy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Memory[0x100us] <- twoBitPrefix
    cpu.Memory[0x101us] <- opcode
    cpu.Registers.HL <- 0xC000us
    cpu.Memory[0xC000us] <- 0b10101011uy
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

    Assert.That (cpu.Memory[0xC000us], Is.EqualTo 0b11010101uy)
    Assert.That (cpu.getFlag Flag.Zero, Is.False)
    Assert.That (cpu.getFlag Flag.Subtract, Is.False)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.False)
    Assert.That (cpu.getFlag Flag.Carry, Is.True)

[<Test>]
let ``Rotate right memory at HL with bit 0 set and carry clear - rr (hl)`` () =
    // Setup
    let opcode = 0x1Euy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Memory[0x100us] <- twoBitPrefix
    cpu.Memory[0x101us] <- opcode
    cpu.Registers.HL <- 0xC000us
    cpu.Memory[0xC000us] <- 0b10101011uy
    cpu.setFlag Flag.Zero true
    cpu.setFlag Flag.Subtract true
    cpu.setFlag Flag.HalfCarry true
    cpu.setFlag Flag.Carry false

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 2)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 4))

    Assert.That (cpu.Memory[0xC000us], Is.EqualTo 0b01010101uy)
    Assert.That (cpu.getFlag Flag.Zero, Is.False)
    Assert.That (cpu.getFlag Flag.Subtract, Is.False)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.False)
    Assert.That (cpu.getFlag Flag.Carry, Is.True)

[<Test>]
let ``Rotate right memory at HL with bit 0 clear and carry set - rr (hl)`` () =
    // Setup
    let opcode = 0x1Euy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Memory[0x100us] <- twoBitPrefix
    cpu.Memory[0x101us] <- opcode
    cpu.Registers.HL <- 0xC000us
    cpu.Memory[0xC000us] <- 0b10101010uy
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

    Assert.That (cpu.Memory[0xC000us], Is.EqualTo 0b11010101uy)
    Assert.That (cpu.getFlag Flag.Zero, Is.False)
    Assert.That (cpu.getFlag Flag.Subtract, Is.False)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.False)
    Assert.That (cpu.getFlag Flag.Carry, Is.False)

[<Test>]
let ``Rotate right memory at HL with bit 0 clear and carry clear - rr (hl)`` () =
    // Setup
    let opcode = 0x1Euy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Memory[0x100us] <- twoBitPrefix
    cpu.Memory[0x101us] <- opcode
    cpu.Registers.HL <- 0xC000us
    cpu.Memory[0xC000us] <- 0b10101010uy
    cpu.setFlag Flag.Zero true
    cpu.setFlag Flag.Subtract true
    cpu.setFlag Flag.HalfCarry true
    cpu.setFlag Flag.Carry false

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 2)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 4))

    Assert.That (cpu.Memory[0xC000us], Is.EqualTo 0b01010101uy)
    Assert.That (cpu.getFlag Flag.Zero, Is.False)
    Assert.That (cpu.getFlag Flag.Subtract, Is.False)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.False)
    Assert.That (cpu.getFlag Flag.Carry, Is.False)

[<Test>]
let ``Rotate right memory at HL all zeros with carry set - rr (hl)`` () =
    // Setup
    let opcode = 0x1Euy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Memory[0x100us] <- twoBitPrefix
    cpu.Memory[0x101us] <- opcode
    cpu.Registers.HL <- 0xC000us
    cpu.Memory[0xC000us] <- 0b00000000uy
    cpu.setFlag Flag.Zero false
    cpu.setFlag Flag.Subtract true
    cpu.setFlag Flag.HalfCarry true
    cpu.setFlag Flag.Carry true

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 2)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 4))

    Assert.That (cpu.Memory[0xC000us], Is.EqualTo 0b10000000uy)
    Assert.That (cpu.getFlag Flag.Zero, Is.False)
    Assert.That (cpu.getFlag Flag.Subtract, Is.False)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.False)
    Assert.That (cpu.getFlag Flag.Carry, Is.False)

[<Test>]
let ``Rotate right memory at HL all zeros with carry clear sets zero flag - rr (hl)`` () =
    // Setup
    let opcode = 0x1Euy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Memory[0x100us] <- twoBitPrefix
    cpu.Memory[0x101us] <- opcode
    cpu.Registers.HL <- 0xC000us
    cpu.Memory[0xC000us] <- 0b00000000uy
    cpu.setFlag Flag.Zero false
    cpu.setFlag Flag.Subtract true
    cpu.setFlag Flag.HalfCarry true
    cpu.setFlag Flag.Carry false

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 2)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 4))

    Assert.That (cpu.Memory[0xC000us], Is.EqualTo 0b00000000uy)
    Assert.That (cpu.getFlag Flag.Zero, Is.True)
    Assert.That (cpu.getFlag Flag.Subtract, Is.False)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.False)
    Assert.That (cpu.getFlag Flag.Carry, Is.False)

[<Test>]
let ``Rotate right memory at HL results in zero sets zero flag - rr (hl)`` () =
    // Setup
    let opcode = 0x1Euy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Memory[0x100us] <- twoBitPrefix
    cpu.Memory[0x101us] <- opcode
    cpu.Registers.HL <- 0xD456us
    cpu.Memory[0xD456us] <- 0b00000001uy
    cpu.setFlag Flag.Zero false
    cpu.setFlag Flag.Subtract true
    cpu.setFlag Flag.HalfCarry true
    cpu.setFlag Flag.Carry false

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 2)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 4))

    Assert.That (cpu.Memory[0xD456us], Is.EqualTo 0b00000000uy)
    Assert.That (cpu.getFlag Flag.Zero, Is.True)
    Assert.That (cpu.getFlag Flag.Subtract, Is.False)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.False)
    Assert.That (cpu.getFlag Flag.Carry, Is.True)

[<Test>]
let ``Shift left arithmetic B register, bit 7 set - sla b`` () =
    // Setup
    let opcode = 0x20uy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Memory[0x100us] <- twoBitPrefix
    cpu.Memory[0x101us] <- opcode
    cpu.Registers.B <- 0b11010110uy
    cpu.setFlag Flag.Zero true
    cpu.setFlag Flag.Subtract true
    cpu.setFlag Flag.HalfCarry true
    cpu.setFlag Flag.Carry false

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 2)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 2))

    Assert.That (cpu.Registers.B, Is.EqualTo 0b10101100uy)
    Assert.That (cpu.getFlag Flag.Zero, Is.False)
    Assert.That (cpu.getFlag Flag.Subtract, Is.False)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.False)
    Assert.That (cpu.getFlag Flag.Carry, Is.True)

[<Test>]
let ``Shift left arithmetic B register, bit 7 not set - sla b`` () =
    // Setup
    let opcode = 0x20uy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Memory[0x100us] <- twoBitPrefix
    cpu.Memory[0x101us] <- opcode
    cpu.Registers.B <- 0b01010110uy
    cpu.setFlag Flag.Zero true
    cpu.setFlag Flag.Subtract true
    cpu.setFlag Flag.HalfCarry true
    cpu.setFlag Flag.Carry true

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 2)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 2))

    Assert.That (cpu.Registers.B, Is.EqualTo 0b10101100uy)
    Assert.That (cpu.getFlag Flag.Zero, Is.False)
    Assert.That (cpu.getFlag Flag.Subtract, Is.False)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.False)
    Assert.That (cpu.getFlag Flag.Carry, Is.False)

[<Test>]
let ``Shift left arithmetic B register, result is zero - sla b`` () =
    // Setup
    let opcode = 0x20uy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Memory[0x100us] <- twoBitPrefix
    cpu.Memory[0x101us] <- opcode
    cpu.Registers.B <- 0b10000000uy
    cpu.setFlag Flag.Zero false
    cpu.setFlag Flag.Subtract true
    cpu.setFlag Flag.HalfCarry true
    cpu.setFlag Flag.Carry false

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 2)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 2))

    Assert.That (cpu.Registers.B, Is.EqualTo 0b00000000uy)
    Assert.That (cpu.getFlag Flag.Zero, Is.True)
    Assert.That (cpu.getFlag Flag.Subtract, Is.False)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.False)
    Assert.That (cpu.getFlag Flag.Carry, Is.True)

[<Test>]
let ``Shift left arithmetic B register, all bits set - sla b`` () =
    // Setup
    let opcode = 0x20uy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Memory[0x100us] <- twoBitPrefix
    cpu.Memory[0x101us] <- opcode
    cpu.Registers.B <- 0b11111111uy
    cpu.setFlag Flag.Zero true
    cpu.setFlag Flag.Subtract true
    cpu.setFlag Flag.HalfCarry true
    cpu.setFlag Flag.Carry false

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 2)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 2))

    Assert.That (cpu.Registers.B, Is.EqualTo 0b11111110uy)
    Assert.That (cpu.getFlag Flag.Zero, Is.False)
    Assert.That (cpu.getFlag Flag.Subtract, Is.False)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.False)
    Assert.That (cpu.getFlag Flag.Carry, Is.True)

[<Test>]
let ``Shift right arithmetic indirect HL, bit 0 set - sra (hl)`` () =
    // Setup
    let opcode = 0x2Euy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Memory[0x100us] <- twoBitPrefix
    cpu.Memory[0x101us] <- opcode
    cpu.Registers.HL <- 0xC000us
    cpu.Memory[0xC000us] <- 0b01101011uy
    cpu.setFlag Flag.Zero true
    cpu.setFlag Flag.Subtract true
    cpu.setFlag Flag.HalfCarry true
    cpu.setFlag Flag.Carry false

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 2)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 4))

    Assert.That (cpu.Memory[0xC000us], Is.EqualTo 0b00110101uy)
    Assert.That (cpu.getFlag Flag.Zero, Is.False)
    Assert.That (cpu.getFlag Flag.Subtract, Is.False)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.False)
    Assert.That (cpu.getFlag Flag.Carry, Is.True)

[<Test>]
let ``Shift right arithmetic indirect HL, bit 0 not set - sra (hl)`` () =
    // Setup
    let opcode = 0x2Euy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Memory[0x100us] <- twoBitPrefix
    cpu.Memory[0x101us] <- opcode
    cpu.Registers.HL <- 0xC000us
    cpu.Memory[0xC000us] <- 0b01101010uy
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

    Assert.That (cpu.Memory[0xC000us], Is.EqualTo 0b00110101uy)
    Assert.That (cpu.getFlag Flag.Zero, Is.False)
    Assert.That (cpu.getFlag Flag.Subtract, Is.False)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.False)
    Assert.That (cpu.getFlag Flag.Carry, Is.False)

[<Test>]
let ``Shift right arithmetic indirect HL, result is zero - sra (hl)`` () =
    // Setup
    let opcode = 0x2Euy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Memory[0x100us] <- twoBitPrefix
    cpu.Memory[0x101us] <- opcode
    cpu.Registers.HL <- 0xC000us
    cpu.Memory[0xC000us] <- 0b00000001uy
    cpu.setFlag Flag.Zero false
    cpu.setFlag Flag.Subtract true
    cpu.setFlag Flag.HalfCarry true
    cpu.setFlag Flag.Carry false

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 2)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 4))

    Assert.That (cpu.Memory[0xC000us], Is.EqualTo 0b00000000uy)
    Assert.That (cpu.getFlag Flag.Zero, Is.True)
    Assert.That (cpu.getFlag Flag.Subtract, Is.False)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.False)
    Assert.That (cpu.getFlag Flag.Carry, Is.True)

[<Test>]
let ``Shift right arithmetic indirect HL, all bits set - sra (hl)`` () =
    // Setup
    let opcode = 0x2Euy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Memory[0x100us] <- twoBitPrefix
    cpu.Memory[0x101us] <- opcode
    cpu.Registers.HL <- 0xC000us
    cpu.Memory[0xC000us] <- 0b11111111uy
    cpu.setFlag Flag.Zero true
    cpu.setFlag Flag.Subtract true
    cpu.setFlag Flag.HalfCarry true
    cpu.setFlag Flag.Carry false

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 2)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 4))

    Assert.That (cpu.Memory[0xC000us], Is.EqualTo 0b01111111uy)
    Assert.That (cpu.getFlag Flag.Zero, Is.False)
    Assert.That (cpu.getFlag Flag.Subtract, Is.False)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.False)
    Assert.That (cpu.getFlag Flag.Carry, Is.True)

[<Test>]
let ``Swap nibbles B register, mixed nibbles - swap b`` () =
    // Setup
    let opcode = 0x30uy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Memory[0x100us] <- twoBitPrefix
    cpu.Memory[0x101us] <- opcode
    cpu.Registers.B <- 0b11010010uy
    cpu.setFlag Flag.Zero true
    cpu.setFlag Flag.Subtract true
    cpu.setFlag Flag.HalfCarry true
    cpu.setFlag Flag.Carry true

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 2)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 2))

    Assert.That (cpu.Registers.B, Is.EqualTo 0b00101101uy)
    Assert.That (cpu.getFlag Flag.Zero, Is.False)
    Assert.That (cpu.getFlag Flag.Subtract, Is.False)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.False)
    Assert.That (cpu.getFlag Flag.Carry, Is.False)

[<Test>]
let ``Swap nibbles B register, result is zero - swap b`` () =
    // Setup
    let opcode = 0x30uy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Memory[0x100us] <- twoBitPrefix
    cpu.Memory[0x101us] <- opcode
    cpu.Registers.B <- 0b00000000uy
    cpu.setFlag Flag.Zero false
    cpu.setFlag Flag.Subtract true
    cpu.setFlag Flag.HalfCarry true
    cpu.setFlag Flag.Carry true

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 2)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 2))

    Assert.That (cpu.Registers.B, Is.EqualTo 0b00000000uy)
    Assert.That (cpu.getFlag Flag.Zero, Is.True)
    Assert.That (cpu.getFlag Flag.Subtract, Is.False)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.False)
    Assert.That (cpu.getFlag Flag.Carry, Is.False)

[<Test>]
let ``Swap nibbles B register, all bits set - swap b`` () =
    // Setup
    let opcode = 0x30uy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Memory[0x100us] <- twoBitPrefix
    cpu.Memory[0x101us] <- opcode
    cpu.Registers.B <- 0b11111111uy
    cpu.setFlag Flag.Zero true
    cpu.setFlag Flag.Subtract true
    cpu.setFlag Flag.HalfCarry true
    cpu.setFlag Flag.Carry true

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 2)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 2))

    Assert.That (cpu.Registers.B, Is.EqualTo 0b11111111uy)
    Assert.That (cpu.getFlag Flag.Zero, Is.False)
    Assert.That (cpu.getFlag Flag.Subtract, Is.False)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.False)
    Assert.That (cpu.getFlag Flag.Carry, Is.False)

[<Test>]
let ``Swap nibbles B register, distinct nibbles - swap b`` () =
    // Setup
    let opcode = 0x30uy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Memory[0x100us] <- twoBitPrefix
    cpu.Memory[0x101us] <- opcode
    cpu.Registers.B <- 0b10100101uy
    cpu.setFlag Flag.Zero true
    cpu.setFlag Flag.Subtract true
    cpu.setFlag Flag.HalfCarry true
    cpu.setFlag Flag.Carry true

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 2)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 2))

    Assert.That (cpu.Registers.B, Is.EqualTo 0b01011010uy)
    Assert.That (cpu.getFlag Flag.Zero, Is.False)
    Assert.That (cpu.getFlag Flag.Subtract, Is.False)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.False)
    Assert.That (cpu.getFlag Flag.Carry, Is.False)

[<Test>]
let ``Shift right arithmetic B register, bit 0 set, bit 7 set - sra b`` () =
    // Setup
    let opcode = 0x28uy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Memory[0x100us] <- twoBitPrefix
    cpu.Memory[0x101us] <- opcode
    cpu.Registers.B <- 0b11010111uy
    cpu.setFlag Flag.Zero true
    cpu.setFlag Flag.Subtract true
    cpu.setFlag Flag.HalfCarry true
    cpu.setFlag Flag.Carry false

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 2)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 2))

    Assert.That (cpu.Registers.B, Is.EqualTo 0b11101011uy)
    Assert.That (cpu.getFlag Flag.Zero, Is.False)
    Assert.That (cpu.getFlag Flag.Subtract, Is.False)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.False)
    Assert.That (cpu.getFlag Flag.Carry, Is.True)

[<Test>]
let ``Shift right arithmetic B register, bit 0 not set, bit 7 set - sra b`` () =
    // Setup
    let opcode = 0x28uy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Memory[0x100us] <- twoBitPrefix
    cpu.Memory[0x101us] <- opcode
    cpu.Registers.B <- 0b11010110uy
    cpu.setFlag Flag.Zero true
    cpu.setFlag Flag.Subtract true
    cpu.setFlag Flag.HalfCarry true
    cpu.setFlag Flag.Carry true

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 2)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 2))

    Assert.That (cpu.Registers.B, Is.EqualTo 0b11101011uy)
    Assert.That (cpu.getFlag Flag.Zero, Is.False)
    Assert.That (cpu.getFlag Flag.Subtract, Is.False)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.False)
    Assert.That (cpu.getFlag Flag.Carry, Is.False)

[<Test>]
let ``Shift right arithmetic B register, bit 0 set, bit 7 not set - sra b`` () =
    // Setup
    let opcode = 0x28uy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Memory[0x100us] <- twoBitPrefix
    cpu.Memory[0x101us] <- opcode
    cpu.Registers.B <- 0b01010111uy
    cpu.setFlag Flag.Zero true
    cpu.setFlag Flag.Subtract true
    cpu.setFlag Flag.HalfCarry true
    cpu.setFlag Flag.Carry false

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 2)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 2))

    Assert.That (cpu.Registers.B, Is.EqualTo 0b00101011uy)
    Assert.That (cpu.getFlag Flag.Zero, Is.False)
    Assert.That (cpu.getFlag Flag.Subtract, Is.False)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.False)
    Assert.That (cpu.getFlag Flag.Carry, Is.True)

[<Test>]
let ``Shift right arithmetic B register, result is zero - sra b`` () =
    // Setup
    let opcode = 0x28uy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Memory[0x100us] <- twoBitPrefix
    cpu.Memory[0x101us] <- opcode
    cpu.Registers.B <- 0b00000001uy
    cpu.setFlag Flag.Zero false
    cpu.setFlag Flag.Subtract true
    cpu.setFlag Flag.HalfCarry true
    cpu.setFlag Flag.Carry false

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 2)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 2))

    Assert.That (cpu.Registers.B, Is.EqualTo 0b00000000uy)
    Assert.That (cpu.getFlag Flag.Zero, Is.True)
    Assert.That (cpu.getFlag Flag.Subtract, Is.False)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.False)
    Assert.That (cpu.getFlag Flag.Carry, Is.True)

[<Test>]
let ``Shift right arithmetic B register, all bits set - sra b`` () =
    // Setup
    let opcode = 0x28uy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Memory[0x100us] <- twoBitPrefix
    cpu.Memory[0x101us] <- opcode
    cpu.Registers.B <- 0b11111111uy
    cpu.setFlag Flag.Zero true
    cpu.setFlag Flag.Subtract true
    cpu.setFlag Flag.HalfCarry true
    cpu.setFlag Flag.Carry false

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 2)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 2))

    Assert.That (cpu.Registers.B, Is.EqualTo 0b11111111uy)
    Assert.That (cpu.getFlag Flag.Zero, Is.False)
    Assert.That (cpu.getFlag Flag.Subtract, Is.False)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.False)
    Assert.That (cpu.getFlag Flag.Carry, Is.True)

[<Test>]
let ``Swap nibbles indirect HL, mixed nibbles - swap (hl)`` () =
    // Setup
    let opcode = 0x36uy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Memory[0x100us] <- twoBitPrefix
    cpu.Memory[0x101us] <- opcode
    cpu.Registers.HL <- 0xC000us
    cpu.Memory[0xC000us] <- 0b11010010uy
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

    Assert.That (cpu.Memory[0xC000us], Is.EqualTo 0b00101101uy)
    Assert.That (cpu.getFlag Flag.Zero, Is.False)
    Assert.That (cpu.getFlag Flag.Subtract, Is.False)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.False)
    Assert.That (cpu.getFlag Flag.Carry, Is.False)

[<Test>]
let ``Swap nibbles indirect HL, result is zero - swap (hl)`` () =
    // Setup
    let opcode = 0x36uy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Memory[0x100us] <- twoBitPrefix
    cpu.Memory[0x101us] <- opcode
    cpu.Registers.HL <- 0xC000us
    cpu.Memory[0xC000us] <- 0b00000000uy
    cpu.setFlag Flag.Zero false
    cpu.setFlag Flag.Subtract true
    cpu.setFlag Flag.HalfCarry true
    cpu.setFlag Flag.Carry true

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 2)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 4))

    Assert.That (cpu.Memory[0xC000us], Is.EqualTo 0b00000000uy)
    Assert.That (cpu.getFlag Flag.Zero, Is.True)
    Assert.That (cpu.getFlag Flag.Subtract, Is.False)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.False)
    Assert.That (cpu.getFlag Flag.Carry, Is.False)

[<Test>]
let ``Swap nibbles indirect HL, all bits set - swap (hl)`` () =
    // Setup
    let opcode = 0x36uy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Memory[0x100us] <- twoBitPrefix
    cpu.Memory[0x101us] <- opcode
    cpu.Registers.HL <- 0xC000us
    cpu.Memory[0xC000us] <- 0b11111111uy
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

    Assert.That (cpu.Memory[0xC000us], Is.EqualTo 0b11111111uy)
    Assert.That (cpu.getFlag Flag.Zero, Is.False)
    Assert.That (cpu.getFlag Flag.Subtract, Is.False)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.False)
    Assert.That (cpu.getFlag Flag.Carry, Is.False)

[<Test>]
let ``Swap nibbles indirect HL, distinct nibbles - swap (hl)`` () =
    // Setup
    let opcode = 0x36uy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Memory[0x100us] <- twoBitPrefix
    cpu.Memory[0x101us] <- opcode
    cpu.Registers.HL <- 0xC000us
    cpu.Memory[0xC000us] <- 0b10100101uy
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

    Assert.That (cpu.Memory[0xC000us], Is.EqualTo 0b01011010uy)
    Assert.That (cpu.getFlag Flag.Zero, Is.False)
    Assert.That (cpu.getFlag Flag.Subtract, Is.False)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.False)
    Assert.That (cpu.getFlag Flag.Carry, Is.False)

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
let ``Reset bit 0 of B register, bit was set - res 0,b`` () =
    // Setup
    let opcode = 0x80uy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Memory[0x100us] <- twoBitPrefix
    cpu.Memory[0x101us] <- opcode
    cpu.Registers.B <- 0b11111111uy
    cpu.setFlag Flag.Zero true
    cpu.setFlag Flag.Subtract true
    cpu.setFlag Flag.HalfCarry true
    cpu.setFlag Flag.Carry true

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 2)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 2))

    Assert.That (cpu.Registers.B, Is.EqualTo 0b11111110uy)
    Assert.That (cpu.getFlag Flag.Zero, Is.True)
    Assert.That (cpu.getFlag Flag.Subtract, Is.True)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.True)
    Assert.That (cpu.getFlag Flag.Carry, Is.True)

[<Test>]
let ``Reset bit 0 of B register, bit was not set - res 0,b`` () =
    // Setup
    let opcode = 0x80uy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Memory[0x100us] <- twoBitPrefix
    cpu.Memory[0x101us] <- opcode
    cpu.Registers.B <- 0b11111110uy
    cpu.setFlag Flag.Zero false
    cpu.setFlag Flag.Subtract false
    cpu.setFlag Flag.HalfCarry false
    cpu.setFlag Flag.Carry false

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 2)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 2))

    Assert.That (cpu.Registers.B, Is.EqualTo 0b11111110uy)
    Assert.That (cpu.getFlag Flag.Zero, Is.False)
    Assert.That (cpu.getFlag Flag.Subtract, Is.False)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.False)
    Assert.That (cpu.getFlag Flag.Carry, Is.False)

[<Test>]
let ``Reset bit 7 of B register, bit was set - res 7,b`` () =
    // Setup
    let opcode = 0xB8uy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Memory[0x100us] <- twoBitPrefix
    cpu.Memory[0x101us] <- opcode
    cpu.Registers.B <- 0b11111111uy
    cpu.setFlag Flag.Zero true
    cpu.setFlag Flag.Subtract true
    cpu.setFlag Flag.HalfCarry true
    cpu.setFlag Flag.Carry true

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 2)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 2))

    Assert.That (cpu.Registers.B, Is.EqualTo 0b01111111uy)
    Assert.That (cpu.getFlag Flag.Zero, Is.True)
    Assert.That (cpu.getFlag Flag.Subtract, Is.True)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.True)
    Assert.That (cpu.getFlag Flag.Carry, Is.True)

[<Test>]
let ``Set bit 0 of indirect HL, bit was already set - set 0,(hl)`` () =
    // Setup
    let opcode = 0xC6uy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Memory[0x100us] <- twoBitPrefix
    cpu.Memory[0x101us] <- opcode
    cpu.Registers.HL <- 0xC000us
    cpu.Memory[0xC000us] <- 0b00000001uy
    cpu.setFlag Flag.Zero false
    cpu.setFlag Flag.Subtract false
    cpu.setFlag Flag.HalfCarry false
    cpu.setFlag Flag.Carry false

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 2)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 4))

    Assert.That (cpu.Memory[0xC000us], Is.EqualTo 0b00000001uy)
    Assert.That (cpu.getFlag Flag.Zero, Is.False)
    Assert.That (cpu.getFlag Flag.Subtract, Is.False)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.False)
    Assert.That (cpu.getFlag Flag.Carry, Is.False)

[<Test>]
let ``Set bit 7 of indirect HL, bit was not set - set 7,(hl)`` () =
    // Setup
    let opcode = 0xFEuy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Memory[0x100us] <- twoBitPrefix
    cpu.Memory[0x101us] <- opcode
    cpu.Registers.HL <- 0xC000us
    cpu.Memory[0xC000us] <- 0b00000000uy
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

    Assert.That (cpu.Memory[0xC000us], Is.EqualTo 0b10000000uy)
    Assert.That (cpu.getFlag Flag.Zero, Is.True)
    Assert.That (cpu.getFlag Flag.Subtract, Is.True)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.True)
    Assert.That (cpu.getFlag Flag.Carry, Is.True)

[<Test>]
let ``Set bit 3 of indirect HL, mixed bits - set 3,(hl)`` () =
    // Setup
    let opcode = 0xDEuy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Memory[0x100us] <- twoBitPrefix
    cpu.Memory[0x101us] <- opcode
    cpu.Registers.HL <- 0xC000us
    cpu.Memory[0xC000us] <- 0b10100010uy
    cpu.setFlag Flag.Zero false
    cpu.setFlag Flag.Subtract false
    cpu.setFlag Flag.HalfCarry false
    cpu.setFlag Flag.Carry false

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 2)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 4))

    Assert.That (cpu.Memory[0xC000us], Is.EqualTo 0b10101010uy)
    Assert.That (cpu.getFlag Flag.Zero, Is.False)
    Assert.That (cpu.getFlag Flag.Subtract, Is.False)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.False)
    Assert.That (cpu.getFlag Flag.Carry, Is.False)

[<Test>]
let ``Shift right logical B register, bit 0 set, bit 7 set - srl b`` () =
    // Setup
    let opcode = 0x38uy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Memory[0x100us] <- twoBitPrefix
    cpu.Memory[0x101us] <- opcode
    cpu.Registers.B <- 0b11010111uy
    cpu.setFlag Flag.Zero true
    cpu.setFlag Flag.Subtract true
    cpu.setFlag Flag.HalfCarry true
    cpu.setFlag Flag.Carry false

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 2)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 2))

    Assert.That (cpu.Registers.B, Is.EqualTo 0b01101011uy)
    Assert.That (cpu.getFlag Flag.Zero, Is.False)
    Assert.That (cpu.getFlag Flag.Subtract, Is.False)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.False)
    Assert.That (cpu.getFlag Flag.Carry, Is.True)

[<Test>]
let ``Shift right logical B register, bit 0 not set, bit 7 set - srl b`` () =
    // Setup
    let opcode = 0x38uy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Memory[0x100us] <- twoBitPrefix
    cpu.Memory[0x101us] <- opcode
    cpu.Registers.B <- 0b11010110uy
    cpu.setFlag Flag.Zero true
    cpu.setFlag Flag.Subtract true
    cpu.setFlag Flag.HalfCarry true
    cpu.setFlag Flag.Carry true

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 2)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 2))

    Assert.That (cpu.Registers.B, Is.EqualTo 0b01101011uy)
    Assert.That (cpu.getFlag Flag.Zero, Is.False)
    Assert.That (cpu.getFlag Flag.Subtract, Is.False)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.False)
    Assert.That (cpu.getFlag Flag.Carry, Is.False)

[<Test>]
let ``Shift right logical B register, bit 0 set, bit 7 not set - srl b`` () =
    // Setup
    let opcode = 0x38uy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Memory[0x100us] <- twoBitPrefix
    cpu.Memory[0x101us] <- opcode
    cpu.Registers.B <- 0b01010111uy
    cpu.setFlag Flag.Zero true
    cpu.setFlag Flag.Subtract true
    cpu.setFlag Flag.HalfCarry true
    cpu.setFlag Flag.Carry false

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 2)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 2))

    Assert.That (cpu.Registers.B, Is.EqualTo 0b00101011uy)
    Assert.That (cpu.getFlag Flag.Zero, Is.False)
    Assert.That (cpu.getFlag Flag.Subtract, Is.False)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.False)
    Assert.That (cpu.getFlag Flag.Carry, Is.True)

[<Test>]
let ``Shift right logical B register, result is zero - srl b`` () =
    // Setup
    let opcode = 0x38uy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Memory[0x100us] <- twoBitPrefix
    cpu.Memory[0x101us] <- opcode
    cpu.Registers.B <- 0b00000001uy
    cpu.setFlag Flag.Zero false
    cpu.setFlag Flag.Subtract true
    cpu.setFlag Flag.HalfCarry true
    cpu.setFlag Flag.Carry false

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 2)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 2))

    Assert.That (cpu.Registers.B, Is.EqualTo 0b00000000uy)
    Assert.That (cpu.getFlag Flag.Zero, Is.True)
    Assert.That (cpu.getFlag Flag.Subtract, Is.False)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.False)
    Assert.That (cpu.getFlag Flag.Carry, Is.True)

[<Test>]
let ``Shift right logical B register, all bits set - srl b`` () =
    // Setup
    let opcode = 0x38uy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Memory[0x100us] <- twoBitPrefix
    cpu.Memory[0x101us] <- opcode
    cpu.Registers.B <- 0b11111111uy
    cpu.setFlag Flag.Zero true
    cpu.setFlag Flag.Subtract true
    cpu.setFlag Flag.HalfCarry true
    cpu.setFlag Flag.Carry false

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 2)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 2))

    Assert.That (cpu.Registers.B, Is.EqualTo 0b01111111uy)
    Assert.That (cpu.getFlag Flag.Zero, Is.False)
    Assert.That (cpu.getFlag Flag.Subtract, Is.False)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.False)
    Assert.That (cpu.getFlag Flag.Carry, Is.True)

[<Test>]
let ``Shift right logical B register, all bits clear - srl b`` () =
    // Setup
    let opcode = 0x38uy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Memory[0x100us] <- twoBitPrefix
    cpu.Memory[0x101us] <- opcode
    cpu.Registers.B <- 0b00000000uy
    cpu.setFlag Flag.Zero false
    cpu.setFlag Flag.Subtract true
    cpu.setFlag Flag.HalfCarry true
    cpu.setFlag Flag.Carry true

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 2)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 2))

    Assert.That (cpu.Registers.B, Is.EqualTo 0b00000000uy)
    Assert.That (cpu.getFlag Flag.Zero, Is.True)
    Assert.That (cpu.getFlag Flag.Subtract, Is.False)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.False)
    Assert.That (cpu.getFlag Flag.Carry, Is.False)
