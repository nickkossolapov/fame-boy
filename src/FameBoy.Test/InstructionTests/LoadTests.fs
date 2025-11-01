module FameBoy.Test.InstructionTests.LoadTests

open FameBoy.Cpu.Execute
open FameBoy.Cpu.Instructions
open FameBoy.Cpu.Opcodes
open FameBoy.Cpu.State
open FameBoy.Memory
open NUnit.Framework

[<Test>]
let ``Load register (register) - ld b,c`` () =
    // Setup
    let opcode = 0x41uy
    let cpu = createCpu (createMemory [||])

    cpu.Pc <- 0x100us
    cpu.Registers.B <- 0x00uy
    cpu.Registers.C <- 0x77uy
    cpu.Memory[0x100us] <- opcode

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 1)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 1))

    Assert.That (cpu.Registers.B, Is.EqualTo 0x77uy)
    Assert.That (cpu.Registers.C, Is.EqualTo 0x77uy)

[<Test>]
let ``Load register (immediate) - ld b,n`` () =
    // Setup
    let opcode = 0x06uy
    let cpu = createCpu (createMemory [||])

    cpu.Pc <- 0x100us
    cpu.Memory[0x100us] <- opcode
    cpu.Memory[0x101us] <- 0x42uy

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 2)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 2))

    Assert.That (cpu.Registers.B, Is.EqualTo 0x42uy)

[<Test>]
let ``Load register (indirect HL) - ld b,(hl)`` () =
    // Setup
    let opcode = 0x46uy
    let cpu = createCpu (createMemory [||])

    cpu.Pc <- 0x100us
    cpu.Registers.HL <- 0xC000us
    cpu.Memory[0x100us] <- opcode
    cpu.Memory[0xC000us] <- 0xABuy

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 1)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 2))

    Assert.That (cpu.Registers.B, Is.EqualTo 0xABuy)

[<Test>]
let ``Load from register (indirect HL) - ld (hl),b`` () =
    // Setup
    let opcode = 0x70uy
    let cpu = createCpu (createMemory [||])

    cpu.Pc <- 0x100us
    cpu.Registers.B <- 0xABuy
    cpu.Registers.HL <- 0xC000us
    cpu.Memory[0x100us] <- opcode

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 1)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 2))

    Assert.That (cpu.Memory[0xC000us], Is.EqualTo 0xABuy)

[<Test>]
let ``Load from immediate (indirect HL) - ld (hl),n`` () =
    // Setup
    let opcode = 0x36uy
    let cpu = createCpu (createMemory [||])

    cpu.Pc <- 0x100us
    cpu.Registers.HL <- 0xC000us
    cpu.Memory[0x100us] <- opcode
    cpu.Memory[0x101us] <- 0xABuy

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 2)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 3))

    Assert.That (cpu.Memory[0xC000us], Is.EqualTo 0xABuy)

[<Test>]
let ``Load accumulator (indirect BC) - ld a,(bc)`` () =
    // Setup
    let opcode = 0x0Auy
    let cpu = createCpu (createMemory [||])

    cpu.Pc <- 0x100us
    cpu.Registers.BC <- 0xC000us
    cpu.Memory[0x100us] <- opcode
    cpu.Memory[0xC000us] <- 0xABuy

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 1)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 2))

    Assert.That (cpu.Registers.A, Is.EqualTo 0xABuy)

[<Test>]
let ``Load accumulator (indirect DE) - ld a,(de)`` () =
    // Setup
    let opcode = 0x1Auy
    let cpu = createCpu (createMemory [||])

    cpu.Pc <- 0x100us
    cpu.Registers.DE <- 0xC000us
    cpu.Memory[0x100us] <- opcode
    cpu.Memory[0xC000us] <- 0xABuy

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 1)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 2))

    Assert.That (cpu.Registers.A, Is.EqualTo 0xABuy)

[<Test>]
let ``Load from accumulator (indirect BC) - ld (bc),a`` () =
    // Setup
    let opcode = 0x02uy
    let cpu = createCpu (createMemory [||])

    cpu.Pc <- 0x100us
    cpu.Registers.A <- 0xABuy
    cpu.Registers.BC <- 0xC000us
    cpu.Memory[0x100us] <- opcode

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 1)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 2))

    Assert.That (cpu.Memory[0xC000us], Is.EqualTo 0xABuy)

[<Test>]
let ``Load from accumulator (indirect DE) - ld (de),a`` () =
    // Setup
    let opcode = 0x12uy
    let cpu = createCpu (createMemory [||])

    cpu.Pc <- 0x100us
    cpu.Registers.A <- 0xABuy
    cpu.Registers.DE <- 0xC000us
    cpu.Memory[0x100us] <- opcode

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 1)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 2))

    Assert.That (cpu.Memory[0xC000us], Is.EqualTo 0xABuy)

[<Test>]
let ``Load accumulator (direct) - ld a,(nn)`` () =
    // Setup
    let opcode = 0xFAuy
    let cpu = createCpu (createMemory [||])

    cpu.Pc <- 0x100us
    cpu.Memory[0x100us] <- opcode
    cpu.Memory[0x101us] <- 0x50uy // LSB
    cpu.Memory[0x102us] <- 0xC0uy // MSB
    cpu.Memory[0xC050us] <- 0xBEuy

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 3)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 4))

    Assert.That (cpu.Registers.A, Is.EqualTo 0xBEuy)

[<Test>]
let ``Load from accumulator (direct) - ld (nn),a`` () =
    // Setup
    let opcode = 0xEAuy
    let cpu = createCpu (createMemory [||])

    cpu.Pc <- 0x100us
    cpu.Registers.A <- 0xBEuy
    cpu.Memory[0x100us] <- opcode
    cpu.Memory[0x101us] <- 0x50uy // LSB
    cpu.Memory[0x102us] <- 0xC0uy // MSB

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 3)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 4))

    Assert.That (cpu.Memory[0xC050us], Is.EqualTo 0xBEuy)

[<Test>]
let ``Load accumulator (indirect 0xFF00+C) - ldh a,(c)`` () =
    // Setup
    let opcode = 0xF2uy
    let cpu = createCpu (createMemory [||])

    cpu.Pc <- 0x100us
    cpu.Registers.C <- 0x80uy
    cpu.Memory[0x100us] <- opcode
    cpu.Memory[0xFF80us] <- 0xDEuy

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 1)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 2))

    Assert.That (cpu.Registers.A, Is.EqualTo 0xDEuy)

[<Test>]
let ``Load from accumulator (indirect 0xFF00+C) - ldh (c),a`` () =
    // Setup
    let opcode = 0xE2uy
    let cpu = createCpu (createMemory [||])

    cpu.Pc <- 0x100us
    cpu.Registers.A <- 0xDEuy
    cpu.Registers.C <- 0x80uy
    cpu.Memory[0x100us] <- opcode

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 1)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 2))
    Assert.That (cpu.Memory[0xFF80us], Is.EqualTo 0xDEuy)
    
[<Test>]
let ``Load accumulator (direct 0xFF00+n) - ldh a,(n)`` () =
    // Setup
    let opcode = 0xF0uy
    let cpu = createCpu (createMemory [||])

    cpu.Pc <- 0x100us
    cpu.Memory[0x100us] <- opcode
    cpu.Memory[0x101us] <- 0x80uy
    cpu.Memory[0xFF80us] <- 0xDEuy

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 2)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 3))

    Assert.That (cpu.Registers.A, Is.EqualTo 0xDEuy)

[<Test>]
let ``Load from accumulator (direct 0xFF00+n) - ldh (n),a`` () =
    // Setup
    let opcode = 0xE0uy
    let cpu = createCpu (createMemory [||])

    cpu.Pc <- 0x100us
    cpu.Registers.A <- 0xDEuy
    cpu.Memory[0x100us] <- opcode
    cpu.Memory[0x101us] <- 0x80uy

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 2)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 3))

    Assert.That (cpu.Memory[0xFF80us], Is.EqualTo 0xDEuy)

[<Test>]
let ``Load accumulator (indirect HL, decrement) - ld a,(hl-)`` () =
    // Setup
    let opcode = 0x3Auy
    let cpu = createCpu (createMemory [||])

    cpu.Pc <- 0x100us
    cpu.Registers.HL <- 0xC001us
    cpu.Memory[0x100us] <- opcode
    cpu.Memory[0xC001us] <- 0xABuy
    cpu.Registers.A <- 0x00uy

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 1)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 2))

    Assert.That (cpu.Registers.A, Is.EqualTo 0xABuy)
    Assert.That (cpu.Registers.HL, Is.EqualTo 0xC000us)

[<Test>]
let ``Load from accumulator (indirect HL, decrement) - ld (hl-),a`` () =
    // Setup
    let opcode = 0x32uy
    let cpu = createCpu (createMemory [||])

    cpu.Pc <- 0x100us
    cpu.Registers.A <- 0xABuy
    cpu.Registers.HL <- 0xC001us
    cpu.Memory[0x100us] <- opcode

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 1)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 2))

    Assert.That (cpu.Memory[0xC001us], Is.EqualTo 0xABuy)
    Assert.That (cpu.Registers.HL, Is.EqualTo 0xC000us)

[<Test>]
let ``Load accumulator (indirect HL, increment) - ld a,(hl+)`` () =
    // Setup
    let opcode = 0x2Auy
    let cpu = createCpu (createMemory [||])

    cpu.Pc <- 0x100us
    cpu.Registers.HL <- 0xC000us
    cpu.Memory[0x100us] <- opcode
    cpu.Memory[0xC000us] <- 0xABuy
    cpu.Registers.A <- 0x00uy

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 1)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 2))

    Assert.That (cpu.Registers.A, Is.EqualTo 0xABuy)
    Assert.That (cpu.Registers.HL, Is.EqualTo 0xC001us)

[<Test>]
let ``Load from accumulator (indirect HL, increment) - ld (hl+),a`` () =
    // Setup
    let opcode = 0x22uy
    let cpu = createCpu (createMemory [||])

    cpu.Pc <- 0x100us
    cpu.Registers.A <- 0xABuy
    cpu.Registers.HL <- 0xC000us
    cpu.Memory[0x100us] <- opcode

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 1)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 2))

    Assert.That (cpu.Memory[0xC000us], Is.EqualTo 0xABuy)
    Assert.That (cpu.Registers.HL, Is.EqualTo 0xC001us)

[<Test>]
let ``Load 16-bit register - ld bc,nn`` () =
    // Setup
    let opcode = 0x01uy
    let cpu = createCpu (createMemory [||])

    cpu.Pc <- 0x100us
    cpu.Memory[0x100us] <- opcode
    cpu.Memory[0x101us] <- 0xFEuy // LSB
    cpu.Memory[0x102us] <- 0xCAuy // MSB

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 3)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 3))

    Assert.That (cpu.Registers.BC, Is.EqualTo 0xCAFEus)

[<Test>]
let ``Load from stack pointer (direct) - ld (nn),sp`` () =
    // Setup
    let opcode = 0x08uy
    let cpu = createCpu (createMemory [||])

    cpu.Pc <- 0x100us
    cpu.Sp <- 0xABCDus
    cpu.Memory[0x100us] <- opcode
    cpu.Memory[0x101us] <- 0x50uy // LSB of nn
    cpu.Memory[0x102us] <- 0xC0uy // MSB of nn

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 3)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 5))

    Assert.That (cpu.Memory[0xC050us], Is.EqualTo 0xCDuy) // LSB of SP
    Assert.That (cpu.Memory[0xC051us], Is.EqualTo 0xABuy) // MSB of SP

[<Test>]
let ``Load stack pointer from HL - ld sp,hl`` () =
    // Setup
    let opcode = 0xF9uy
    let cpu = createCpu (createMemory [||])

    cpu.Pc <- 0x100us
    cpu.Registers.HL <- 0xABCDus
    cpu.Sp <- 0x0000us
    cpu.Memory[0x100us] <- opcode

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 1)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 2))

    Assert.That (cpu.Sp, Is.EqualTo 0xABCDus)

[<Test>]
let ``Push to stack - push bc`` () =
    // Setup
    let opcode = 0xC5uy
    let cpu = createCpu (createMemory [||])

    cpu.Pc <- 0x100us
    cpu.Sp <- 0xFFFEus
    cpu.Registers.BC <- 0xABCDus
    cpu.Memory[0x100us] <- opcode

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 1)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 4))

    Assert.That (cpu.Sp, Is.EqualTo 0xFFFCus)
    Assert.That (cpu.Memory[0xFFFDus], Is.EqualTo 0xABuy) // MSB
    Assert.That (cpu.Memory[0xFFFCus], Is.EqualTo 0xCDuy) // LSB

[<Test>]
let ``Pop from stack - pop bc`` () =
    // Setup
    let opcode = 0xC1uy
    let cpu = createCpu (createMemory [||])

    cpu.Pc <- 0x100us
    cpu.Sp <- 0xFFFCus
    cpu.Memory[0xFFFCus] <- 0xCDuy // LSB
    cpu.Memory[0xFFFDus] <- 0xABuy // MSB
    cpu.Memory[0x100us] <- opcode
    cpu.Registers.BC <- 0x0000us

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 1)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 3))

    Assert.That (cpu.Registers.BC, Is.EqualTo 0xABCDus)
    Assert.That (cpu.Sp, Is.EqualTo 0xFFFEus)

[<Test>]
let ``Pop from stack (flags) - pop af`` () =
    // Setup
    let opcode = 0xF1uy
    let cpu = createCpu (createMemory [||])

    cpu.Pc <- 0x100us
    cpu.Sp <- 0xFFFCus
    cpu.Memory[0xFFFCus] <- 0xCDuy // LSB
    cpu.Memory[0xFFFDus] <- 0xABuy // MSB
    cpu.Memory[0x100us] <- opcode
    cpu.Registers.AF <- 0x0000us

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 1)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 3))

    Assert.That (cpu.Registers.AF, Is.EqualTo 0xABCDus)
    Assert.That (cpu.Sp, Is.EqualTo 0xFFFEus)

[<Test>]
let ``Load HL from adjusted stack pointer - ld hl,sp+e`` () =
    // Setup
    let opcode = 0xF8uy
    let cpu = createCpu (createMemory [||])

    cpu.Pc <- 0x100us
    cpu.Sp <- 0xFEFEus
    cpu.Memory[0x100us] <- opcode
    cpu.Memory[0x101us] <- 0x02uy // e = 2

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 2)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 3))

    Assert.That (cpu.Registers.HL, Is.EqualTo 0xFF00us)
    Assert.That (cpu.getFlag Flag.Zero, Is.False)
    Assert.That (cpu.getFlag Flag.Subtract, Is.False)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.True)
    Assert.That (cpu.getFlag Flag.Carry, Is.True)

[<Test>]
let ``Load HL from adjusted stack pointer (negative) - ld hl,sp+e`` () =
    // Setup
    let opcode = 0xF8uy
    let cpu = createCpu (createMemory [||])

    cpu.Pc <- 0x100us
    cpu.Sp <- 0xFF01us
    cpu.Memory[0x100us] <- opcode
    cpu.Memory[0x101us] <- 0xFEuy // e = -2

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 2)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 3))

    Assert.That (cpu.Registers.HL, Is.EqualTo 0xFEFFus)
    Assert.That (cpu.getFlag Flag.Zero, Is.False)
    Assert.That (cpu.getFlag Flag.Subtract, Is.False)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.False)
    Assert.That (cpu.getFlag Flag.Carry, Is.False)
