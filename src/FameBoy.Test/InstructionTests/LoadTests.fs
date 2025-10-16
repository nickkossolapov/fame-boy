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

    cpu.Pc <- 0x100us
    cpu.Memory[0x100us] <- opcode
    cpu.Memory[0x101us] <- 0xFFuy
    cpu.Memory[0x102us] <- 0xFEuy

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

    cpu.Pc <- 0x100us
    cpu.Registers.A <- 0xABuy
    cpu.Registers.HL <- 0xFE00us
    cpu.Memory[0x100us] <- opcode

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 1)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 2))

    Assert.That (cpu.Memory[0xFE00us], Is.EqualTo 0xABuy)
    Assert.That (cpu.Registers.HL, Is.EqualTo 0xFDFFus)

[<Test>]
let ``Load 8-bit register (immediate) - ld c,n8`` () =
    // Setup
    let opcode = 0x0Euy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Memory[0x100us] <- opcode
    cpu.Memory[0x101us] <- 0x42uy

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

    cpu.Pc <- 0x100us
    cpu.Registers.A <- 0xABuy
    cpu.Registers.C <- 0x42uy
    cpu.Memory[0x100us] <- opcode

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 1)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 2))

    Assert.That (cpu.Memory[0xFF42us], Is.EqualTo 0xABuy)

[<Test>]
let ``Load from register (indirect HL) - ld [hl],a`` () =
    // Setup
    let opcode = 0x77uy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Registers.A <- 0x5Auy
    cpu.Registers.HL <- 0xC123us
    cpu.Memory[0x100us] <- opcode

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 1)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 2))

    Assert.That (cpu.Memory[0xC123us], Is.EqualTo 0x5Auy)
    Assert.That (cpu.Registers.HL, Is.EqualTo 0xC123us)
    Assert.That (cpu.Registers.A, Is.EqualTo 0x5Auy)

[<Test>]
let ``Load from accumulator (direct 0xFF00+n) - ldh [n],a`` () =
    // Setup
    let opcode = 0xE0uy
    let n = 0x42uy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Registers.A <- 0xABuy
    cpu.Memory[0x100us] <- opcode
    cpu.Memory[0x101us] <- n

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 2)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 3))

    Assert.That (cpu.Memory[0xFF42us], Is.EqualTo 0xABuy)
    Assert.That (cpu.Registers.A, Is.EqualTo 0xABuy)

[<Test>]
let ``Load accumulator (indirect DE) - ld a,[de]`` () =
    // Setup
    let opcode = 0x1Auy
    let cpu = createCpu [||]
    cpu.Pc <- 0x100us
    cpu.Registers.DE <- 0xC123us
    cpu.Memory[0x100us] <- opcode
    cpu.Memory[0xC123us] <- 0x77uy
    cpu.Registers.A <- 0x00uy // clear A

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 1)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 2))
    Assert.That (cpu.Registers.A, Is.EqualTo 0x77uy)

[<Test>]
let ``Load register from A to C - ld c,a`` () =
    // Setup
    let opcode = 0x4Fuy
    let cpu = createCpu [||]
    cpu.Pc <- 0x100us
    cpu.Registers.A <- 0x77uy
    cpu.Registers.C <- 0x00uy
    cpu.Memory[0x100us] <- opcode

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 1)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 1))

    Assert.That (cpu.Registers.C, Is.EqualTo 0x77uy)
    Assert.That (cpu.Registers.A, Is.EqualTo 0x77uy)

[<Test>]
let ``Push BC to stack - push bc`` () =
    // Setup
    let opcode = 0xC5uy
    let cpu = createCpu [||]
    cpu.Pc <- 0x100us
    cpu.Sp <- 0xFFFEus
    cpu.Registers.BC <- 0x1234us
    cpu.Memory[0x100us] <- opcode

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 1)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 4))

    Assert.That (cpu.Sp, Is.EqualTo 0xFFFCus)
    Assert.That (cpu.Pc, Is.EqualTo 0x101us)
    Assert.That (cpu.Memory[0xFFFDus], Is.EqualTo 0x12uy) // MSB of BC
    Assert.That (cpu.Memory[0xFFFCus], Is.EqualTo 0x34uy) // LSB of BC
    Assert.That (cpu.Registers.BC, Is.EqualTo 0x1234us)
