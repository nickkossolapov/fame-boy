module FameBoy.Test.InstructionTests.OtherTests

open FameBoy.Cpu.Instructions
open FameBoy.Memory
open NUnit.Framework
open FameBoy.Cpu.State
open FameBoy.Cpu.Opcodes
open FameBoy.Cpu.Execute

[<Test>]
let ``halt enables halt`` () =
    // Setup
    let cpu = createCpu (createMemory [||])
    cpu.Pc <- 0x100us
    cpu.Memory[0x100us] <- 0x76uy
    cpu.Halted <- false

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 1)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 1))

    Assert.That (cpu.Halted, Is.True)

[<Test>]
let ``di disables interrupts`` () =
    // Setup
    let cpu = createCpu (createMemory [||])
    cpu.Pc <- 0x100us
    cpu.Memory[0x100us] <- 0xF3uy
    cpu.Ime <- true

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 1)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 1))

    Assert.That (cpu.Ime, Is.False)

[<Test>]
let ``ei enables interrupts`` () =
    // Setup
    let cpu = createCpu (createMemory [||])
    cpu.Pc <- 0x100us
    cpu.Memory[0x100us] <- 0xFBuy
    cpu.Ime <- false

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 1)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 1))

    Assert.That (cpu.Ime, Is.True)

[<Test>]
let ``NOP changes nothing`` () =
    // Setup
    let cpu = createCpu (createMemory [||])
    cpu.Pc <- 0x100us
    cpu.Memory[0x100us] <- 0x00uy
    cpu.Registers.A <- 0x12uy
    cpu.Registers.B <- 0x34uy
    cpu.Registers.C <- 0x56uy
    cpu.Registers.D <- 0x78uy
    cpu.Registers.E <- 0x9Auy
    cpu.Registers.H <- 0xBCuy
    cpu.Registers.L <- 0xDEuy
    cpu.Sp <- 0xFFFEus

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 1)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 1))

    Assert.That (cpu.Registers.A, Is.EqualTo 0x12uy)
    Assert.That (cpu.Registers.B, Is.EqualTo 0x34uy)
    Assert.That (cpu.Registers.C, Is.EqualTo 0x56uy)
    Assert.That (cpu.Registers.D, Is.EqualTo 0x78uy)
    Assert.That (cpu.Registers.E, Is.EqualTo 0x9Auy)
    Assert.That (cpu.Registers.H, Is.EqualTo 0xBCuy)
    Assert.That (cpu.Registers.L, Is.EqualTo 0xDEuy)
    Assert.That (cpu.Sp, Is.EqualTo 0xFFFEus)

    Assert.That (cpu.getFlag Flag.Zero, Is.False)
    Assert.That (cpu.getFlag Flag.Subtract, Is.False)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.False)
    Assert.That (cpu.getFlag Flag.Carry, Is.False)

[<Test>]
let ``Unknown opcode acts as NOP`` () =
    // Setup
    let cpu = createCpu (createMemory [||])
    cpu.Pc <- 0x100us
    cpu.Memory[0x100us] <- 0xEBuy // 0xEB is unused/unknown
    cpu.Registers.A <- 0x12uy
    cpu.Registers.B <- 0x34uy
    cpu.Registers.C <- 0x56uy
    cpu.Registers.D <- 0x78uy
    cpu.Registers.E <- 0x9Auy
    cpu.Registers.H <- 0xBCuy
    cpu.Registers.L <- 0xDEuy
    cpu.Sp <- 0xFFFEus

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 1)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 1))

    Assert.That (cpu.Registers.A, Is.EqualTo 0x12uy)
    Assert.That (cpu.Registers.B, Is.EqualTo 0x34uy)
    Assert.That (cpu.Registers.C, Is.EqualTo 0x56uy)
    Assert.That (cpu.Registers.D, Is.EqualTo 0x78uy)
    Assert.That (cpu.Registers.E, Is.EqualTo 0x9Auy)
    Assert.That (cpu.Registers.H, Is.EqualTo 0xBCuy)
    Assert.That (cpu.Registers.L, Is.EqualTo 0xDEuy)
    Assert.That (cpu.Sp, Is.EqualTo 0xFFFEus)

    Assert.That (cpu.getFlag Flag.Zero, Is.False)
    Assert.That (cpu.getFlag Flag.Subtract, Is.False)
    Assert.That (cpu.getFlag Flag.HalfCarry, Is.False)
    Assert.That (cpu.getFlag Flag.Carry, Is.False)
