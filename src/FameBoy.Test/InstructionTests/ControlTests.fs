module FameBoy.Test.InstructionTests.ControlTests

open FameBoy.Cpu.Execute
open FameBoy.Cpu.Opcodes
open FameBoy.Cpu.State
open FameBoy.Cpu.Instructions
open NUnit.Framework

[<Test>]
let ``Jump relative if not zero, taken - jr nz,s8`` () =
    // Setup
    let cpu = createCpu [||]
    cpu.Pc <- 0x100us
    cpu.Memory[0x100us] <- 0x20uy // jr nz,s8
    cpu.Memory[0x101us] <- 0x05uy // s8 = +5
    cpu.setFlag Flag.Zero false // Z flag not set, so jump is taken

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 2)
    Assert.That (instr.MCycles, Is.EqualTo (Conditional { Met = 3; NotMet = 2 }))

    Assert.That (cpu.Pc, Is.EqualTo 0x107) // 0x100 + 2 + 5

[<Test>]
let ``Jump relative if not zero, not taken - jr nz,s8`` () =
    // Setup
    let cpu = createCpu [||]
    cpu.Pc <- 0x100us
    cpu.Memory[0x100us] <- 0x20uy // jr nz,s8
    cpu.Memory[0x101us] <- 0x05uy // s8 = +5
    cpu.setFlag Flag.Zero true // Z flag set, so jump is not taken

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 2)
    Assert.That (instr.MCycles, Is.EqualTo (Conditional { Met = 3; NotMet = 2 }))

    Assert.That (cpu.Pc, Is.EqualTo 0x102) // PC just advances past instruction

[<Test>]
let ``Call function - call nn`` () =
    // Setup
    let cpu = createCpu [||]
    cpu.Pc <- 0x100us
    cpu.Sp <- 0xFFFEus
    cpu.Memory[0x100us] <- 0xCDuy // CALL nn opcode
    cpu.Memory[0x101us] <- 0x34uy // LSB(nn)
    cpu.Memory[0x102us] <- 0x12uy // MSB(nn)

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 3)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 6))

    Assert.That (cpu.Pc, Is.EqualTo 0x1234us)
    Assert.That (cpu.Sp, Is.EqualTo 0xFFFCus)
    Assert.That (cpu.Memory[0xFFFDus], Is.EqualTo 0x01uy) // MSB of 0x103
    Assert.That (cpu.Memory[0xFFFCus], Is.EqualTo 0x03uy) // LSB of 0x103
