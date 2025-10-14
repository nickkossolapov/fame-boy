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
    cpu.Pc <- 0x100
    cpu.Memory[0x100] <- 0x20uy // jr nz,s8
    cpu.Memory[0x101] <- 0x05uy // s8 = +5
    cpu.setFlag Flag.Zero false // Z flag not set, so jump is taken

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That(instr.Length, Is.EqualTo 2)
    Assert.That(instr.MCycles, Is.EqualTo (Conditional { Met = 3; NotMet = 2 }))
    
    Assert.That(cpu.Pc, Is.EqualTo 0x107) // 0x100 + 2 + 5

[<Test>]
let ``Jump relative if not zero, not taken - jr nz,s8`` () =
    // Setup
    let cpu = createCpu [||]
    cpu.Pc <- 0x100
    cpu.Memory[0x100] <- 0x20uy // jr nz,s8
    cpu.Memory[0x101] <- 0x05uy // s8 = +5
    cpu.setFlag Flag.Zero true // Z flag set, so jump is not taken

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That(instr.Length, Is.EqualTo 2)
    Assert.That(instr.MCycles, Is.EqualTo (Conditional { Met = 3; NotMet = 2 }))
    
    Assert.That(cpu.Pc, Is.EqualTo 0x102) // PC just advances past instruction
