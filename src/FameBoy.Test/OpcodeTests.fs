module FameBoy.Test.OpcodeTests

open FameBoy.Cpu.Execute
open FameBoy.Cpu.Opcodes
open FameBoy.Cpu.State
open NUnit.Framework

[<Test>]
let ``Test little endian order for 3-byte instruction - ld hl,n16 (L = PC+1, H = PC+2)`` () =
    let opcode = 0x21uy
    let cpu = createCpu [||]
    cpu.Pc <- 0x200
    cpu.Memory[0x200] <- opcode
    cpu.Memory[0x201] <- 0x34uy
    cpu.Memory[0x202] <- 0x12uy

    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    Assert.That (cpu.Registers.H, Is.EqualTo 0x12uy)
    Assert.That (cpu.Registers.L, Is.EqualTo 0x34uy)
    Assert.That (cpu.Registers.getHL (), Is.EqualTo 0x1234us)
