module FameBoy.Cpu.Utils

open FameBoy.Cpu.Instructions
open FameBoy.Cpu.State

let getWordFromMemory (memory: Memory) (p: uint16) =
    let msb, lsb = memory[p + 1us], memory[p]

    (uint16 msb <<< 8) + uint16 lsb

let popFromStack (cpu: Cpu) (reg: Reg16) =
    let value = getWordFromMemory cpu.Memory cpu.Sp

    reg.SetTo cpu value
    cpu.Sp <- cpu.Sp + 2us

let pushToStack (cpu: Cpu) (value: uint16) =
    let lsb = uint8 value
    let msb = uint8 (value >>> 8)

    cpu.Memory[cpu.Sp - 1us] <- msb
    cpu.Memory[cpu.Sp - 2us] <- lsb
    cpu.Sp <- cpu.Sp - 2us
