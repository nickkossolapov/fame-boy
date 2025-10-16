module FameBoy.Cpu.Utils

open FameBoy.Cpu.State

let pushToStack (cpu: Cpu) (value: uint16) =
    let lsb = uint8 value
    let msb = uint8 (value >>> 8)

    cpu.Memory[cpu.Sp - 1us] <- msb
    cpu.Memory[cpu.Sp - 2us] <- lsb
    cpu.Sp <- cpu.Sp - 2us
