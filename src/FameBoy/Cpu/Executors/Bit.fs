module FameBoy.Cpu.Executors.Bit

open FameBoy.Cpu.Instructions
open FameBoy.Cpu.State

let executeBit (cpu: Cpu) (instr: BitInstr) =
    match instr with
    | TestBit (u3, reg) ->
        let regVal = reg.readFromCpu cpu
        let bitIsZero = ((regVal >>> (int u3)) &&& 1uy) = 0uy

        cpu.setFlag Zero bitIsZero
        cpu.setFlag Subtraction false
        cpu.setFlag HalfCarry true
