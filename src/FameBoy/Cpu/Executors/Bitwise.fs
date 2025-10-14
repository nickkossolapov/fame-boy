module FameBoy.Cpu.Executors.Bitwise

open FameBoy.Cpu.Instructions
open FameBoy.Cpu.State

let executeBitwise (cpu: Cpu) (instr: BitwiseInstr) =
    match instr with
    | Bit (u3, reg) ->
        let regVal = reg.readFromCpu cpu
        let bitIsZero = ((regVal >>> (int u3)) &&& 1uy) = 0uy

        cpu.setFlag Zero bitIsZero
        cpu.setFlag Subtract false
        cpu.setFlag HalfCarry true
