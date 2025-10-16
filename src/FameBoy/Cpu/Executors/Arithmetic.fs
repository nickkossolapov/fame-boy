module FameBoy.Cpu.Executors.Arithmetic

open FameBoy.Cpu.Instructions
open FameBoy.Cpu.State

let executeArithmetic (cpu: Cpu) (instr: ArithmeticInstr) =
    match instr with
    | IncReg8 reg ->
        let result = reg.GetFromCpu cpu + 1uy

        cpu.setFlag Zero (result = 0uy)
        cpu.setFlag Subtract false
        cpu.setFlag HalfCarry ((reg.GetFromCpu cpu &&& 0xFuy) = 0xFuy)
        reg.SetToCpu cpu result
