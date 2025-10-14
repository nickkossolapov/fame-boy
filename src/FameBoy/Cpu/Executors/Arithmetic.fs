module FameBoy.Cpu.Executors.Arithmetic

open FameBoy.Cpu.Instructions
open FameBoy.Cpu.State

let executeArithmetic (cpu: Cpu) (instr: ArithmeticInstr) =
    match instr with
    | IncReg reg ->
        let result = reg.readFromCpu cpu + 1uy

        cpu.setFlag Zero (result = 0uy)
        cpu.setFlag Subtract false
        cpu.setFlag HalfCarry ((reg.readFromCpu cpu &&& 0xFuy) = 0xFuy)
        reg.writeToCpu cpu result
