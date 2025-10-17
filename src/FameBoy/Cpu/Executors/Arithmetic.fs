module FameBoy.Cpu.Executors.Arithmetic

open FameBoy.Cpu.Instructions
open FameBoy.Cpu.State

let executeArithmetic (cpu: Cpu) (instr: ArithmeticInstr) =
    match instr with
    | IncReg8 reg ->
        let result = reg.GetFromCpu cpu + 1uy
        let original = reg.GetFromCpu cpu
        let carried = (original &&& 0xFuy) = 0xFuy

        reg.SetToCpu cpu result
        cpu.setFlags [ Zero, result = 0uy; Subtract, false; HalfCarry, carried ]
    | DecReg8 reg ->
        let value = reg.GetFromCpu cpu
        let result = value - 1uy
        let carried = (value &&& 0xFuy) = 0x0uy

        reg.SetToCpu cpu result
        cpu.setFlags [ Zero, result = 0uy; Subtract, true; HalfCarry, carried ]
