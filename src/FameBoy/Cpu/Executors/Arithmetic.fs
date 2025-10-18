module FameBoy.Cpu.Executors.Arithmetic

open FameBoy.Cpu.Instructions
open FameBoy.Cpu.State

let executeArithmetic (cpu: Cpu) (instr: ArithmeticInstr) =
    match instr with
    | IncReg8 reg ->
        let result = reg.GetFrom cpu + 1uy
        let original = reg.GetFrom cpu
        let carried = (original &&& 0xFuy) = 0xFuy

        reg.SetTo cpu result
        cpu.setFlags [ Zero, result = 0uy; Subtract, false; HalfCarry, carried ]
    | DecReg8 reg ->
        let value = reg.GetFrom cpu
        let result = value - 1uy
        let carried = (value &&& 0xFuy) = 0x0uy

        reg.SetTo cpu result
        cpu.setFlags [ Zero, result = 0uy; Subtract, true; HalfCarry, carried ]
