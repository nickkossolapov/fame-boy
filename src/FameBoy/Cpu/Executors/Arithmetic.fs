module FameBoy.Cpu.Executors.Arithmetic

open FameBoy.Cpu.Instructions
open FameBoy.Cpu.State

let executeArithmetic (cpu: Cpu) (instr: ArithmeticInstr) =
    match instr with
    | IncReg8 reg ->
        let result = reg.GetFromCpu cpu + 1uy

        cpu.setFlags
            [ Zero, result = 0uy
              Subtract, false
              HalfCarry, (reg.GetFromCpu cpu &&& 0xFuy) = 0xFuy ]

        reg.SetToCpu cpu result
