module FameBoy.Cpu.Executors.Bitwise

open FameBoy.Cpu.Instructions
open FameBoy.Cpu.State


let executeBitwise (cpu: Cpu) (instr: BitwiseInstr) =
    match instr with
    | Bit (u3, reg) ->
        let regVal = reg.GetFromCpu cpu
        let bitIsZero = ((regVal >>> (int u3)) &&& 1uy) = 0uy

        cpu.setFlags [ Zero, bitIsZero; Subtract, false; HalfCarry, true ]
    | RlA ->
        let rotated = uint8 (cpu.Registers.A <<< 1) + cpu.getFlagBit Carry
        let newCarry = cpu.Registers.A >>> 7 &&& 1uy

        cpu.Registers.A <- rotated
        cpu.setFlags [ Zero, false; Subtract, false; HalfCarry, false; Carry, newCarry <> 0uy ]
    | RlReg8 reg ->
        let rotated = uint8 ((reg.GetFromCpu cpu) <<< 1) + cpu.getFlagBit Carry
        let newCarry = (reg.GetFromCpu cpu) >>> 7 &&& 1uy

        reg.SetToCpu cpu rotated

        cpu.setFlags
            [ Zero, rotated = 0uy
              Subtract, false
              HalfCarry, false
              Carry, newCarry <> 0uy ]
