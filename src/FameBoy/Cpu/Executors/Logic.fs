module FameBoy.Cpu.Executors.Logic

open FameBoy.Cpu.Instructions
open FameBoy.Cpu.State

let xor8 cpu (reg: Reg8) =
    let regValue = reg.readFromCpu cpu
    let result = (cpu.Registers.A ^^^ regValue)

    cpu.Registers.A <- result
    cpu.setFlags (if result <> 0uy then 0b0000 else 0b1000)

    ()

let executeLogic (cpu: Cpu) (instr: LogicInstr) =
    match instr with
    | Xor8 reg8 -> xor8 cpu reg8

    ()
