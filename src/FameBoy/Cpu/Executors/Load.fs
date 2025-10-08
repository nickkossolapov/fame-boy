module FameBoy.State.Executors.Load

open FameBoy.Cpu.Instructions
open FameBoy.Cpu.State

let private loadReg16 (cpu: Cpu) reg value =
    let high = uint8 (value >>> 8)
    let low = uint8 (value &&& 0xFFus)

    match reg with
    | BC ->
        cpu.Registers.B <- high
        cpu.Registers.C <- low
    | DE ->
        cpu.Registers.D <- high
        cpu.Registers.E <- low
    | HL ->
        cpu.Registers.H <- high
        cpu.Registers.L <- low
    | PC -> cpu.Pc <- int value
    | SP -> cpu.Sp <- value

let executeLoad (cpu: Cpu) (instr: LoadInstr) =
    match instr with
    | Reg16Word (reg16, s) -> loadReg16 cpu reg16 s
