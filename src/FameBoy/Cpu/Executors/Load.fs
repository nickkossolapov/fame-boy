module FameBoy.Cpu.Executors.Load

open FameBoy.Cpu.Instructions
open FameBoy.Cpu.State


let private loadReg8 (cpu: Cpu) (reg: Reg8) (value: uint8) =
    match reg with
    | A -> cpu.Registers.A <- value
    | B -> cpu.Registers.B <- value
    | C -> cpu.Registers.C <- value
    | D -> cpu.Registers.D <- value
    | E -> cpu.Registers.E <- value
    | H -> cpu.Registers.H <- value
    | L -> cpu.Registers.L <- value
    | F -> cpu.Registers.F <- value

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
    | ToReg8 (reg8, b) -> loadReg8 cpu reg8 b
    | ToReg16 (reg16, w) -> loadReg16 cpu reg16 w
    | StoreAToHLDecrement ->
        cpu.Memory[int (cpu.Registers.getHL ())] <- cpu.Registers.A
        cpu.Registers.setHL (cpu.Registers.getHL () - 1us)
