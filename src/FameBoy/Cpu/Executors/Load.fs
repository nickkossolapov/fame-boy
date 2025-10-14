module FameBoy.Cpu.Executors.Load

open FameBoy.Cpu.Instructions
open FameBoy.Cpu.State


let private loadReg8 (cpu: Cpu) (value: uint8) =
    function
    | A -> cpu.Registers.A <- value
    | B -> cpu.Registers.B <- value
    | C -> cpu.Registers.C <- value
    | D -> cpu.Registers.D <- value
    | E -> cpu.Registers.E <- value
    | H -> cpu.Registers.H <- value
    | L -> cpu.Registers.L <- value
    | F -> cpu.Registers.F <- value

let private loadReg16 (cpu: Cpu) (value: uint16) =
    function
    | AF -> cpu.Registers.AF <- value
    | BC -> cpu.Registers.BC <- value
    | DE -> cpu.Registers.DE <- value
    | HL -> cpu.Registers.HL <- value
    | SP -> cpu.Sp <- value

let executeLoad (cpu: Cpu) (instr: LoadInstr) =
    match instr with
    | LdRegFromByte (reg8, b) -> loadReg8 cpu b reg8
    | LdRegFromWord (reg16, w) -> loadReg16 cpu w reg16
    | LdAtHLFromReg reg8 -> cpu.Memory[cpu.Registers.HL] <- reg8.GetFromCpu cpu
    | LdAFromAtHLDec ->
        cpu.Memory.[cpu.Registers.HL] <- cpu.Registers.A
        cpu.Registers.HL <- cpu.Registers.HL - 1us
    | LdhAtCFromA ->
        let address = 0xFF00us + uint16 cpu.Registers.C
        cpu.Memory.[address] <- cpu.Registers.A
