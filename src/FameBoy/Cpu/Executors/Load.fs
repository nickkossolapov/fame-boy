module FameBoy.Cpu.Executors.Load

open FameBoy.Cpu.Instructions
open FameBoy.Cpu.State
open FameBoy.Cpu.Utils


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
    | LdReg8FromReg8 (regTo, regFrom) -> loadReg8 cpu (regFrom.GetFromCpu cpu) regTo
    | LdReg8FromByte (reg, b) -> loadReg8 cpu b reg
    | LdReg8FromAtHL reg -> reg.SetToCpu cpu (cpu.Memory[cpu.Registers.HL])
    | LdAtHLFromReg8 reg -> cpu.Memory[cpu.Registers.HL] <- reg.GetFromCpu cpu
    | LdAtHLFromByte b -> cpu.Memory[cpu.Registers.HL] <- b
    | LdAFromAtBC -> cpu.Registers.A <- cpu.Memory[cpu.Registers.BC]
    | LdAFromAtDE -> cpu.Registers.A <- cpu.Memory[cpu.Registers.DE]
    | LdAtBCFromA -> cpu.Memory[cpu.Registers.BC] <- cpu.Registers.A
    | LdAtDEFromA -> cpu.Memory[cpu.Registers.DE] <- cpu.Registers.A
    | LdAFromAtWord w -> cpu.Registers.A <- cpu.Memory[w]
    | LdAtWordFromA w -> cpu.Memory[w] <- cpu.Registers.A
    | LdhAFromAtC ->
        let address = 0xFF00us + uint16 cpu.Registers.C
        cpu.Registers.A <- cpu.Memory[address]
    | LdhAtCFromA ->
        let address = 0xFF00us + uint16 cpu.Registers.C
        cpu.Memory[address] <- cpu.Registers.A
    | LdhAFromAByte b ->
        let address = 0xFF00us + (uint16 b)
        cpu.Registers.A <- cpu.Memory[address]
    | LdhAtByteFromA b ->
        let address = 0xFF00us + (uint16 b)
        cpu.Memory[address] <- cpu.Registers.A
    | LdAFromAtHLDec ->
        cpu.Registers.A <- cpu.Memory[cpu.Registers.HL]
        cpu.Registers.HL <- cpu.Registers.HL - 1us
    | LdAtHLDecFromA ->
        cpu.Memory[cpu.Registers.HL] <- cpu.Registers.A
        cpu.Registers.HL <- cpu.Registers.HL - 1us
    | LdAFromAtHLInc ->
        cpu.Registers.A <- cpu.Memory[cpu.Registers.HL]
        cpu.Registers.HL <- cpu.Registers.HL + 1us
    | LdAtHLIncFromA ->
        cpu.Memory[cpu.Registers.HL] <- cpu.Registers.A
        cpu.Registers.HL <- cpu.Registers.HL + 1us
    | LdReg16FromWord (reg, w) -> loadReg16 cpu w reg
    | LdAtWordFromSP w ->
        let msb, lsb = uint8 (cpu.Sp >>> 8), uint8 cpu.Sp

        cpu.Memory[w] <- lsb
        cpu.Memory[w + 1us] <- msb
    | LdSPFromHL -> cpu.Sp <- cpu.Registers.HL
    | Push reg -> pushToStack cpu (reg.GetFromCpu cpu)
    | Pop reg -> popFromStack cpu reg
    | LdHLFromSPe b ->
        let offset = int16 b
        let sp = cpu.Sp
        let result = uint16 (sp + uint16 offset)

        let halfCarry = (((sp &&& 0x0Fus) + (uint16 (offset &&& 0x0Fs))) &&& 0x10us) <> 0us
        let carry = (((sp &&& 0xFFus) + (uint16 (offset &&& 0xFFs))) &&& 0x100us) <> 0us

        cpu.Registers.HL <- result
        cpu.setFlags [ Zero, false; Subtract, false; HalfCarry, halfCarry; Carry, carry ]
