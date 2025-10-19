module FameBoy.Cpu.Executors.Load

open FameBoy.Cpu.Instructions
open FameBoy.Cpu.State
open FameBoy.Cpu.Utils

let executeLoad (cpu: Cpu) (instr: LoadInstr) =
    match instr with
    | LdReg8 (reg, source) -> source.GetFrom cpu |> reg.SetTo cpu
    | LdAtHLFromReg8 reg -> cpu.Memory[cpu.Registers.HL] <- reg.GetFrom cpu
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
    | LdReg16FromWord (reg, w) -> reg.SetTo cpu w
    | LdAtWordFromSP w ->
        let msb, lsb = uint8 (cpu.Sp >>> 8), uint8 cpu.Sp

        cpu.Memory[w] <- lsb
        cpu.Memory[w + 1us] <- msb
    | LdSPFromHL -> cpu.Sp <- cpu.Registers.HL
    | Push reg -> pushToStack cpu (reg.GetFrom cpu)
    | Pop reg -> popFromStack cpu reg
    | LdHLFromSPe b ->
        let offset = int16 b
        let sp = cpu.Sp
        let result = uint16 (sp + uint16 offset)

        let halfCarry = (((sp &&& 0x0Fus) + (uint16 (offset &&& 0x0Fs))) &&& 0x10us) <> 0us
        let carry = (((sp &&& 0xFFus) + (uint16 (offset &&& 0xFFs))) &&& 0x100us) <> 0us

        cpu.Registers.HL <- result
        cpu.setFlags [ Zero, false; Subtract, false; HalfCarry, halfCarry; Carry, carry ]
