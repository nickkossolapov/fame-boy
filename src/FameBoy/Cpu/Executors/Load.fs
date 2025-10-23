module FameBoy.Cpu.Executors.Load

open FameBoy.Cpu.Instructions
open FameBoy.Cpu.Instructions.LoadTypes
open FameBoy.Cpu.State
open FameBoy.Cpu.Utils

let executeLoad (cpu: Cpu) (instr: LoadInstr) =
    let getFullAddress (byte: uint8) = 0xFF00us + uint16 byte

    match instr with
    | LdReg8 (reg, source) -> source.GetFrom cpu |> reg.SetTo cpu
    | LdAtHLFromReg8 reg -> cpu.Memory[cpu.Registers.HL] <- reg.GetFrom cpu
    | LdAtHLFromByte b -> cpu.Memory[cpu.Registers.HL] <- b
    | LdA (d, s) ->
        match d, s with
        | From, AtBC -> cpu.Registers.A <- cpu.Memory[cpu.Registers.BC]
        | From, AtDE -> cpu.Registers.A <- cpu.Memory[cpu.Registers.DE]
        | To, AtBC -> cpu.Memory[cpu.Registers.BC] <- cpu.Registers.A
        | To, AtDE -> cpu.Memory[cpu.Registers.DE] <- cpu.Registers.A
        | From, AtWord w -> cpu.Registers.A <- cpu.Memory[w]
        | To, AtWord w -> cpu.Memory[w] <- cpu.Registers.A
        | From, AtCHigh -> cpu.Registers.A <- cpu.Memory[getFullAddress cpu.Registers.C]
        | To, AtCHigh -> cpu.Memory[getFullAddress cpu.Registers.C] <- cpu.Registers.A
        | From, AtByteHigh b -> cpu.Registers.A <- cpu.Memory[getFullAddress b]
        | To, AtByteHigh b -> cpu.Memory[getFullAddress b] <- cpu.Registers.A
        | From, AtHLInc ->
            cpu.Registers.A <- cpu.Memory[cpu.Registers.HL]
            cpu.Registers.HL <- cpu.Registers.HL + 1us
        | From, AtHLDec ->
            cpu.Registers.A <- cpu.Memory[cpu.Registers.HL]
            cpu.Registers.HL <- cpu.Registers.HL - 1us
        | To, AtHLInc ->
            cpu.Memory[cpu.Registers.HL] <- cpu.Registers.A
            cpu.Registers.HL <- cpu.Registers.HL + 1us
        | To, AtHLDec ->
            cpu.Memory[cpu.Registers.HL] <- cpu.Registers.A
            cpu.Registers.HL <- cpu.Registers.HL - 1us
    | LdReg16FromWord (reg, w) -> reg.SetTo cpu w
    | LdAtWordFromSP w ->
        let msb, lsb = uint8 (cpu.Sp >>> 8), uint8 cpu.Sp

        cpu.Memory[w] <- lsb
        cpu.Memory[w + 1us] <- msb
    | LdSPFromHL -> cpu.Sp <- cpu.Registers.HL
    | Push reg -> pushToStack cpu (reg.GetFrom cpu)
    | Pop reg -> popFromStack cpu reg
    | LdHLFromSPe b ->
        let sp, e = cpu.Sp, uint16 b
        let res = sp + e

        let halfCarry = ((sp &&& 0x0Fus) + (e &&& 0x0Fus)) &&& 0x10us <> 0us
        let carry = ((sp &&& 0xFFus) + (e &&& 0xFFus)) &&& 0x100us <> 0us

        cpu.Registers.HL <- res
        cpu.setFlags [ Zero, false; Subtract, false; HalfCarry, halfCarry; Carry, carry ]
