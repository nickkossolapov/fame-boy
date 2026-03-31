module FameBoy.Cpu.Opcodes

open FameBoy.Cpu.Instructions
open FameBoy.Cpu.Instructions.Operand
open FameBoy.Cpu.Instructions.LoadTypes
open FameBoy.Cpu.Utils
open FameBoy.Memory

module private TwoByteInstructions =
    let regOrder =
        [| Target.RegDirect B
           Target.RegDirect C
           Target.RegDirect D
           Target.RegDirect E
           Target.RegDirect H
           Target.RegDirect L
           Target.HLIndirect
           Target.RegDirect A |]
        |> Array.mapi (fun i write -> uint8 i, write)

    let rotateSwapInstructions =
        [| Rlc; Rrc; Rl; Rr; Sla; Sra; Swap; Srl |]
        |> Array.mapi (fun i instr -> uint8 (i * 8), instr)
        |> Array.allPairs regOrder
        |> Array.map (fun ((i2, target), (i1, instr)) -> i1 + i2, Bitwise(instr target))

    let indexedInstructions =
        Array.allPairs [| Bit; Res; Set |] [| 0uy .. 7uy |]
        |> Array.mapi (fun i (instr, bit) -> 0x40uy + uint8 (i * 8), bit, instr)
        |> Array.allPairs regOrder
        |> Array.map (fun ((i2, target), (i1, bit, instr)) -> i1 + i2, Bitwise(instr (bit, target)))

    let twoByteInstructionMap =
        Array.append rotateSwapInstructions indexedInstructions |> Array.sortBy fst

    let fetchAndDecode2Byte (opcode: uint8) =
        twoByteInstructionMap[int opcode] |> snd

open TwoByteInstructions

let fetchAndDecode (memory: Memory) (pc: uint16) : DecodedInstruction =
    let opcode = int memory[pc]

    let withUint8 () = memory[pc + 1us]
    let withImmediate () = Source.Immediate(withUint8 ())
    let withInt8 () = int8 memory[pc + 1us]

    let withUint16 () = getWordFromMemory memory (pc + 1us)

    match opcode with
    | 0x00 -> Nop
    | 0x01 -> Ld16FromWord(BC, withUint16 ()) |> Load
    | 0x02 -> LdA(To, AtBC) |> Load
    | 0x03 -> IncReg16 BC |> Arithmetic
    | 0x04 -> Inc(Target.RegDirect B) |> Arithmetic
    | 0x05 -> Dec(Target.RegDirect B) |> Arithmetic
    | 0x06 -> Ld8(Target.RegDirect B, withImmediate ()) |> Load
    | 0x07 -> Rlca |> Bitwise
    | 0x08 -> LdAtWordFromSP(withUint16 ()) |> Load
    | 0x09 -> AddHL BC |> Arithmetic
    | 0x0A -> LdA(From, AtBC) |> Load
    | 0x0B -> DecReg16 BC |> Arithmetic
    | 0x0C -> Inc(Target.RegDirect C) |> Arithmetic
    | 0x0D -> Dec(Target.RegDirect C) |> Arithmetic
    | 0x0E -> Ld8(Target.RegDirect C, withImmediate ()) |> Load
    | 0x0F -> Rrca |> Bitwise
    | 0x10 -> Stop
    | 0x11 -> Ld16FromWord(DE, withUint16 ()) |> Load
    | 0x12 -> LdA(To, AtDE) |> Load
    | 0x13 -> IncReg16 DE |> Arithmetic
    | 0x14 -> Inc(Target.RegDirect D) |> Arithmetic
    | 0x15 -> Dec(Target.RegDirect D) |> Arithmetic
    | 0x16 -> Ld8(Target.RegDirect D, withImmediate ()) |> Load
    | 0x17 -> Rla |> Bitwise
    | 0x18 -> Jr(withInt8 ()) |> Control
    | 0x19 -> AddHL DE |> Arithmetic
    | 0x1A -> LdA(From, AtDE) |> Load
    | 0x1B -> DecReg16 DE |> Arithmetic
    | 0x1C -> Inc(Target.RegDirect E) |> Arithmetic
    | 0x1D -> Dec(Target.RegDirect E) |> Arithmetic
    | 0x1E -> Ld8(Target.RegDirect E, withImmediate ()) |> Load
    | 0x1F -> Rra |> Bitwise
    | 0x20 -> JrCond(Condition.NotZero, withInt8 ()) |> Control
    | 0x21 -> Ld16FromWord(HL, withUint16 ()) |> Load
    | 0x22 -> LdA(To, AtHLInc) |> Load
    | 0x23 -> IncReg16 HL |> Arithmetic
    | 0x24 -> Inc(Target.RegDirect H) |> Arithmetic
    | 0x25 -> Dec(Target.RegDirect H) |> Arithmetic
    | 0x26 -> Ld8(Target.RegDirect H, withImmediate ()) |> Load
    | 0x27 -> Daa |> Logic
    | 0x28 -> JrCond(Condition.Zero, withInt8 ()) |> Control
    | 0x29 -> AddHL HL |> Arithmetic
    | 0x2A -> LdA(From, AtHLInc) |> Load
    | 0x2B -> DecReg16 HL |> Arithmetic
    | 0x2C -> Inc(Target.RegDirect L) |> Arithmetic
    | 0x2D -> Dec(Target.RegDirect L) |> Arithmetic
    | 0x2E -> Ld8(Target.RegDirect L, withImmediate ()) |> Load
    | 0x2F -> Cpl |> Logic
    | 0x30 -> JrCond(Condition.NoCarry, withInt8 ()) |> Control
    | 0x31 -> Ld16FromWord(SP, withUint16 ()) |> Load
    | 0x32 -> LdA(To, AtHLDec) |> Load
    | 0x33 -> IncReg16 SP |> Arithmetic
    | 0x34 -> Inc Target.HLIndirect |> Arithmetic
    | 0x35 -> Dec Target.HLIndirect |> Arithmetic
    | 0x36 -> Ld8(Target.HLIndirect, withImmediate ()) |> Load
    | 0x37 -> Scf |> Logic
    | 0x38 -> JrCond(Condition.Carry, withInt8 ()) |> Control
    | 0x39 -> AddHL SP |> Arithmetic
    | 0x3A -> LdA(From, AtHLDec) |> Load
    | 0x3B -> DecReg16 SP |> Arithmetic
    | 0x3C -> Inc(Target.RegDirect A) |> Arithmetic
    | 0x3D -> Dec(Target.RegDirect A) |> Arithmetic
    | 0x3E -> Ld8(Target.RegDirect A, withImmediate ()) |> Load
    | 0x3F -> Ccf |> Logic
    | 0x40 -> Ld8(Target.RegDirect B, Source.RegDirect B) |> Load
    | 0x41 -> Ld8(Target.RegDirect B, Source.RegDirect C) |> Load
    | 0x42 -> Ld8(Target.RegDirect B, Source.RegDirect D) |> Load
    | 0x43 -> Ld8(Target.RegDirect B, Source.RegDirect E) |> Load
    | 0x44 -> Ld8(Target.RegDirect B, Source.RegDirect H) |> Load
    | 0x45 -> Ld8(Target.RegDirect B, Source.RegDirect L) |> Load
    | 0x46 -> Ld8(Target.RegDirect B, Source.HLIndirect) |> Load
    | 0x47 -> Ld8(Target.RegDirect B, Source.RegDirect A) |> Load
    | 0x48 -> Ld8(Target.RegDirect C, Source.RegDirect B) |> Load
    | 0x49 -> Ld8(Target.RegDirect C, Source.RegDirect C) |> Load
    | 0x4A -> Ld8(Target.RegDirect C, Source.RegDirect D) |> Load
    | 0x4B -> Ld8(Target.RegDirect C, Source.RegDirect E) |> Load
    | 0x4C -> Ld8(Target.RegDirect C, Source.RegDirect H) |> Load
    | 0x4D -> Ld8(Target.RegDirect C, Source.RegDirect L) |> Load
    | 0x4E -> Ld8(Target.RegDirect C, Source.HLIndirect) |> Load
    | 0x4F -> Ld8(Target.RegDirect C, Source.RegDirect A) |> Load
    | 0x50 -> Ld8(Target.RegDirect D, Source.RegDirect B) |> Load
    | 0x51 -> Ld8(Target.RegDirect D, Source.RegDirect C) |> Load
    | 0x52 -> Ld8(Target.RegDirect D, Source.RegDirect D) |> Load
    | 0x53 -> Ld8(Target.RegDirect D, Source.RegDirect E) |> Load
    | 0x54 -> Ld8(Target.RegDirect D, Source.RegDirect H) |> Load
    | 0x55 -> Ld8(Target.RegDirect D, Source.RegDirect L) |> Load
    | 0x56 -> Ld8(Target.RegDirect D, Source.HLIndirect) |> Load
    | 0x57 -> Ld8(Target.RegDirect D, Source.RegDirect A) |> Load
    | 0x58 -> Ld8(Target.RegDirect E, Source.RegDirect B) |> Load
    | 0x59 -> Ld8(Target.RegDirect E, Source.RegDirect C) |> Load
    | 0x5A -> Ld8(Target.RegDirect E, Source.RegDirect D) |> Load
    | 0x5B -> Ld8(Target.RegDirect E, Source.RegDirect E) |> Load
    | 0x5C -> Ld8(Target.RegDirect E, Source.RegDirect H) |> Load
    | 0x5D -> Ld8(Target.RegDirect E, Source.RegDirect L) |> Load
    | 0x5E -> Ld8(Target.RegDirect E, Source.HLIndirect) |> Load
    | 0x5F -> Ld8(Target.RegDirect E, Source.RegDirect A) |> Load
    | 0x60 -> Ld8(Target.RegDirect H, Source.RegDirect B) |> Load
    | 0x61 -> Ld8(Target.RegDirect H, Source.RegDirect C) |> Load
    | 0x62 -> Ld8(Target.RegDirect H, Source.RegDirect D) |> Load
    | 0x63 -> Ld8(Target.RegDirect H, Source.RegDirect E) |> Load
    | 0x64 -> Ld8(Target.RegDirect H, Source.RegDirect H) |> Load
    | 0x65 -> Ld8(Target.RegDirect H, Source.RegDirect L) |> Load
    | 0x66 -> Ld8(Target.RegDirect H, Source.HLIndirect) |> Load
    | 0x67 -> Ld8(Target.RegDirect H, Source.RegDirect A) |> Load
    | 0x68 -> Ld8(Target.RegDirect L, Source.RegDirect B) |> Load
    | 0x69 -> Ld8(Target.RegDirect L, Source.RegDirect C) |> Load
    | 0x6A -> Ld8(Target.RegDirect L, Source.RegDirect D) |> Load
    | 0x6B -> Ld8(Target.RegDirect L, Source.RegDirect E) |> Load
    | 0x6C -> Ld8(Target.RegDirect L, Source.RegDirect H) |> Load
    | 0x6D -> Ld8(Target.RegDirect L, Source.RegDirect L) |> Load
    | 0x6E -> Ld8(Target.RegDirect L, Source.HLIndirect) |> Load
    | 0x6F -> Ld8(Target.RegDirect L, Source.RegDirect A) |> Load
    | 0x70 -> Ld8(Target.HLIndirect, Source.RegDirect B) |> Load
    | 0x71 -> Ld8(Target.HLIndirect, Source.RegDirect C) |> Load
    | 0x72 -> Ld8(Target.HLIndirect, Source.RegDirect D) |> Load
    | 0x73 -> Ld8(Target.HLIndirect, Source.RegDirect E) |> Load
    | 0x74 -> Ld8(Target.HLIndirect, Source.RegDirect H) |> Load
    | 0x75 -> Ld8(Target.HLIndirect, Source.RegDirect L) |> Load
    | 0x76 -> Halt
    | 0x77 -> Ld8(Target.HLIndirect, Source.RegDirect A) |> Load
    | 0x78 -> Ld8(Target.RegDirect A, Source.RegDirect B) |> Load
    | 0x79 -> Ld8(Target.RegDirect A, Source.RegDirect C) |> Load
    | 0x7A -> Ld8(Target.RegDirect A, Source.RegDirect D) |> Load
    | 0x7B -> Ld8(Target.RegDirect A, Source.RegDirect E) |> Load
    | 0x7C -> Ld8(Target.RegDirect A, Source.RegDirect H) |> Load
    | 0x7D -> Ld8(Target.RegDirect A, Source.RegDirect L) |> Load
    | 0x7E -> Ld8(Target.RegDirect A, Source.HLIndirect) |> Load
    | 0x7F -> Ld8(Target.RegDirect A, Source.RegDirect A) |> Load
    | 0x80 -> Add(Source.RegDirect B) |> Arithmetic
    | 0x81 -> Add(Source.RegDirect C) |> Arithmetic
    | 0x82 -> Add(Source.RegDirect D) |> Arithmetic
    | 0x83 -> Add(Source.RegDirect E) |> Arithmetic
    | 0x84 -> Add(Source.RegDirect H) |> Arithmetic
    | 0x85 -> Add(Source.RegDirect L) |> Arithmetic
    | 0x86 -> Add Source.HLIndirect |> Arithmetic
    | 0x87 -> Add(Source.RegDirect A) |> Arithmetic
    | 0x88 -> Adc(Source.RegDirect B) |> Arithmetic
    | 0x89 -> Adc(Source.RegDirect C) |> Arithmetic
    | 0x8A -> Adc(Source.RegDirect D) |> Arithmetic
    | 0x8B -> Adc(Source.RegDirect E) |> Arithmetic
    | 0x8C -> Adc(Source.RegDirect H) |> Arithmetic
    | 0x8D -> Adc(Source.RegDirect L) |> Arithmetic
    | 0x8E -> Adc Source.HLIndirect |> Arithmetic
    | 0x8F -> Adc(Source.RegDirect A) |> Arithmetic
    | 0x90 -> Sub(Source.RegDirect B) |> Arithmetic
    | 0x91 -> Sub(Source.RegDirect C) |> Arithmetic
    | 0x92 -> Sub(Source.RegDirect D) |> Arithmetic
    | 0x93 -> Sub(Source.RegDirect E) |> Arithmetic
    | 0x94 -> Sub(Source.RegDirect H) |> Arithmetic
    | 0x95 -> Sub(Source.RegDirect L) |> Arithmetic
    | 0x96 -> Sub Source.HLIndirect |> Arithmetic
    | 0x97 -> Sub(Source.RegDirect A) |> Arithmetic
    | 0x98 -> Sbc(Source.RegDirect B) |> Arithmetic
    | 0x99 -> Sbc(Source.RegDirect C) |> Arithmetic
    | 0x9A -> Sbc(Source.RegDirect D) |> Arithmetic
    | 0x9B -> Sbc(Source.RegDirect E) |> Arithmetic
    | 0x9C -> Sbc(Source.RegDirect H) |> Arithmetic
    | 0x9D -> Sbc(Source.RegDirect L) |> Arithmetic
    | 0x9E -> Sbc Source.HLIndirect |> Arithmetic
    | 0x9F -> Sbc(Source.RegDirect A) |> Arithmetic
    | 0xA0 -> And(Source.RegDirect B) |> Logic
    | 0xA1 -> And(Source.RegDirect C) |> Logic
    | 0xA2 -> And(Source.RegDirect D) |> Logic
    | 0xA3 -> And(Source.RegDirect E) |> Logic
    | 0xA4 -> And(Source.RegDirect H) |> Logic
    | 0xA5 -> And(Source.RegDirect L) |> Logic
    | 0xA6 -> And Source.HLIndirect |> Logic
    | 0xA7 -> And(Source.RegDirect A) |> Logic
    | 0xA8 -> Xor(Source.RegDirect B) |> Logic
    | 0xA9 -> Xor(Source.RegDirect C) |> Logic
    | 0xAA -> Xor(Source.RegDirect D) |> Logic
    | 0xAB -> Xor(Source.RegDirect E) |> Logic
    | 0xAC -> Xor(Source.RegDirect H) |> Logic
    | 0xAD -> Xor(Source.RegDirect L) |> Logic
    | 0xAE -> Xor Source.HLIndirect |> Logic
    | 0xAF -> Xor(Source.RegDirect A) |> Logic
    | 0xB0 -> Or(Source.RegDirect B) |> Logic
    | 0xB1 -> Or(Source.RegDirect C) |> Logic
    | 0xB2 -> Or(Source.RegDirect D) |> Logic
    | 0xB3 -> Or(Source.RegDirect E) |> Logic
    | 0xB4 -> Or(Source.RegDirect H) |> Logic
    | 0xB5 -> Or(Source.RegDirect L) |> Logic
    | 0xB6 -> Or Source.HLIndirect |> Logic
    | 0xB7 -> Or(Source.RegDirect A) |> Logic
    | 0xB8 -> Cp(Source.RegDirect B) |> Arithmetic
    | 0xB9 -> Cp(Source.RegDirect C) |> Arithmetic
    | 0xBA -> Cp(Source.RegDirect D) |> Arithmetic
    | 0xBB -> Cp(Source.RegDirect E) |> Arithmetic
    | 0xBC -> Cp(Source.RegDirect H) |> Arithmetic
    | 0xBD -> Cp(Source.RegDirect L) |> Arithmetic
    | 0xBE -> Cp Source.HLIndirect |> Arithmetic
    | 0xBF -> Cp(Source.RegDirect A) |> Arithmetic
    | 0xC0 -> RetCond Condition.NotZero |> Control
    | 0xC1 -> Pop BC |> Load
    | 0xC2 -> JpCond(Condition.NotZero, withUint16 ()) |> Control
    | 0xC3 -> Jp(withUint16 ()) |> Control
    | 0xC4 -> CallCond(Condition.NotZero, withUint16 ()) |> Control
    | 0xC5 -> Push BC |> Load
    | 0xC6 -> Add(withImmediate ()) |> Arithmetic
    | 0xC7 -> Rst 0x00uy |> Control
    | 0xC8 -> RetCond Condition.Zero |> Control
    | 0xC9 -> Ret |> Control
    | 0xCA -> JpCond(Condition.Zero, withUint16 ()) |> Control
    | 0xCB -> fetchAndDecode2Byte memory[pc + 1us]
    | 0xCC -> CallCond(Condition.Zero, withUint16 ()) |> Control
    | 0xCD -> Call(withUint16 ()) |> Control
    | 0xCE -> Adc(withImmediate ()) |> Arithmetic
    | 0xCF -> Rst 0x08uy |> Control
    | 0xD0 -> RetCond Condition.NoCarry |> Control
    | 0xD1 -> Pop DE |> Load
    | 0xD2 -> JpCond(Condition.NoCarry, withUint16 ()) |> Control
    | 0xD4 -> CallCond(Condition.NoCarry, withUint16 ()) |> Control
    | 0xD5 -> Push DE |> Load
    | 0xD6 -> Sub(withImmediate ()) |> Arithmetic
    | 0xD7 -> Rst 0x10uy |> Control
    | 0xD8 -> RetCond Condition.Carry |> Control
    | 0xD9 -> Reti |> Control
    | 0xDA -> JpCond(Condition.Carry, withUint16 ()) |> Control
    | 0xDC -> CallCond(Condition.Carry, withUint16 ()) |> Control
    | 0xDE -> Sbc(withImmediate ()) |> Arithmetic
    | 0xDF -> Rst 0x18uy |> Control
    | 0xE0 -> Ldh(To, AtByteHigh(withUint8 ())) |> Load
    | 0xE1 -> Pop HL |> Load
    | 0xE2 -> Ldh(To, AtCHigh) |> Load
    | 0xE5 -> Push HL |> Load
    | 0xE6 -> And(withImmediate ()) |> Logic
    | 0xE7 -> Rst 0x20uy |> Control
    | 0xE8 -> AddSPe(withInt8 ()) |> Arithmetic
    | 0xE9 -> JpHL |> Control
    | 0xEA -> LdA(To, AtWord(withUint16 ())) |> Load
    | 0xEE -> Xor(withImmediate ()) |> Logic
    | 0xEF -> Rst 0x28uy |> Control
    | 0xF0 -> Ldh(From, AtByteHigh(withUint8 ())) |> Load
    | 0xF1 -> Pop AF |> Load
    | 0xF2 -> Ldh(From, AtCHigh) |> Load
    | 0xF3 -> Di
    | 0xF5 -> Push AF |> Load
    | 0xF6 -> Or(withImmediate ()) |> Logic
    | 0xF7 -> Rst 0x30uy |> Control
    | 0xF8 -> LdHLFromSPe(withInt8 ()) |> Load
    | 0xF9 -> LdSPFromHL |> Load
    | 0xFA -> LdA(From, AtWord(withUint16 ())) |> Load
    | 0xFB -> Ei
    | 0xFE -> Cp(withImmediate ()) |> Arithmetic
    | 0xFF -> Rst 0x38uy |> Control
    | _ -> Unknown
    |> withLengthAndCycles
