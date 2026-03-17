module FameBoy.Cpu.Opcodes

open FameBoy.Cpu.Instructions
open FameBoy.Cpu.Instructions.ByteSource
open FameBoy.Cpu.Instructions.LoadTypes
open FameBoy.Cpu.Utils
open FameBoy.Memory

module private TwoByteInstructions =
    let regOrder =
        [| Write.RegDirect B
           Write.RegDirect C
           Write.RegDirect D
           Write.RegDirect E
           Write.RegDirect H
           Write.RegDirect L
           Write.HLIndirect
           Write.RegDirect A |]
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
    let withImmediate () = Read.Immediate(withUint8 ())
    let withInt8 () = int8 memory[pc + 1us]

    let withUint16 () = getWordFromMemory memory (pc + 1us)

    match opcode with
    | 0x00 -> Nop
    | 0x01 -> Ld16FromWord(BC, withUint16 ()) |> Load
    | 0x02 -> LdA(To, AtBC) |> Load
    | 0x03 -> IncReg16 BC |> Arithmetic
    | 0x04 -> Inc(Write.RegDirect B) |> Arithmetic
    | 0x05 -> Dec(Write.RegDirect B) |> Arithmetic
    | 0x06 -> Ld8(Write.RegDirect B, withImmediate ()) |> Load
    | 0x07 -> Rlca |> Bitwise
    | 0x08 -> LdAtWordFromSP(withUint16 ()) |> Load
    | 0x09 -> AddHL BC |> Arithmetic
    | 0x0A -> LdA(From, AtBC) |> Load
    | 0x0B -> DecReg16 BC |> Arithmetic
    | 0x0C -> Inc(Write.RegDirect C) |> Arithmetic
    | 0x0D -> Dec(Write.RegDirect C) |> Arithmetic
    | 0x0E -> Ld8(Write.RegDirect C, withImmediate ()) |> Load
    | 0x0F -> Rrca |> Bitwise
    | 0x10 -> Stop
    | 0x11 -> Ld16FromWord(DE, withUint16 ()) |> Load
    | 0x12 -> LdA(To, AtDE) |> Load
    | 0x13 -> IncReg16 DE |> Arithmetic
    | 0x14 -> Inc(Write.RegDirect D) |> Arithmetic
    | 0x15 -> Dec(Write.RegDirect D) |> Arithmetic
    | 0x16 -> Ld8(Write.RegDirect D, withImmediate ()) |> Load
    | 0x17 -> Rla |> Bitwise
    | 0x18 -> Jr(withInt8 ()) |> Control
    | 0x19 -> AddHL DE |> Arithmetic
    | 0x1A -> LdA(From, AtDE) |> Load
    | 0x1B -> DecReg16 DE |> Arithmetic
    | 0x1C -> Inc(Write.RegDirect E) |> Arithmetic
    | 0x1D -> Dec(Write.RegDirect E) |> Arithmetic
    | 0x1E -> Ld8(Write.RegDirect E, withImmediate ()) |> Load
    | 0x1F -> Rra |> Bitwise
    | 0x20 -> JrCond(Condition.NotZero, withInt8 ()) |> Control
    | 0x21 -> Ld16FromWord(HL, withUint16 ()) |> Load
    | 0x22 -> LdA(To, AtHLInc) |> Load
    | 0x23 -> IncReg16 HL |> Arithmetic
    | 0x24 -> Inc(Write.RegDirect H) |> Arithmetic
    | 0x25 -> Dec(Write.RegDirect H) |> Arithmetic
    | 0x26 -> Ld8(Write.RegDirect H, withImmediate ()) |> Load
    | 0x27 -> Daa |> Logic
    | 0x28 -> JrCond(Condition.Zero, withInt8 ()) |> Control
    | 0x29 -> AddHL HL |> Arithmetic
    | 0x2A -> LdA(From, AtHLInc) |> Load
    | 0x2B -> DecReg16 HL |> Arithmetic
    | 0x2C -> Inc(Write.RegDirect L) |> Arithmetic
    | 0x2D -> Dec(Write.RegDirect L) |> Arithmetic
    | 0x2E -> Ld8(Write.RegDirect L, withImmediate ()) |> Load
    | 0x2F -> Cpl |> Logic
    | 0x30 -> JrCond(Condition.NoCarry, withInt8 ()) |> Control
    | 0x31 -> Ld16FromWord(SP, withUint16 ()) |> Load
    | 0x32 -> LdA(To, AtHLDec) |> Load
    | 0x33 -> IncReg16 SP |> Arithmetic
    | 0x34 -> Inc Write.HLIndirect |> Arithmetic
    | 0x35 -> Dec Write.HLIndirect |> Arithmetic
    | 0x36 -> Ld8(Write.HLIndirect, withImmediate ()) |> Load
    | 0x37 -> Scf |> Logic
    | 0x38 -> JrCond(Condition.Carry, withInt8 ()) |> Control
    | 0x39 -> AddHL SP |> Arithmetic
    | 0x3A -> LdA(From, AtHLDec) |> Load
    | 0x3B -> DecReg16 SP |> Arithmetic
    | 0x3C -> Inc(Write.RegDirect A) |> Arithmetic
    | 0x3D -> Dec(Write.RegDirect A) |> Arithmetic
    | 0x3E -> Ld8(Write.RegDirect A, withImmediate ()) |> Load
    | 0x3F -> Ccf |> Logic
    | 0x40 -> Ld8(Write.RegDirect B, Read.RegDirect B) |> Load
    | 0x41 -> Ld8(Write.RegDirect B, Read.RegDirect C) |> Load
    | 0x42 -> Ld8(Write.RegDirect B, Read.RegDirect D) |> Load
    | 0x43 -> Ld8(Write.RegDirect B, Read.RegDirect E) |> Load
    | 0x44 -> Ld8(Write.RegDirect B, Read.RegDirect H) |> Load
    | 0x45 -> Ld8(Write.RegDirect B, Read.RegDirect L) |> Load
    | 0x46 -> Ld8(Write.RegDirect B, Read.HLIndirect) |> Load
    | 0x47 -> Ld8(Write.RegDirect B, Read.RegDirect A) |> Load
    | 0x48 -> Ld8(Write.RegDirect C, Read.RegDirect B) |> Load
    | 0x49 -> Ld8(Write.RegDirect C, Read.RegDirect C) |> Load
    | 0x4A -> Ld8(Write.RegDirect C, Read.RegDirect D) |> Load
    | 0x4B -> Ld8(Write.RegDirect C, Read.RegDirect E) |> Load
    | 0x4C -> Ld8(Write.RegDirect C, Read.RegDirect H) |> Load
    | 0x4D -> Ld8(Write.RegDirect C, Read.RegDirect L) |> Load
    | 0x4E -> Ld8(Write.RegDirect C, Read.HLIndirect) |> Load
    | 0x4F -> Ld8(Write.RegDirect C, Read.RegDirect A) |> Load
    | 0x50 -> Ld8(Write.RegDirect D, Read.RegDirect B) |> Load
    | 0x51 -> Ld8(Write.RegDirect D, Read.RegDirect C) |> Load
    | 0x52 -> Ld8(Write.RegDirect D, Read.RegDirect D) |> Load
    | 0x53 -> Ld8(Write.RegDirect D, Read.RegDirect E) |> Load
    | 0x54 -> Ld8(Write.RegDirect D, Read.RegDirect H) |> Load
    | 0x55 -> Ld8(Write.RegDirect D, Read.RegDirect L) |> Load
    | 0x56 -> Ld8(Write.RegDirect D, Read.HLIndirect) |> Load
    | 0x57 -> Ld8(Write.RegDirect D, Read.RegDirect A) |> Load
    | 0x58 -> Ld8(Write.RegDirect E, Read.RegDirect B) |> Load
    | 0x59 -> Ld8(Write.RegDirect E, Read.RegDirect C) |> Load
    | 0x5A -> Ld8(Write.RegDirect E, Read.RegDirect D) |> Load
    | 0x5B -> Ld8(Write.RegDirect E, Read.RegDirect E) |> Load
    | 0x5C -> Ld8(Write.RegDirect E, Read.RegDirect H) |> Load
    | 0x5D -> Ld8(Write.RegDirect E, Read.RegDirect L) |> Load
    | 0x5E -> Ld8(Write.RegDirect E, Read.HLIndirect) |> Load
    | 0x5F -> Ld8(Write.RegDirect E, Read.RegDirect A) |> Load
    | 0x60 -> Ld8(Write.RegDirect H, Read.RegDirect B) |> Load
    | 0x61 -> Ld8(Write.RegDirect H, Read.RegDirect C) |> Load
    | 0x62 -> Ld8(Write.RegDirect H, Read.RegDirect D) |> Load
    | 0x63 -> Ld8(Write.RegDirect H, Read.RegDirect E) |> Load
    | 0x64 -> Ld8(Write.RegDirect H, Read.RegDirect H) |> Load
    | 0x65 -> Ld8(Write.RegDirect H, Read.RegDirect L) |> Load
    | 0x66 -> Ld8(Write.RegDirect H, Read.HLIndirect) |> Load
    | 0x67 -> Ld8(Write.RegDirect H, Read.RegDirect A) |> Load
    | 0x68 -> Ld8(Write.RegDirect L, Read.RegDirect B) |> Load
    | 0x69 -> Ld8(Write.RegDirect L, Read.RegDirect C) |> Load
    | 0x6A -> Ld8(Write.RegDirect L, Read.RegDirect D) |> Load
    | 0x6B -> Ld8(Write.RegDirect L, Read.RegDirect E) |> Load
    | 0x6C -> Ld8(Write.RegDirect L, Read.RegDirect H) |> Load
    | 0x6D -> Ld8(Write.RegDirect L, Read.RegDirect L) |> Load
    | 0x6E -> Ld8(Write.RegDirect L, Read.HLIndirect) |> Load
    | 0x6F -> Ld8(Write.RegDirect L, Read.RegDirect A) |> Load
    | 0x70 -> Ld8(Write.HLIndirect, Read.RegDirect B) |> Load
    | 0x71 -> Ld8(Write.HLIndirect, Read.RegDirect C) |> Load
    | 0x72 -> Ld8(Write.HLIndirect, Read.RegDirect D) |> Load
    | 0x73 -> Ld8(Write.HLIndirect, Read.RegDirect E) |> Load
    | 0x74 -> Ld8(Write.HLIndirect, Read.RegDirect H) |> Load
    | 0x75 -> Ld8(Write.HLIndirect, Read.RegDirect L) |> Load
    | 0x76 -> Halt
    | 0x77 -> Ld8(Write.HLIndirect, Read.RegDirect A) |> Load
    | 0x78 -> Ld8(Write.RegDirect A, Read.RegDirect B) |> Load
    | 0x79 -> Ld8(Write.RegDirect A, Read.RegDirect C) |> Load
    | 0x7A -> Ld8(Write.RegDirect A, Read.RegDirect D) |> Load
    | 0x7B -> Ld8(Write.RegDirect A, Read.RegDirect E) |> Load
    | 0x7C -> Ld8(Write.RegDirect A, Read.RegDirect H) |> Load
    | 0x7D -> Ld8(Write.RegDirect A, Read.RegDirect L) |> Load
    | 0x7E -> Ld8(Write.RegDirect A, Read.HLIndirect) |> Load
    | 0x7F -> Ld8(Write.RegDirect A, Read.RegDirect A) |> Load
    | 0x80 -> Add(Read.RegDirect B) |> Arithmetic
    | 0x81 -> Add(Read.RegDirect C) |> Arithmetic
    | 0x82 -> Add(Read.RegDirect D) |> Arithmetic
    | 0x83 -> Add(Read.RegDirect E) |> Arithmetic
    | 0x84 -> Add(Read.RegDirect H) |> Arithmetic
    | 0x85 -> Add(Read.RegDirect L) |> Arithmetic
    | 0x86 -> Add Read.HLIndirect |> Arithmetic
    | 0x87 -> Add(Read.RegDirect A) |> Arithmetic
    | 0x88 -> Adc(Read.RegDirect B) |> Arithmetic
    | 0x89 -> Adc(Read.RegDirect C) |> Arithmetic
    | 0x8A -> Adc(Read.RegDirect D) |> Arithmetic
    | 0x8B -> Adc(Read.RegDirect E) |> Arithmetic
    | 0x8C -> Adc(Read.RegDirect H) |> Arithmetic
    | 0x8D -> Adc(Read.RegDirect L) |> Arithmetic
    | 0x8E -> Adc Read.HLIndirect |> Arithmetic
    | 0x8F -> Adc(Read.RegDirect A) |> Arithmetic
    | 0x90 -> Sub(Read.RegDirect B) |> Arithmetic
    | 0x91 -> Sub(Read.RegDirect C) |> Arithmetic
    | 0x92 -> Sub(Read.RegDirect D) |> Arithmetic
    | 0x93 -> Sub(Read.RegDirect E) |> Arithmetic
    | 0x94 -> Sub(Read.RegDirect H) |> Arithmetic
    | 0x95 -> Sub(Read.RegDirect L) |> Arithmetic
    | 0x96 -> Sub Read.HLIndirect |> Arithmetic
    | 0x97 -> Sub(Read.RegDirect A) |> Arithmetic
    | 0x98 -> Sbc(Read.RegDirect B) |> Arithmetic
    | 0x99 -> Sbc(Read.RegDirect C) |> Arithmetic
    | 0x9A -> Sbc(Read.RegDirect D) |> Arithmetic
    | 0x9B -> Sbc(Read.RegDirect E) |> Arithmetic
    | 0x9C -> Sbc(Read.RegDirect H) |> Arithmetic
    | 0x9D -> Sbc(Read.RegDirect L) |> Arithmetic
    | 0x9E -> Sbc Read.HLIndirect |> Arithmetic
    | 0x9F -> Sbc(Read.RegDirect A) |> Arithmetic
    | 0xA0 -> And(Read.RegDirect B) |> Logic
    | 0xA1 -> And(Read.RegDirect C) |> Logic
    | 0xA2 -> And(Read.RegDirect D) |> Logic
    | 0xA3 -> And(Read.RegDirect E) |> Logic
    | 0xA4 -> And(Read.RegDirect H) |> Logic
    | 0xA5 -> And(Read.RegDirect L) |> Logic
    | 0xA6 -> And Read.HLIndirect |> Logic
    | 0xA7 -> And(Read.RegDirect A) |> Logic
    | 0xA8 -> Xor(Read.RegDirect B) |> Logic
    | 0xA9 -> Xor(Read.RegDirect C) |> Logic
    | 0xAA -> Xor(Read.RegDirect D) |> Logic
    | 0xAB -> Xor(Read.RegDirect E) |> Logic
    | 0xAC -> Xor(Read.RegDirect H) |> Logic
    | 0xAD -> Xor(Read.RegDirect L) |> Logic
    | 0xAE -> Xor Read.HLIndirect |> Logic
    | 0xAF -> Xor(Read.RegDirect A) |> Logic
    | 0xB0 -> Or(Read.RegDirect B) |> Logic
    | 0xB1 -> Or(Read.RegDirect C) |> Logic
    | 0xB2 -> Or(Read.RegDirect D) |> Logic
    | 0xB3 -> Or(Read.RegDirect E) |> Logic
    | 0xB4 -> Or(Read.RegDirect H) |> Logic
    | 0xB5 -> Or(Read.RegDirect L) |> Logic
    | 0xB6 -> Or Read.HLIndirect |> Logic
    | 0xB7 -> Or(Read.RegDirect A) |> Logic
    | 0xB8 -> Cp(Read.RegDirect B) |> Arithmetic
    | 0xB9 -> Cp(Read.RegDirect C) |> Arithmetic
    | 0xBA -> Cp(Read.RegDirect D) |> Arithmetic
    | 0xBB -> Cp(Read.RegDirect E) |> Arithmetic
    | 0xBC -> Cp(Read.RegDirect H) |> Arithmetic
    | 0xBD -> Cp(Read.RegDirect L) |> Arithmetic
    | 0xBE -> Cp Read.HLIndirect |> Arithmetic
    | 0xBF -> Cp(Read.RegDirect A) |> Arithmetic
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
