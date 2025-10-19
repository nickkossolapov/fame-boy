module FameBoy.Cpu.Opcodes

open FameBoy.Cpu.Instructions
open FameBoy.Cpu.Instructions.LoadTypes
open FameBoy.Cpu.State
open FameBoy.Cpu.Utils

let private fetchAndDecode2Byte (memory: Memory) (pc: uint16) =
    let opcode = int memory[pc + 1us]

    match opcode with
    | 0x00 -> failwith "0x00 <rlc b> not implemented"
    | 0x01 -> failwith "0x01 <rlc c> not implemented"
    | 0x02 -> failwith "0x02 <rlc d> not implemented"
    | 0x03 -> failwith "0x03 <rlc e> not implemented"
    | 0x04 -> failwith "0x04 <rlc h> not implemented"
    | 0x05 -> failwith "0x05 <rlc l> not implemented"
    | 0x06 -> failwith "0x06 <rlc (hl)> not implemented"
    | 0x07 -> failwith "0x07 <rlc a> not implemented"
    | 0x08 -> failwith "0x08 <rrc b> not implemented"
    | 0x09 -> failwith "0x09 <rrc c> not implemented"
    | 0x0A -> failwith "0x0A <rrc d> not implemented"
    | 0x0B -> failwith "0x0B <rrc e> not implemented"
    | 0x0C -> failwith "0x0C <rrc h> not implemented"
    | 0x0D -> failwith "0x0D <rrc l> not implemented"
    | 0x0E -> failwith "0x0E <rrc (hl)> not implemented"
    | 0x0F -> failwith "0x0F <rrc a> not implemented"
    | 0x10 -> failwith "0x10 <rl b> not implemented"
    | 0x11 -> RlReg8 C |> Bitwise
    | 0x12 -> failwith "0x12 <rl d> not implemented"
    | 0x13 -> failwith "0x13 <rl e> not implemented"
    | 0x14 -> failwith "0x14 <rl h> not implemented"
    | 0x15 -> failwith "0x15 <rl l> not implemented"
    | 0x16 -> failwith "0x16 <rl (hl)> not implemented"
    | 0x17 -> failwith "0x17 <rl a> not implemented"
    | 0x18 -> failwith "0x18 <rr b> not implemented"
    | 0x19 -> failwith "0x19 <rr c> not implemented"
    | 0x1A -> failwith "0x1A <rr d> not implemented"
    | 0x1B -> failwith "0x1B <rr e> not implemented"
    | 0x1C -> failwith "0x1C <rr h> not implemented"
    | 0x1D -> failwith "0x1D <rr l> not implemented"
    | 0x1E -> failwith "0x1E <rr (hl)> not implemented"
    | 0x1F -> failwith "0x1F <rr a> not implemented"
    | 0x20 -> failwith "0x20 <sla b> not implemented"
    | 0x21 -> failwith "0x21 <sla c> not implemented"
    | 0x22 -> failwith "0x22 <sla d> not implemented"
    | 0x23 -> failwith "0x23 <sla e> not implemented"
    | 0x24 -> failwith "0x24 <sla h> not implemented"
    | 0x25 -> failwith "0x25 <sla l> not implemented"
    | 0x26 -> failwith "0x26 <sla (hl)> not implemented"
    | 0x27 -> failwith "0x27 <sla a> not implemented"
    | 0x28 -> failwith "0x28 <sra b> not implemented"
    | 0x29 -> failwith "0x29 <sra c> not implemented"
    | 0x2A -> failwith "0x2A <sra d> not implemented"
    | 0x2B -> failwith "0x2B <sra e> not implemented"
    | 0x2C -> failwith "0x2C <sra h> not implemented"
    | 0x2D -> failwith "0x2D <sra l> not implemented"
    | 0x2E -> failwith "0x2E <sra (hl)> not implemented"
    | 0x2F -> failwith "0x2F <sra a> not implemented"
    | 0x30 -> failwith "0x30 <swap b> not implemented"
    | 0x31 -> failwith "0x31 <swap c> not implemented"
    | 0x32 -> failwith "0x32 <swap d> not implemented"
    | 0x33 -> failwith "0x33 <swap e> not implemented"
    | 0x34 -> failwith "0x34 <swap h> not implemented"
    | 0x35 -> failwith "0x35 <swap l> not implemented"
    | 0x36 -> failwith "0x36 <swap (hl)> not implemented"
    | 0x37 -> failwith "0x37 <swap a> not implemented"
    | 0x38 -> failwith "0x38 <srl b> not implemented"
    | 0x39 -> failwith "0x39 <srl c> not implemented"
    | 0x3A -> failwith "0x3A <srl d> not implemented"
    | 0x3B -> failwith "0x3B <srl e> not implemented"
    | 0x3C -> failwith "0x3C <srl h> not implemented"
    | 0x3D -> failwith "0x3D <srl l> not implemented"
    | 0x3E -> failwith "0x3E <srl (hl)> not implemented"
    | 0x3F -> failwith "0x3F <srl a> not implemented"
    | 0x40 -> failwith "0x40 <bit 0,b> not implemented"
    | 0x41 -> failwith "0x41 <bit 0,c> not implemented"
    | 0x42 -> failwith "0x42 <bit 0,d> not implemented"
    | 0x43 -> failwith "0x43 <bit 0,e> not implemented"
    | 0x44 -> failwith "0x44 <bit 0,h> not implemented"
    | 0x45 -> failwith "0x45 <bit 0,l> not implemented"
    | 0x46 -> failwith "0x46 <bit 0,(hl)> not implemented"
    | 0x47 -> failwith "0x47 <bit 0,a> not implemented"
    | 0x48 -> failwith "0x48 <bit 1,b> not implemented"
    | 0x49 -> failwith "0x49 <bit 1,c> not implemented"
    | 0x4A -> failwith "0x4A <bit 1,d> not implemented"
    | 0x4B -> failwith "0x4B <bit 1,e> not implemented"
    | 0x4C -> failwith "0x4C <bit 1,h> not implemented"
    | 0x4D -> failwith "0x4D <bit 1,l> not implemented"
    | 0x4E -> failwith "0x4E <bit 1,(hl)> not implemented"
    | 0x4F -> failwith "0x4F <bit 1,a> not implemented"
    | 0x50 -> failwith "0x50 <bit 2,b> not implemented"
    | 0x51 -> failwith "0x51 <bit 2,c> not implemented"
    | 0x52 -> failwith "0x52 <bit 2,d> not implemented"
    | 0x53 -> failwith "0x53 <bit 2,e> not implemented"
    | 0x54 -> failwith "0x54 <bit 2,h> not implemented"
    | 0x55 -> failwith "0x55 <bit 2,l> not implemented"
    | 0x56 -> failwith "0x56 <bit 2,(hl)> not implemented"
    | 0x57 -> failwith "0x57 <bit 2,a> not implemented"
    | 0x58 -> failwith "0x58 <bit 3,b> not implemented"
    | 0x59 -> failwith "0x59 <bit 3,c> not implemented"
    | 0x5A -> failwith "0x5A <bit 3,d> not implemented"
    | 0x5B -> failwith "0x5B <bit 3,e> not implemented"
    | 0x5C -> failwith "0x5C <bit 3,h> not implemented"
    | 0x5D -> failwith "0x5D <bit 3,l> not implemented"
    | 0x5E -> failwith "0x5E <bit 3,(hl)> not implemented"
    | 0x5F -> failwith "0x5F <bit 3,a> not implemented"
    | 0x60 -> failwith "0x60 <bit 4,b> not implemented"
    | 0x61 -> failwith "0x61 <bit 4,c> not implemented"
    | 0x62 -> failwith "0x62 <bit 4,d> not implemented"
    | 0x63 -> failwith "0x63 <bit 4,e> not implemented"
    | 0x64 -> failwith "0x64 <bit 4,h> not implemented"
    | 0x65 -> failwith "0x65 <bit 4,l> not implemented"
    | 0x66 -> failwith "0x66 <bit 4,(hl)> not implemented"
    | 0x67 -> failwith "0x67 <bit 4,a> not implemented"
    | 0x68 -> failwith "0x68 <bit 5,b> not implemented"
    | 0x69 -> failwith "0x69 <bit 5,c> not implemented"
    | 0x6A -> failwith "0x6A <bit 5,d> not implemented"
    | 0x6B -> failwith "0x6B <bit 5,e> not implemented"
    | 0x6C -> failwith "0x6C <bit 5,h> not implemented"
    | 0x6D -> failwith "0x6D <bit 5,l> not implemented"
    | 0x6E -> failwith "0x6E <bit 5,(hl)> not implemented"
    | 0x6F -> failwith "0x6F <bit 5,a> not implemented"
    | 0x70 -> failwith "0x70 <bit 6,b> not implemented"
    | 0x71 -> failwith "0x71 <bit 6,c> not implemented"
    | 0x72 -> failwith "0x72 <bit 6,d> not implemented"
    | 0x73 -> failwith "0x73 <bit 6,e> not implemented"
    | 0x74 -> failwith "0x74 <bit 6,h> not implemented"
    | 0x75 -> failwith "0x75 <bit 6,l> not implemented"
    | 0x76 -> failwith "0x76 <bit 6,(hl)> not implemented"
    | 0x77 -> failwith "0x77 <bit 6,a> not implemented"
    | 0x78 -> failwith "0x78 <bit 7,b> not implemented"
    | 0x79 -> failwith "0x79 <bit 7,c> not implemented"
    | 0x7A -> failwith "0x7A <bit 7,d> not implemented"
    | 0x7B -> failwith "0x7B <bit 7,e> not implemented"
    | 0x7C -> Bit (7uy, H) |> Bitwise
    | 0x7D -> failwith "0x7D <bit 7,l> not implemented"
    | 0x7E -> failwith "0x7E <bit 7,(hl)> not implemented"
    | 0x7F -> failwith "0x7F <bit 7,a> not implemented"
    | 0x80 -> failwith "0x80 <res 0,b> not implemented"
    | 0x81 -> failwith "0x81 <res 0,c> not implemented"
    | 0x82 -> failwith "0x82 <res 0,d> not implemented"
    | 0x83 -> failwith "0x83 <res 0,e> not implemented"
    | 0x84 -> failwith "0x84 <res 0,h> not implemented"
    | 0x85 -> failwith "0x85 <res 0,l> not implemented"
    | 0x86 -> failwith "0x86 <res 0,(hl)> not implemented"
    | 0x87 -> failwith "0x87 <res 0,a> not implemented"
    | 0x88 -> failwith "0x88 <res 1,b> not implemented"
    | 0x89 -> failwith "0x89 <res 1,c> not implemented"
    | 0x8A -> failwith "0x8A <res 1,d> not implemented"
    | 0x8B -> failwith "0x8B <res 1,e> not implemented"
    | 0x8C -> failwith "0x8C <res 1,h> not implemented"
    | 0x8D -> failwith "0x8D <res 1,l> not implemented"
    | 0x8E -> failwith "0x8E <res 1,(hl)> not implemented"
    | 0x8F -> failwith "0x8F <res 1,a> not implemented"
    | 0x90 -> failwith "0x90 <res 2,b> not implemented"
    | 0x91 -> failwith "0x91 <res 2,c> not implemented"
    | 0x92 -> failwith "0x92 <res 2,d> not implemented"
    | 0x93 -> failwith "0x93 <res 2,e> not implemented"
    | 0x94 -> failwith "0x94 <res 2,h> not implemented"
    | 0x95 -> failwith "0x95 <res 2,l> not implemented"
    | 0x96 -> failwith "0x96 <res 2,(hl)> not implemented"
    | 0x97 -> failwith "0x97 <res 2,a> not implemented"
    | 0x98 -> failwith "0x98 <res 3,b> not implemented"
    | 0x99 -> failwith "0x99 <res 3,c> not implemented"
    | 0x9A -> failwith "0x9A <res 3,d> not implemented"
    | 0x9B -> failwith "0x9B <res 3,e> not implemented"
    | 0x9C -> failwith "0x9C <res 3,h> not implemented"
    | 0x9D -> failwith "0x9D <res 3,l> not implemented"
    | 0x9E -> failwith "0x9E <res 3,(hl)> not implemented"
    | 0x9F -> failwith "0x9F <res 3,a> not implemented"
    | 0xA0 -> failwith "0xA0 <res 4,b> not implemented"
    | 0xA1 -> failwith "0xA1 <res 4,c> not implemented"
    | 0xA2 -> failwith "0xA2 <res 4,d> not implemented"
    | 0xA3 -> failwith "0xA3 <res 4,e> not implemented"
    | 0xA4 -> failwith "0xA4 <res 4,h> not implemented"
    | 0xA5 -> failwith "0xA5 <res 4,l> not implemented"
    | 0xA6 -> failwith "0xA6 <res 4,(hl)> not implemented"
    | 0xA7 -> failwith "0xA7 <res 4,a> not implemented"
    | 0xA8 -> failwith "0xA8 <res 5,b> not implemented"
    | 0xA9 -> failwith "0xA9 <res 5,c> not implemented"
    | 0xAA -> failwith "0xAA <res 5,d> not implemented"
    | 0xAB -> failwith "0xAB <res 5,e> not implemented"
    | 0xAC -> failwith "0xAC <res 5,h> not implemented"
    | 0xAD -> failwith "0xAD <res 5,l> not implemented"
    | 0xAE -> failwith "0xAE <res 5,(hl)> not implemented"
    | 0xAF -> failwith "0xAF <res 5,a> not implemented"
    | 0xB0 -> failwith "0xB0 <res 6,b> not implemented"
    | 0xB1 -> failwith "0xB1 <res 6,c> not implemented"
    | 0xB2 -> failwith "0xB2 <res 6,d> not implemented"
    | 0xB3 -> failwith "0xB3 <res 6,e> not implemented"
    | 0xB4 -> failwith "0xB4 <res 6,h> not implemented"
    | 0xB5 -> failwith "0xB5 <res 6,l> not implemented"
    | 0xB6 -> failwith "0xB6 <res 6,(hl)> not implemented"
    | 0xB7 -> failwith "0xB7 <res 6,a> not implemented"
    | 0xB8 -> failwith "0xB8 <res 7,b> not implemented"
    | 0xB9 -> failwith "0xB9 <res 7,c> not implemented"
    | 0xBA -> failwith "0xBA <res 7,d> not implemented"
    | 0xBB -> failwith "0xBB <res 7,e> not implemented"
    | 0xBC -> failwith "0xBC <res 7,h> not implemented"
    | 0xBD -> failwith "0xBD <res 7,l> not implemented"
    | 0xBE -> failwith "0xBE <res 7,(hl)> not implemented"
    | 0xBF -> failwith "0xBF <res 7,a> not implemented"
    | 0xC0 -> failwith "0xC0 <set 0,b> not implemented"
    | 0xC1 -> failwith "0xC1 <set 0,c> not implemented"
    | 0xC2 -> failwith "0xC2 <set 0,d> not implemented"
    | 0xC3 -> failwith "0xC3 <set 0,e> not implemented"
    | 0xC4 -> failwith "0xC4 <set 0,h> not implemented"
    | 0xC5 -> failwith "0xC5 <set 0,l> not implemented"
    | 0xC6 -> failwith "0xC6 <set 0,(hl)> not implemented"
    | 0xC7 -> failwith "0xC7 <set 0,a> not implemented"
    | 0xC8 -> failwith "0xC8 <set 1,b> not implemented"
    | 0xC9 -> failwith "0xC9 <set 1,c> not implemented"
    | 0xCA -> failwith "0xCA <set 1,d> not implemented"
    | 0xCB -> failwith "0xCB <set 1,e> not implemented"
    | 0xCC -> failwith "0xCC <set 1,h> not implemented"
    | 0xCD -> failwith "0xCD <set 1,l> not implemented"
    | 0xCE -> failwith "0xCE <set 1,(hl)> not implemented"
    | 0xCF -> failwith "0xCF <set 1,a> not implemented"
    | 0xD0 -> failwith "0xD0 <set 2,b> not implemented"
    | 0xD1 -> failwith "0xD1 <set 2,c> not implemented"
    | 0xD2 -> failwith "0xD2 <set 2,d> not implemented"
    | 0xD3 -> failwith "0xD3 <set 2,e> not implemented"
    | 0xD4 -> failwith "0xD4 <set 2,h> not implemented"
    | 0xD5 -> failwith "0xD5 <set 2,l> not implemented"
    | 0xD6 -> failwith "0xD6 <set 2,(hl)> not implemented"
    | 0xD7 -> failwith "0xD7 <set 2,a> not implemented"
    | 0xD8 -> failwith "0xD8 <set 3,b> not implemented"
    | 0xD9 -> failwith "0xD9 <set 3,c> not implemented"
    | 0xDA -> failwith "0xDA <set 3,d> not implemented"
    | 0xDB -> failwith "0xDB <set 3,e> not implemented"
    | 0xDC -> failwith "0xDC <set 3,h> not implemented"
    | 0xDD -> failwith "0xDD <set 3,l> not implemented"
    | 0xDE -> failwith "0xDE <set 3,(hl)> not implemented"
    | 0xDF -> failwith "0xDF <set 3,a> not implemented"
    | 0xE0 -> failwith "0xE0 <set 4,b> not implemented"
    | 0xE1 -> failwith "0xE1 <set 4,c> not implemented"
    | 0xE2 -> failwith "0xE2 <set 4,d> not implemented"
    | 0xE3 -> failwith "0xE3 <set 4,e> not implemented"
    | 0xE4 -> failwith "0xE4 <set 4,h> not implemented"
    | 0xE5 -> failwith "0xE5 <set 4,l> not implemented"
    | 0xE6 -> failwith "0xE6 <set 4,(hl)> not implemented"
    | 0xE7 -> failwith "0xE7 <set 4,a> not implemented"
    | 0xE8 -> failwith "0xE8 <set 5,b> not implemented"
    | 0xE9 -> failwith "0xE9 <set 5,c> not implemented"
    | 0xEA -> failwith "0xEA <set 5,d> not implemented"
    | 0xEB -> failwith "0xEB <set 5,e> not implemented"
    | 0xEC -> failwith "0xEC <set 5,h> not implemented"
    | 0xED -> failwith "0xED <set 5,l> not implemented"
    | 0xEE -> failwith "0xEE <set 5,(hl)> not implemented"
    | 0xEF -> failwith "0xEF <set 5,a> not implemented"
    | 0xF0 -> failwith "0xF0 <set 6,b> not implemented"
    | 0xF1 -> failwith "0xF1 <set 6,c> not implemented"
    | 0xF2 -> failwith "0xF2 <set 6,d> not implemented"
    | 0xF3 -> failwith "0xF3 <set 6,e> not implemented"
    | 0xF4 -> failwith "0xF4 <set 6,h> not implemented"
    | 0xF5 -> failwith "0xF5 <set 6,l> not implemented"
    | 0xF6 -> failwith "0xF6 <set 6,(hl)> not implemented"
    | 0xF7 -> failwith "0xF7 <set 6,a> not implemented"
    | 0xF8 -> failwith "0xF8 <set 7,b> not implemented"
    | 0xF9 -> failwith "0xF9 <set 7,c> not implemented"
    | 0xFA -> failwith "0xFA <set 7,d> not implemented"
    | 0xFB -> failwith "0xFB <set 7,e> not implemented"
    | 0xFC -> failwith "0xFC <set 7,h> not implemented"
    | 0xFD -> failwith "0xFD <set 7,l> not implemented"
    | 0xFE -> failwith "0xFE <set 7,(hl)> not implemented"
    | 0xFF -> failwith "0xFF <set 7,a> not implemented"
    | _ -> failwith $"There should be no unused 2-byte opcodes. {opcode:X2} is not implemented."

let fetchAndDecode (memory: Memory) (pc: uint16) : DecodedInstruction =
    let opcode = int memory[pc]

    let withUint8 () = memory[pc + 1us]
    let withImmediate () = Immediate (withUint8 ())
    let withInt8 () = int8 memory[pc + 1us]

    let withUint16 () = getWordFromMemory memory (pc + 1us)

    match opcode with
    | 0x00 -> Nop
    | 0x01 -> LdReg16FromWord (BC, withUint16 ()) |> Load
    | 0x02 -> LdAtBCFromA |> Load
    | 0x03 -> IncReg16 BC |> Arithmetic // "0x03 <inc bc> not implemented"
    | 0x04 -> IncReg8 B |> Arithmetic // "0x04 <inc b> not implemented"
    | 0x05 -> DecReg8 B |> Arithmetic
    | 0x06 -> LdReg8 (B, withImmediate ()) |> Load
    | 0x07 -> failwith "0x07 <rlca> not implemented"
    | 0x08 -> LdAtWordFromSP (withUint16 ()) |> Load
    | 0x09 -> AddHL BC |> Arithmetic
    | 0x0A -> LdAFromAtBC |> Load
    | 0x0B -> DecReg16 BC |> Arithmetic // "0x0B <dec bc> not implemented"
    | 0x0C -> IncReg8 C |> Arithmetic
    | 0x0D -> DecReg8 C |> Arithmetic // "0x0D <dec c> not implemented"
    | 0x0E -> LdReg8 (C, withImmediate ()) |> Load
    | 0x0F -> failwith "0x0F <rrca> not implemented"
    | 0x10 -> failwith "0x10 <stop 0> not implemented"
    | 0x11 -> LdReg16FromWord (DE, withUint16 ()) |> Load
    | 0x12 -> LdAtDEFromA |> Load
    | 0x13 -> IncReg16 DE |> Arithmetic // "0x13 <inc de> not implemented"
    | 0x14 -> IncReg8 D |> Arithmetic // "0x14 <inc d> not implemented"
    | 0x15 -> DecReg8 D |> Arithmetic // "0x15 <dec d> not implemented"
    | 0x16 -> LdReg8 (D, withImmediate ()) |> Load
    | 0x17 -> RlA |> Bitwise
    | 0x18 -> Jr (withInt8 ()) |> Control
    | 0x19 -> AddHL DE |> Arithmetic
    | 0x1A -> LdAFromAtDE |> Load
    | 0x1B -> DecReg16 DE |> Arithmetic // "0x1B <dec de> not implemented"
    | 0x1C -> IncReg8 E |> Arithmetic // "0x1C <inc e> not implemented"
    | 0x1D -> DecReg8 E |> Arithmetic // "0x1D <dec e> not implemented"
    | 0x1E -> LdReg8 (E, withImmediate ()) |> Load
    | 0x1F -> failwith "0x1F <rra> not implemented"
    | 0x20 -> JrCond (Condition.NotZero, withInt8 ()) |> Control
    | 0x21 -> LdReg16FromWord (HL, withUint16 ()) |> Load
    | 0x22 -> LdAtHLIncFromA |> Load
    | 0x23 -> IncReg16 HL |> Arithmetic // "0x23 <inc hl> not implemented"
    | 0x24 -> IncReg8 H |> Arithmetic // "0x24 <inc h> not implemented"
    | 0x25 -> DecReg8 H |> Arithmetic // "0x25 <dec h> not implemented"
    | 0x26 -> LdReg8 (H, withImmediate ()) |> Load
    | 0x27 -> failwith "0x27 <daa> not implemented"
    | 0x28 -> JrCond (Condition.Zero, withInt8 ()) |> Control
    | 0x29 -> AddHL HL |> Arithmetic
    | 0x2A -> LdAFromAtHLInc |> Load
    | 0x2B -> DecReg16 HL |> Arithmetic // "0x2B <dec hl> not implemented"
    | 0x2C -> IncReg8 L |> Arithmetic // "0x2C <inc l> not implemented"
    | 0x2D -> DecReg8 L |> Arithmetic // "0x2D <dec l> not implemented"
    | 0x2E -> LdReg8 (L, withImmediate ()) |> Load
    | 0x2F -> failwith "0x2F <cpl> not implemented"
    | 0x30 -> JrCond (Condition.NoCarry, withInt8 ()) |> Control
    | 0x31 -> LdReg16FromWord (SP, withUint16 ()) |> Load
    | 0x32 -> LdAtHLDecFromA |> Load
    | 0x33 -> IncReg16 SP |> Arithmetic // "0x33 <inc sp> not implemented"
    | 0x34 -> IncAtHL |> Arithmetic // "0x34 <inc (hl)> not implemented"
    | 0x35 -> DecAtHL |> Arithmetic // "0x35 <dec (hl)> not implemented"
    | 0x36 -> LdAtHLFromByte (withUint8 ()) |> Load
    | 0x37 -> failwith "0x37 <scf> not implemented"
    | 0x38 -> JrCond (Condition.Carry, withInt8 ()) |> Control
    | 0x39 -> AddHL SP |> Arithmetic
    | 0x3A -> LdAFromAtHLDec |> Load
    | 0x3B -> DecReg16 SP |> Arithmetic // "0x3B <dec sp> not implemented"
    | 0x3C -> IncReg8 A |> Arithmetic // "0x3C <inc a> not implemented"
    | 0x3D -> DecReg8 A |> Arithmetic // "0x3D <dec a> not implemented"
    | 0x3E -> LdReg8 (A, withImmediate ()) |> Load
    | 0x3F -> failwith "0x3F <ccf> not implemented"
    | 0x40 -> LdReg8 (B, RegDirect B) |> Load
    | 0x41 -> LdReg8 (B, RegDirect C) |> Load
    | 0x42 -> LdReg8 (B, RegDirect D) |> Load
    | 0x43 -> LdReg8 (B, RegDirect E) |> Load
    | 0x44 -> LdReg8 (B, RegDirect H) |> Load
    | 0x45 -> LdReg8 (B, RegDirect L) |> Load
    | 0x46 -> LdReg8 (B, HLIndirect) |> Load
    | 0x47 -> LdReg8 (B, RegDirect A) |> Load
    | 0x48 -> LdReg8 (C, RegDirect B) |> Load
    | 0x49 -> LdReg8 (C, RegDirect C) |> Load
    | 0x4A -> LdReg8 (C, RegDirect D) |> Load
    | 0x4B -> LdReg8 (C, RegDirect E) |> Load
    | 0x4C -> LdReg8 (C, RegDirect H) |> Load
    | 0x4D -> LdReg8 (C, RegDirect L) |> Load
    | 0x4E -> LdReg8 (C, HLIndirect) |> Load
    | 0x4F -> LdReg8 (C, RegDirect A) |> Load
    | 0x50 -> LdReg8 (D, RegDirect B) |> Load
    | 0x51 -> LdReg8 (D, RegDirect C) |> Load
    | 0x52 -> LdReg8 (D, RegDirect D) |> Load
    | 0x53 -> LdReg8 (D, RegDirect E) |> Load
    | 0x54 -> LdReg8 (D, RegDirect H) |> Load
    | 0x55 -> LdReg8 (D, RegDirect L) |> Load
    | 0x56 -> LdReg8 (D, HLIndirect) |> Load
    | 0x57 -> LdReg8 (D, RegDirect A) |> Load
    | 0x58 -> LdReg8 (E, RegDirect B) |> Load
    | 0x59 -> LdReg8 (E, RegDirect C) |> Load
    | 0x5A -> LdReg8 (E, RegDirect D) |> Load
    | 0x5B -> LdReg8 (E, RegDirect E) |> Load
    | 0x5C -> LdReg8 (E, RegDirect H) |> Load
    | 0x5D -> LdReg8 (E, RegDirect L) |> Load
    | 0x5E -> LdReg8 (E, HLIndirect) |> Load
    | 0x5F -> LdReg8 (E, RegDirect A) |> Load
    | 0x60 -> LdReg8 (H, RegDirect B) |> Load
    | 0x61 -> LdReg8 (H, RegDirect C) |> Load
    | 0x62 -> LdReg8 (H, RegDirect D) |> Load
    | 0x63 -> LdReg8 (H, RegDirect E) |> Load
    | 0x64 -> LdReg8 (H, RegDirect H) |> Load
    | 0x65 -> LdReg8 (H, RegDirect L) |> Load
    | 0x66 -> LdReg8 (H, HLIndirect) |> Load
    | 0x67 -> LdReg8 (H, RegDirect A) |> Load
    | 0x68 -> LdReg8 (L, RegDirect B) |> Load
    | 0x69 -> LdReg8 (L, RegDirect C) |> Load
    | 0x6A -> LdReg8 (L, RegDirect D) |> Load
    | 0x6B -> LdReg8 (L, RegDirect E) |> Load
    | 0x6C -> LdReg8 (L, RegDirect H) |> Load
    | 0x6D -> LdReg8 (L, RegDirect L) |> Load
    | 0x6E -> LdReg8 (L, HLIndirect) |> Load
    | 0x6F -> LdReg8 (L, RegDirect A) |> Load
    | 0x70 -> LdAtHLFromReg8 B |> Load
    | 0x71 -> LdAtHLFromReg8 C |> Load
    | 0x72 -> LdAtHLFromReg8 D |> Load
    | 0x73 -> LdAtHLFromReg8 E |> Load
    | 0x74 -> LdAtHLFromReg8 H |> Load
    | 0x75 -> LdAtHLFromReg8 L |> Load
    | 0x76 -> failwith "0x76 <halt> not implemented"
    | 0x77 -> LdAtHLFromReg8 A |> Load
    | 0x78 -> LdReg8 (A, RegDirect B) |> Load
    | 0x79 -> LdReg8 (A, RegDirect C) |> Load
    | 0x7A -> LdReg8 (A, RegDirect D) |> Load
    | 0x7B -> LdReg8 (A, RegDirect E) |> Load
    | 0x7C -> LdReg8 (A, RegDirect H) |> Load
    | 0x7D -> LdReg8 (A, RegDirect L) |> Load
    | 0x7E -> LdReg8 (A, HLIndirect) |> Load
    | 0x7F -> LdReg8 (A, RegDirect A) |> Load
    | 0x80 -> Add (RegDirect B) |> Arithmetic
    | 0x81 -> Add (RegDirect C) |> Arithmetic
    | 0x82 -> Add (RegDirect D) |> Arithmetic
    | 0x83 -> Add (RegDirect E) |> Arithmetic
    | 0x84 -> Add (RegDirect H) |> Arithmetic
    | 0x85 -> Add (RegDirect L) |> Arithmetic
    | 0x86 -> Add HLIndirect |> Arithmetic
    | 0x87 -> Add (RegDirect A) |> Arithmetic
    | 0x88 -> Adc (RegDirect B) |> Arithmetic
    | 0x89 -> Adc (RegDirect C) |> Arithmetic
    | 0x8A -> Adc (RegDirect D) |> Arithmetic
    | 0x8B -> Adc (RegDirect E) |> Arithmetic
    | 0x8C -> Adc (RegDirect H) |> Arithmetic
    | 0x8D -> Adc (RegDirect L) |> Arithmetic
    | 0x8E -> Adc HLIndirect |> Arithmetic
    | 0x8F -> Adc (RegDirect A) |> Arithmetic
    | 0x90 -> Sub (RegDirect B) |> Arithmetic
    | 0x91 -> Sub (RegDirect C) |> Arithmetic
    | 0x92 -> Sub (RegDirect D) |> Arithmetic
    | 0x93 -> Sub (RegDirect E) |> Arithmetic
    | 0x94 -> Sub (RegDirect H) |> Arithmetic
    | 0x95 -> Sub (RegDirect L) |> Arithmetic
    | 0x96 -> Sub HLIndirect |> Arithmetic
    | 0x97 -> Sub (RegDirect A) |> Arithmetic
    | 0x98 -> Sbc (RegDirect B) |> Arithmetic
    | 0x99 -> Sbc (RegDirect C) |> Arithmetic
    | 0x9A -> Sbc (RegDirect D) |> Arithmetic
    | 0x9B -> Sbc (RegDirect E) |> Arithmetic
    | 0x9C -> Sbc (RegDirect H) |> Arithmetic
    | 0x9D -> Sbc (RegDirect L) |> Arithmetic
    | 0x9E -> Sbc HLIndirect |> Arithmetic
    | 0x9F -> Sbc (RegDirect A) |> Arithmetic
    | 0xA0 -> failwith "0xA0 <and b> not implemented"
    | 0xA1 -> failwith "0xA1 <and c> not implemented"
    | 0xA2 -> failwith "0xA2 <and d> not implemented"
    | 0xA3 -> failwith "0xA3 <and e> not implemented"
    | 0xA4 -> failwith "0xA4 <and h> not implemented"
    | 0xA5 -> failwith "0xA5 <and l> not implemented"
    | 0xA6 -> failwith "0xA6 <and (hl)> not implemented"
    | 0xA7 -> failwith "0xA7 <and a> not implemented"
    | 0xA8 -> failwith "0xA8 <xor b> not implemented"
    | 0xA9 -> failwith "0xA9 <xor c> not implemented"
    | 0xAA -> failwith "0xAA <xor d> not implemented"
    | 0xAB -> failwith "0xAB <xor e> not implemented"
    | 0xAC -> failwith "0xAC <xor h> not implemented"
    | 0xAD -> failwith "0xAD <xor l> not implemented"
    | 0xAE -> failwith "0xAE <xor (hl)> not implemented"
    | 0xAF -> Xor8 A |> Logic
    | 0xB0 -> failwith "0xB0 <or b> not implemented"
    | 0xB1 -> failwith "0xB1 <or c> not implemented"
    | 0xB2 -> failwith "0xB2 <or d> not implemented"
    | 0xB3 -> failwith "0xB3 <or e> not implemented"
    | 0xB4 -> failwith "0xB4 <or h> not implemented"
    | 0xB5 -> failwith "0xB5 <or l> not implemented"
    | 0xB6 -> failwith "0xB6 <or (hl)> not implemented"
    | 0xB7 -> failwith "0xB7 <or a> not implemented"
    | 0xB8 -> Cp (RegDirect B) |> Arithmetic
    | 0xB9 -> Cp (RegDirect C) |> Arithmetic
    | 0xBA -> Cp (RegDirect D) |> Arithmetic
    | 0xBB -> Cp (RegDirect E) |> Arithmetic
    | 0xBC -> Cp (RegDirect H) |> Arithmetic
    | 0xBD -> Cp (RegDirect L) |> Arithmetic
    | 0xBE -> Cp HLIndirect |> Arithmetic
    | 0xBF -> Cp (RegDirect A) |> Arithmetic
    | 0xC0 -> RetCond Condition.NotZero |> Control
    | 0xC1 -> Pop BC |> Load
    | 0xC2 -> JpCond (Condition.NotZero, withUint16 ()) |> Control
    | 0xC3 -> Jp (withUint16 ()) |> Control
    | 0xC4 -> CallCond (Condition.NotZero, withUint16 ()) |> Control
    | 0xC5 -> Push BC |> Load
    | 0xC6 -> Add (withImmediate ()) |> Arithmetic
    | 0xC7 -> Rst 0x00uy |> Control
    | 0xC8 -> RetCond Condition.Zero |> Control
    | 0xC9 -> Ret |> Control
    | 0xCA -> JpCond (Condition.Zero, withUint16 ()) |> Control
    | 0xCB -> fetchAndDecode2Byte memory pc
    | 0xCC -> CallCond (Condition.Zero, withUint16 ()) |> Control
    | 0xCD -> Call (withUint16 ()) |> Control
    | 0xCE -> Adc (withImmediate ()) |> Arithmetic
    | 0xCF -> Rst 0x08uy |> Control
    | 0xD0 -> RetCond Condition.NoCarry |> Control
    | 0xD1 -> Pop DE |> Load
    | 0xD2 -> JpCond (Condition.NoCarry, withUint16 ()) |> Control
    | 0xD4 -> CallCond (Condition.NoCarry, withUint16 ()) |> Control
    | 0xD5 -> Push DE |> Load
    | 0xD6 -> Sub (withImmediate ()) |> Arithmetic
    | 0xD7 -> Rst 0x10uy |> Control
    | 0xD8 -> RetCond Condition.Carry |> Control
    | 0xD9 -> Reti |> Control
    | 0xDA -> JpCond (Condition.Carry, withUint16 ()) |> Control
    | 0xDC -> CallCond (Condition.Carry, withUint16 ()) |> Control
    | 0xDE -> Sbc (withImmediate ()) |> Arithmetic
    | 0xDF -> Rst 0x18uy |> Control
    | 0xE0 -> LdhAtByteFromA (withUint8 ()) |> Load
    | 0xE1 -> Pop HL |> Load
    | 0xE2 -> LdhAtCFromA |> Load
    | 0xE5 -> Push HL |> Load
    | 0xE6 -> failwith "0xE6 <and d8> not implemented"
    | 0xE7 -> Rst 0x20uy |> Control
    | 0xE8 -> AddSPe (withInt8 ()) |> Arithmetic
    | 0xE9 -> JpHL |> Control
    | 0xEA -> LdAtWordFromA (withUint16 ()) |> Load
    | 0xEE -> failwith "0xEE <xor d8> not implemented"
    | 0xEF -> Rst 0x28uy |> Control
    | 0xF0 -> LdhAFromAByte (withUint8 ()) |> Load
    | 0xF1 -> Pop AF |> Load
    | 0xF2 -> LdhAFromAtC |> Load
    | 0xF3 -> failwith "0xF3 <di> not implemented"
    | 0xF5 -> Push AF |> Load
    | 0xF6 -> failwith "0xF6 <or d8> not implemented"
    | 0xF7 -> Rst 0x30uy |> Control
    | 0xF8 -> LdHLFromSPe (withInt8 ()) |> Load
    | 0xF9 -> LdSPFromHL |> Load
    | 0xFA -> LdAFromAtWord (withUint16 ()) |> Load
    | 0xFB -> failwith "0xFB <ei> not implemented"
    | 0xFE -> Cp (withImmediate ()) |> Arithmetic
    | 0xFF -> Rst 0x38uy |> Control
    | _ -> Unknown
    |> withLengthAndCycles
