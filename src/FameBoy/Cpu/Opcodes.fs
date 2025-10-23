module FameBoy.Cpu.Opcodes

open FameBoy.Cpu.Instructions
open FameBoy.Cpu.Instructions.ByteSource
open FameBoy.Cpu.Instructions.LoadTypes
open FameBoy.Cpu.State
open FameBoy.Cpu.Utils

let private fetchAndDecode2Byte (memory: Memory) (pc: uint16) =
    let opcode = memory[pc + 1us]

    match opcode with
    | 0x00uy -> failwith "0x00 <rlc b> not implemented"
    | 0x01uy -> failwith "0x01 <rlc c> not implemented"
    | 0x02uy -> failwith "0x02 <rlc d> not implemented"
    | 0x03uy -> failwith "0x03 <rlc e> not implemented"
    | 0x04uy -> failwith "0x04 <rlc h> not implemented"
    | 0x05uy -> failwith "0x05 <rlc l> not implemented"
    | 0x06uy -> failwith "0x06 <rlc (hl)> not implemented"
    | 0x07uy -> failwith "0x07 <rlc a> not implemented"
    | 0x08uy -> failwith "0x08 <rrc b> not implemented"
    | 0x09uy -> failwith "0x09 <rrc c> not implemented"
    | 0x0Auy -> failwith "0x0A <rrc d> not implemented"
    | 0x0Buy -> failwith "0x0B <rrc e> not implemented"
    | 0x0Cuy -> failwith "0x0C <rrc h> not implemented"
    | 0x0Duy -> failwith "0x0D <rrc l> not implemented"
    | 0x0Euy -> failwith "0x0E <rrc (hl)> not implemented"
    | 0x0Fuy -> failwith "0x0F <rrc a> not implemented"
    | 0x10uy -> failwith "0x10 <rl b> not implemented"
    | 0x11uy -> RlReg8 C |> Bitwise
    | 0x12uy -> failwith "0x12 <rl d> not implemented"
    | 0x13uy -> failwith "0x13 <rl e> not implemented"
    | 0x14uy -> failwith "0x14 <rl h> not implemented"
    | 0x15uy -> failwith "0x15 <rl l> not implemented"
    | 0x16uy -> failwith "0x16 <rl (hl)> not implemented"
    | 0x17uy -> failwith "0x17 <rl a> not implemented"
    | 0x18uy -> failwith "0x18 <rr b> not implemented"
    | 0x19uy -> failwith "0x19 <rr c> not implemented"
    | 0x1Auy -> failwith "0x1A <rr d> not implemented"
    | 0x1Buy -> failwith "0x1B <rr e> not implemented"
    | 0x1Cuy -> failwith "0x1C <rr h> not implemented"
    | 0x1Duy -> failwith "0x1D <rr l> not implemented"
    | 0x1Euy -> failwith "0x1E <rr (hl)> not implemented"
    | 0x1Fuy -> failwith "0x1F <rr a> not implemented"
    | 0x20uy -> failwith "0x20 <sla b> not implemented"
    | 0x21uy -> failwith "0x21 <sla c> not implemented"
    | 0x22uy -> failwith "0x22 <sla d> not implemented"
    | 0x23uy -> failwith "0x23 <sla e> not implemented"
    | 0x24uy -> failwith "0x24 <sla h> not implemented"
    | 0x25uy -> failwith "0x25 <sla l> not implemented"
    | 0x26uy -> failwith "0x26 <sla (hl)> not implemented"
    | 0x27uy -> failwith "0x27 <sla a> not implemented"
    | 0x28uy -> failwith "0x28 <sra b> not implemented"
    | 0x29uy -> failwith "0x29 <sra c> not implemented"
    | 0x2Auy -> failwith "0x2A <sra d> not implemented"
    | 0x2Buy -> failwith "0x2B <sra e> not implemented"
    | 0x2Cuy -> failwith "0x2C <sra h> not implemented"
    | 0x2Duy -> failwith "0x2D <sra l> not implemented"
    | 0x2Euy -> failwith "0x2E <sra (hl)> not implemented"
    | 0x2Fuy -> failwith "0x2F <sra a> not implemented"
    | 0x30uy -> failwith "0x30 <swap b> not implemented"
    | 0x31uy -> failwith "0x31 <swap c> not implemented"
    | 0x32uy -> failwith "0x32 <swap d> not implemented"
    | 0x33uy -> failwith "0x33 <swap e> not implemented"
    | 0x34uy -> failwith "0x34 <swap h> not implemented"
    | 0x35uy -> failwith "0x35 <swap l> not implemented"
    | 0x36uy -> failwith "0x36 <swap (hl)> not implemented"
    | 0x37uy -> failwith "0x37 <swap a> not implemented"
    | 0x38uy -> failwith "0x38 <srl b> not implemented"
    | 0x39uy -> failwith "0x39 <srl c> not implemented"
    | 0x3Auy -> failwith "0x3A <srl d> not implemented"
    | 0x3Buy -> failwith "0x3B <srl e> not implemented"
    | 0x3Cuy -> failwith "0x3C <srl h> not implemented"
    | 0x3Duy -> failwith "0x3D <srl l> not implemented"
    | 0x3Euy -> failwith "0x3E <srl (hl)> not implemented"
    | 0x3Fuy -> failwith "0x3F <srl a> not implemented"
    | 0x40uy -> failwith "0x40 <bit 0,b> not implemented"
    | 0x41uy -> failwith "0x41 <bit 0,c> not implemented"
    | 0x42uy -> failwith "0x42 <bit 0,d> not implemented"
    | 0x43uy -> failwith "0x43 <bit 0,e> not implemented"
    | 0x44uy -> failwith "0x44 <bit 0,h> not implemented"
    | 0x45uy -> failwith "0x45 <bit 0,l> not implemented"
    | 0x46uy -> failwith "0x46 <bit 0,(hl)> not implemented"
    | 0x47uy -> failwith "0x47 <bit 0,a> not implemented"
    | 0x48uy -> failwith "0x48 <bit 1,b> not implemented"
    | 0x49uy -> failwith "0x49 <bit 1,c> not implemented"
    | 0x4Auy -> failwith "0x4A <bit 1,d> not implemented"
    | 0x4Buy -> failwith "0x4B <bit 1,e> not implemented"
    | 0x4Cuy -> failwith "0x4C <bit 1,h> not implemented"
    | 0x4Duy -> failwith "0x4D <bit 1,l> not implemented"
    | 0x4Euy -> failwith "0x4E <bit 1,(hl)> not implemented"
    | 0x4Fuy -> failwith "0x4F <bit 1,a> not implemented"
    | 0x50uy -> failwith "0x50 <bit 2,b> not implemented"
    | 0x51uy -> failwith "0x51 <bit 2,c> not implemented"
    | 0x52uy -> failwith "0x52 <bit 2,d> not implemented"
    | 0x53uy -> failwith "0x53 <bit 2,e> not implemented"
    | 0x54uy -> failwith "0x54 <bit 2,h> not implemented"
    | 0x55uy -> failwith "0x55 <bit 2,l> not implemented"
    | 0x56uy -> failwith "0x56 <bit 2,(hl)> not implemented"
    | 0x57uy -> failwith "0x57 <bit 2,a> not implemented"
    | 0x58uy -> failwith "0x58 <bit 3,b> not implemented"
    | 0x59uy -> failwith "0x59 <bit 3,c> not implemented"
    | 0x5Auy -> failwith "0x5A <bit 3,d> not implemented"
    | 0x5Buy -> failwith "0x5B <bit 3,e> not implemented"
    | 0x5Cuy -> failwith "0x5C <bit 3,h> not implemented"
    | 0x5Duy -> failwith "0x5D <bit 3,l> not implemented"
    | 0x5Euy -> failwith "0x5E <bit 3,(hl)> not implemented"
    | 0x5Fuy -> failwith "0x5F <bit 3,a> not implemented"
    | 0x60uy -> failwith "0x60 <bit 4,b> not implemented"
    | 0x61uy -> failwith "0x61 <bit 4,c> not implemented"
    | 0x62uy -> failwith "0x62 <bit 4,d> not implemented"
    | 0x63uy -> failwith "0x63 <bit 4,e> not implemented"
    | 0x64uy -> failwith "0x64 <bit 4,h> not implemented"
    | 0x65uy -> failwith "0x65 <bit 4,l> not implemented"
    | 0x66uy -> failwith "0x66 <bit 4,(hl)> not implemented"
    | 0x67uy -> failwith "0x67 <bit 4,a> not implemented"
    | 0x68uy -> failwith "0x68 <bit 5,b> not implemented"
    | 0x69uy -> failwith "0x69 <bit 5,c> not implemented"
    | 0x6Auy -> failwith "0x6A <bit 5,d> not implemented"
    | 0x6Buy -> failwith "0x6B <bit 5,e> not implemented"
    | 0x6Cuy -> failwith "0x6C <bit 5,h> not implemented"
    | 0x6Duy -> failwith "0x6D <bit 5,l> not implemented"
    | 0x6Euy -> failwith "0x6E <bit 5,(hl)> not implemented"
    | 0x6Fuy -> failwith "0x6F <bit 5,a> not implemented"
    | 0x70uy -> failwith "0x70 <bit 6,b> not implemented"
    | 0x71uy -> failwith "0x71 <bit 6,c> not implemented"
    | 0x72uy -> failwith "0x72 <bit 6,d> not implemented"
    | 0x73uy -> failwith "0x73 <bit 6,e> not implemented"
    | 0x74uy -> failwith "0x74 <bit 6,h> not implemented"
    | 0x75uy -> failwith "0x75 <bit 6,l> not implemented"
    | 0x76uy -> failwith "0x76 <bit 6,(hl)> not implemented"
    | 0x77uy -> failwith "0x77 <bit 6,a> not implemented"
    | 0x78uy -> failwith "0x78 <bit 7,b> not implemented"
    | 0x79uy -> failwith "0x79 <bit 7,c> not implemented"
    | 0x7Auy -> failwith "0x7A <bit 7,d> not implemented"
    | 0x7Buy -> failwith "0x7B <bit 7,e> not implemented"
    | 0x7Cuy -> Bit (7uy, H) |> Bitwise
    | 0x7Duy -> failwith "0x7D <bit 7,l> not implemented"
    | 0x7Euy -> failwith "0x7E <bit 7,(hl)> not implemented"
    | 0x7Fuy -> failwith "0x7F <bit 7,a> not implemented"
    | 0x80uy -> failwith "0x80 <res 0,b> not implemented"
    | 0x81uy -> failwith "0x81 <res 0,c> not implemented"
    | 0x82uy -> failwith "0x82 <res 0,d> not implemented"
    | 0x83uy -> failwith "0x83 <res 0,e> not implemented"
    | 0x84uy -> failwith "0x84 <res 0,h> not implemented"
    | 0x85uy -> failwith "0x85 <res 0,l> not implemented"
    | 0x86uy -> failwith "0x86 <res 0,(hl)> not implemented"
    | 0x87uy -> failwith "0x87 <res 0,a> not implemented"
    | 0x88uy -> failwith "0x88 <res 1,b> not implemented"
    | 0x89uy -> failwith "0x89 <res 1,c> not implemented"
    | 0x8Auy -> failwith "0x8A <res 1,d> not implemented"
    | 0x8Buy -> failwith "0x8B <res 1,e> not implemented"
    | 0x8Cuy -> failwith "0x8C <res 1,h> not implemented"
    | 0x8Duy -> failwith "0x8D <res 1,l> not implemented"
    | 0x8Euy -> failwith "0x8E <res 1,(hl)> not implemented"
    | 0x8Fuy -> failwith "0x8F <res 1,a> not implemented"
    | 0x90uy -> failwith "0x90 <res 2,b> not implemented"
    | 0x91uy -> failwith "0x91 <res 2,c> not implemented"
    | 0x92uy -> failwith "0x92 <res 2,d> not implemented"
    | 0x93uy -> failwith "0x93 <res 2,e> not implemented"
    | 0x94uy -> failwith "0x94 <res 2,h> not implemented"
    | 0x95uy -> failwith "0x95 <res 2,l> not implemented"
    | 0x96uy -> failwith "0x96 <res 2,(hl)> not implemented"
    | 0x97uy -> failwith "0x97 <res 2,a> not implemented"
    | 0x98uy -> failwith "0x98 <res 3,b> not implemented"
    | 0x99uy -> failwith "0x99 <res 3,c> not implemented"
    | 0x9Auy -> failwith "0x9A <res 3,d> not implemented"
    | 0x9Buy -> failwith "0x9B <res 3,e> not implemented"
    | 0x9Cuy -> failwith "0x9C <res 3,h> not implemented"
    | 0x9Duy -> failwith "0x9D <res 3,l> not implemented"
    | 0x9Euy -> failwith "0x9E <res 3,(hl)> not implemented"
    | 0x9Fuy -> failwith "0x9F <res 3,a> not implemented"
    | 0xA0uy -> failwith "0xA0 <res 4,b> not implemented"
    | 0xA1uy -> failwith "0xA1 <res 4,c> not implemented"
    | 0xA2uy -> failwith "0xA2 <res 4,d> not implemented"
    | 0xA3uy -> failwith "0xA3 <res 4,e> not implemented"
    | 0xA4uy -> failwith "0xA4 <res 4,h> not implemented"
    | 0xA5uy -> failwith "0xA5 <res 4,l> not implemented"
    | 0xA6uy -> failwith "0xA6 <res 4,(hl)> not implemented"
    | 0xA7uy -> failwith "0xA7 <res 4,a> not implemented"
    | 0xA8uy -> failwith "0xA8 <res 5,b> not implemented"
    | 0xA9uy -> failwith "0xA9 <res 5,c> not implemented"
    | 0xAAuy -> failwith "0xAA <res 5,d> not implemented"
    | 0xABuy -> failwith "0xAB <res 5,e> not implemented"
    | 0xACuy -> failwith "0xAC <res 5,h> not implemented"
    | 0xADuy -> failwith "0xAD <res 5,l> not implemented"
    | 0xAEuy -> failwith "0xAE <res 5,(hl)> not implemented"
    | 0xAFuy -> failwith "0xAF <res 5,a> not implemented"
    | 0xB0uy -> failwith "0xB0 <res 6,b> not implemented"
    | 0xB1uy -> failwith "0xB1 <res 6,c> not implemented"
    | 0xB2uy -> failwith "0xB2 <res 6,d> not implemented"
    | 0xB3uy -> failwith "0xB3 <res 6,e> not implemented"
    | 0xB4uy -> failwith "0xB4 <res 6,h> not implemented"
    | 0xB5uy -> failwith "0xB5 <res 6,l> not implemented"
    | 0xB6uy -> failwith "0xB6 <res 6,(hl)> not implemented"
    | 0xB7uy -> failwith "0xB7 <res 6,a> not implemented"
    | 0xB8uy -> failwith "0xB8 <res 7,b> not implemented"
    | 0xB9uy -> failwith "0xB9 <res 7,c> not implemented"
    | 0xBAuy -> failwith "0xBA <res 7,d> not implemented"
    | 0xBBuy -> failwith "0xBB <res 7,e> not implemented"
    | 0xBCuy -> failwith "0xBC <res 7,h> not implemented"
    | 0xBDuy -> failwith "0xBD <res 7,l> not implemented"
    | 0xBEuy -> failwith "0xBE <res 7,(hl)> not implemented"
    | 0xBFuy -> failwith "0xBF <res 7,a> not implemented"
    | 0xC0uy -> failwith "0xC0 <set 0,b> not implemented"
    | 0xC1uy -> failwith "0xC1 <set 0,c> not implemented"
    | 0xC2uy -> failwith "0xC2 <set 0,d> not implemented"
    | 0xC3uy -> failwith "0xC3 <set 0,e> not implemented"
    | 0xC4uy -> failwith "0xC4 <set 0,h> not implemented"
    | 0xC5uy -> failwith "0xC5 <set 0,l> not implemented"
    | 0xC6uy -> failwith "0xC6 <set 0,(hl)> not implemented"
    | 0xC7uy -> failwith "0xC7 <set 0,a> not implemented"
    | 0xC8uy -> failwith "0xC8 <set 1,b> not implemented"
    | 0xC9uy -> failwith "0xC9 <set 1,c> not implemented"
    | 0xCAuy -> failwith "0xCA <set 1,d> not implemented"
    | 0xCBuy -> failwith "0xCB <set 1,e> not implemented"
    | 0xCCuy -> failwith "0xCC <set 1,h> not implemented"
    | 0xCDuy -> failwith "0xCD <set 1,l> not implemented"
    | 0xCEuy -> failwith "0xCE <set 1,(hl)> not implemented"
    | 0xCFuy -> failwith "0xCF <set 1,a> not implemented"
    | 0xD0uy -> failwith "0xD0 <set 2,b> not implemented"
    | 0xD1uy -> failwith "0xD1 <set 2,c> not implemented"
    | 0xD2uy -> failwith "0xD2 <set 2,d> not implemented"
    | 0xD3uy -> failwith "0xD3 <set 2,e> not implemented"
    | 0xD4uy -> failwith "0xD4 <set 2,h> not implemented"
    | 0xD5uy -> failwith "0xD5 <set 2,l> not implemented"
    | 0xD6uy -> failwith "0xD6 <set 2,(hl)> not implemented"
    | 0xD7uy -> failwith "0xD7 <set 2,a> not implemented"
    | 0xD8uy -> failwith "0xD8 <set 3,b> not implemented"
    | 0xD9uy -> failwith "0xD9 <set 3,c> not implemented"
    | 0xDAuy -> failwith "0xDA <set 3,d> not implemented"
    | 0xDBuy -> failwith "0xDB <set 3,e> not implemented"
    | 0xDCuy -> failwith "0xDC <set 3,h> not implemented"
    | 0xDDuy -> failwith "0xDD <set 3,l> not implemented"
    | 0xDEuy -> failwith "0xDE <set 3,(hl)> not implemented"
    | 0xDFuy -> failwith "0xDF <set 3,a> not implemented"
    | 0xE0uy -> failwith "0xE0 <set 4,b> not implemented"
    | 0xE1uy -> failwith "0xE1 <set 4,c> not implemented"
    | 0xE2uy -> failwith "0xE2 <set 4,d> not implemented"
    | 0xE3uy -> failwith "0xE3 <set 4,e> not implemented"
    | 0xE4uy -> failwith "0xE4 <set 4,h> not implemented"
    | 0xE5uy -> failwith "0xE5 <set 4,l> not implemented"
    | 0xE6uy -> failwith "0xE6 <set 4,(hl)> not implemented"
    | 0xE7uy -> failwith "0xE7 <set 4,a> not implemented"
    | 0xE8uy -> failwith "0xE8 <set 5,b> not implemented"
    | 0xE9uy -> failwith "0xE9 <set 5,c> not implemented"
    | 0xEAuy -> failwith "0xEA <set 5,d> not implemented"
    | 0xEBuy -> failwith "0xEB <set 5,e> not implemented"
    | 0xECuy -> failwith "0xEC <set 5,h> not implemented"
    | 0xEDuy -> failwith "0xED <set 5,l> not implemented"
    | 0xEEuy -> failwith "0xEE <set 5,(hl)> not implemented"
    | 0xEFuy -> failwith "0xEF <set 5,a> not implemented"
    | 0xF0uy -> failwith "0xF0 <set 6,b> not implemented"
    | 0xF1uy -> failwith "0xF1 <set 6,c> not implemented"
    | 0xF2uy -> failwith "0xF2 <set 6,d> not implemented"
    | 0xF3uy -> failwith "0xF3 <set 6,e> not implemented"
    | 0xF4uy -> failwith "0xF4 <set 6,h> not implemented"
    | 0xF5uy -> failwith "0xF5 <set 6,l> not implemented"
    | 0xF6uy -> failwith "0xF6 <set 6,(hl)> not implemented"
    | 0xF7uy -> failwith "0xF7 <set 6,a> not implemented"
    | 0xF8uy -> failwith "0xF8 <set 7,b> not implemented"
    | 0xF9uy -> failwith "0xF9 <set 7,c> not implemented"
    | 0xFAuy -> failwith "0xFA <set 7,d> not implemented"
    | 0xFBuy -> failwith "0xFB <set 7,e> not implemented"
    | 0xFCuy -> failwith "0xFC <set 7,h> not implemented"
    | 0xFDuy -> failwith "0xFD <set 7,l> not implemented"
    | 0xFEuy -> failwith "0xFE <set 7,(hl)> not implemented"
    | 0xFFuy -> failwith "0xFF <set 7,a> not implemented"

let fetchAndDecode (memory: Memory) (pc: uint16) : DecodedInstruction =
    let opcode = int memory[pc]

    let withUint8 () = memory[pc + 1us]
    let withImmediate () = Read.Immediate (withUint8 ())
    let withInt8 () = int8 memory[pc + 1us]

    let withUint16 () = getWordFromMemory memory (pc + 1us)

    match opcode with
    | 0x00 -> Nop
    | 0x01 -> LdReg16FromWord (BC, withUint16 ()) |> Load
    | 0x02 -> LdA (To, AtBC) |> Load
    | 0x03 -> IncReg16 BC |> Arithmetic
    | 0x04 -> Inc (Write.RegDirect B) |> Arithmetic
    | 0x05 -> Dec (Write.RegDirect B) |> Arithmetic
    | 0x06 -> LdReg8 (B, withImmediate ()) |> Load
    | 0x07 -> failwith "0x07 <rlca> not implemented"
    | 0x08 -> LdAtWordFromSP (withUint16 ()) |> Load
    | 0x09 -> AddHL BC |> Arithmetic
    | 0x0A -> LdA (From, AtBC) |> Load
    | 0x0B -> DecReg16 BC |> Arithmetic
    | 0x0C -> Inc (Write.RegDirect C) |> Arithmetic
    | 0x0D -> Dec (Write.RegDirect C) |> Arithmetic
    | 0x0E -> LdReg8 (C, withImmediate ()) |> Load
    | 0x0F -> failwith "0x0F <rrca> not implemented"
    | 0x10 -> failwith "0x10 <stop 0> not implemented"
    | 0x11 -> LdReg16FromWord (DE, withUint16 ()) |> Load
    | 0x12 -> LdA (To, AtDE) |> Load
    | 0x13 -> IncReg16 DE |> Arithmetic
    | 0x14 -> Inc (Write.RegDirect D) |> Arithmetic
    | 0x15 -> Dec (Write.RegDirect D) |> Arithmetic
    | 0x16 -> LdReg8 (D, withImmediate ()) |> Load
    | 0x17 -> RlA |> Bitwise
    | 0x18 -> Jr (withInt8 ()) |> Control
    | 0x19 -> AddHL DE |> Arithmetic
    | 0x1A -> LdA (From, AtDE) |> Load
    | 0x1B -> DecReg16 DE |> Arithmetic
    | 0x1C -> Inc (Write.RegDirect E) |> Arithmetic
    | 0x1D -> Dec (Write.RegDirect E) |> Arithmetic
    | 0x1E -> LdReg8 (E, withImmediate ()) |> Load
    | 0x1F -> failwith "0x1F <rra> not implemented"
    | 0x20 -> JrCond (Condition.NotZero, withInt8 ()) |> Control
    | 0x21 -> LdReg16FromWord (HL, withUint16 ()) |> Load
    | 0x22 -> LdA (To, AtHLInc) |> Load
    | 0x23 -> IncReg16 HL |> Arithmetic
    | 0x24 -> Inc (Write.RegDirect H) |> Arithmetic
    | 0x25 -> Dec (Write.RegDirect H) |> Arithmetic
    | 0x26 -> LdReg8 (H, withImmediate ()) |> Load
    | 0x27 -> Daa |> Logic
    | 0x28 -> JrCond (Condition.Zero, withInt8 ()) |> Control
    | 0x29 -> AddHL HL |> Arithmetic
    | 0x2A -> LdA (From, AtHLInc) |> Load
    | 0x2B -> DecReg16 HL |> Arithmetic
    | 0x2C -> Inc (Write.RegDirect L) |> Arithmetic
    | 0x2D -> Dec (Write.RegDirect L) |> Arithmetic
    | 0x2E -> LdReg8 (L, withImmediate ()) |> Load
    | 0x2F -> Cpl |> Logic
    | 0x30 -> JrCond (Condition.NoCarry, withInt8 ()) |> Control
    | 0x31 -> LdReg16FromWord (SP, withUint16 ()) |> Load
    | 0x32 -> LdA (To, AtHLDec) |> Load
    | 0x33 -> IncReg16 SP |> Arithmetic
    | 0x34 -> Inc Write.HLIndirect |> Arithmetic
    | 0x35 -> Dec Write.HLIndirect |> Arithmetic
    | 0x36 -> LdAtHLFromByte (withUint8 ()) |> Load
    | 0x37 -> Scf |> Logic
    | 0x38 -> JrCond (Condition.Carry, withInt8 ()) |> Control
    | 0x39 -> AddHL SP |> Arithmetic
    | 0x3A -> LdA (From, AtHLDec) |> Load
    | 0x3B -> DecReg16 SP |> Arithmetic
    | 0x3C -> Inc (Write.RegDirect A) |> Arithmetic
    | 0x3D -> Dec (Write.RegDirect A) |> Arithmetic
    | 0x3E -> LdReg8 (A, withImmediate ()) |> Load
    | 0x3F -> Ccf |> Logic
    | 0x40 -> LdReg8 (B, Read.RegDirect B) |> Load
    | 0x41 -> LdReg8 (B, Read.RegDirect C) |> Load
    | 0x42 -> LdReg8 (B, Read.RegDirect D) |> Load
    | 0x43 -> LdReg8 (B, Read.RegDirect E) |> Load
    | 0x44 -> LdReg8 (B, Read.RegDirect H) |> Load
    | 0x45 -> LdReg8 (B, Read.RegDirect L) |> Load
    | 0x46 -> LdReg8 (B, Read.HLIndirect) |> Load
    | 0x47 -> LdReg8 (B, Read.RegDirect A) |> Load
    | 0x48 -> LdReg8 (C, Read.RegDirect B) |> Load
    | 0x49 -> LdReg8 (C, Read.RegDirect C) |> Load
    | 0x4A -> LdReg8 (C, Read.RegDirect D) |> Load
    | 0x4B -> LdReg8 (C, Read.RegDirect E) |> Load
    | 0x4C -> LdReg8 (C, Read.RegDirect H) |> Load
    | 0x4D -> LdReg8 (C, Read.RegDirect L) |> Load
    | 0x4E -> LdReg8 (C, Read.HLIndirect) |> Load
    | 0x4F -> LdReg8 (C, Read.RegDirect A) |> Load
    | 0x50 -> LdReg8 (D, Read.RegDirect B) |> Load
    | 0x51 -> LdReg8 (D, Read.RegDirect C) |> Load
    | 0x52 -> LdReg8 (D, Read.RegDirect D) |> Load
    | 0x53 -> LdReg8 (D, Read.RegDirect E) |> Load
    | 0x54 -> LdReg8 (D, Read.RegDirect H) |> Load
    | 0x55 -> LdReg8 (D, Read.RegDirect L) |> Load
    | 0x56 -> LdReg8 (D, Read.HLIndirect) |> Load
    | 0x57 -> LdReg8 (D, Read.RegDirect A) |> Load
    | 0x58 -> LdReg8 (E, Read.RegDirect B) |> Load
    | 0x59 -> LdReg8 (E, Read.RegDirect C) |> Load
    | 0x5A -> LdReg8 (E, Read.RegDirect D) |> Load
    | 0x5B -> LdReg8 (E, Read.RegDirect E) |> Load
    | 0x5C -> LdReg8 (E, Read.RegDirect H) |> Load
    | 0x5D -> LdReg8 (E, Read.RegDirect L) |> Load
    | 0x5E -> LdReg8 (E, Read.HLIndirect) |> Load
    | 0x5F -> LdReg8 (E, Read.RegDirect A) |> Load
    | 0x60 -> LdReg8 (H, Read.RegDirect B) |> Load
    | 0x61 -> LdReg8 (H, Read.RegDirect C) |> Load
    | 0x62 -> LdReg8 (H, Read.RegDirect D) |> Load
    | 0x63 -> LdReg8 (H, Read.RegDirect E) |> Load
    | 0x64 -> LdReg8 (H, Read.RegDirect H) |> Load
    | 0x65 -> LdReg8 (H, Read.RegDirect L) |> Load
    | 0x66 -> LdReg8 (H, Read.HLIndirect) |> Load
    | 0x67 -> LdReg8 (H, Read.RegDirect A) |> Load
    | 0x68 -> LdReg8 (L, Read.RegDirect B) |> Load
    | 0x69 -> LdReg8 (L, Read.RegDirect C) |> Load
    | 0x6A -> LdReg8 (L, Read.RegDirect D) |> Load
    | 0x6B -> LdReg8 (L, Read.RegDirect E) |> Load
    | 0x6C -> LdReg8 (L, Read.RegDirect H) |> Load
    | 0x6D -> LdReg8 (L, Read.RegDirect L) |> Load
    | 0x6E -> LdReg8 (L, Read.HLIndirect) |> Load
    | 0x6F -> LdReg8 (L, Read.RegDirect A) |> Load
    | 0x70 -> LdAtHLFromReg8 B |> Load
    | 0x71 -> LdAtHLFromReg8 C |> Load
    | 0x72 -> LdAtHLFromReg8 D |> Load
    | 0x73 -> LdAtHLFromReg8 E |> Load
    | 0x74 -> LdAtHLFromReg8 H |> Load
    | 0x75 -> LdAtHLFromReg8 L |> Load
    | 0x76 -> failwith "0x76 <halt> not implemented"
    | 0x77 -> LdAtHLFromReg8 A |> Load
    | 0x78 -> LdReg8 (A, Read.RegDirect B) |> Load
    | 0x79 -> LdReg8 (A, Read.RegDirect C) |> Load
    | 0x7A -> LdReg8 (A, Read.RegDirect D) |> Load
    | 0x7B -> LdReg8 (A, Read.RegDirect E) |> Load
    | 0x7C -> LdReg8 (A, Read.RegDirect H) |> Load
    | 0x7D -> LdReg8 (A, Read.RegDirect L) |> Load
    | 0x7E -> LdReg8 (A, Read.HLIndirect) |> Load
    | 0x7F -> LdReg8 (A, Read.RegDirect A) |> Load
    | 0x80 -> Add (Read.RegDirect B) |> Arithmetic
    | 0x81 -> Add (Read.RegDirect C) |> Arithmetic
    | 0x82 -> Add (Read.RegDirect D) |> Arithmetic
    | 0x83 -> Add (Read.RegDirect E) |> Arithmetic
    | 0x84 -> Add (Read.RegDirect H) |> Arithmetic
    | 0x85 -> Add (Read.RegDirect L) |> Arithmetic
    | 0x86 -> Add Read.HLIndirect |> Arithmetic
    | 0x87 -> Add (Read.RegDirect A) |> Arithmetic
    | 0x88 -> Adc (Read.RegDirect B) |> Arithmetic
    | 0x89 -> Adc (Read.RegDirect C) |> Arithmetic
    | 0x8A -> Adc (Read.RegDirect D) |> Arithmetic
    | 0x8B -> Adc (Read.RegDirect E) |> Arithmetic
    | 0x8C -> Adc (Read.RegDirect H) |> Arithmetic
    | 0x8D -> Adc (Read.RegDirect L) |> Arithmetic
    | 0x8E -> Adc Read.HLIndirect |> Arithmetic
    | 0x8F -> Adc (Read.RegDirect A) |> Arithmetic
    | 0x90 -> Sub (Read.RegDirect B) |> Arithmetic
    | 0x91 -> Sub (Read.RegDirect C) |> Arithmetic
    | 0x92 -> Sub (Read.RegDirect D) |> Arithmetic
    | 0x93 -> Sub (Read.RegDirect E) |> Arithmetic
    | 0x94 -> Sub (Read.RegDirect H) |> Arithmetic
    | 0x95 -> Sub (Read.RegDirect L) |> Arithmetic
    | 0x96 -> Sub Read.HLIndirect |> Arithmetic
    | 0x97 -> Sub (Read.RegDirect A) |> Arithmetic
    | 0x98 -> Sbc (Read.RegDirect B) |> Arithmetic
    | 0x99 -> Sbc (Read.RegDirect C) |> Arithmetic
    | 0x9A -> Sbc (Read.RegDirect D) |> Arithmetic
    | 0x9B -> Sbc (Read.RegDirect E) |> Arithmetic
    | 0x9C -> Sbc (Read.RegDirect H) |> Arithmetic
    | 0x9D -> Sbc (Read.RegDirect L) |> Arithmetic
    | 0x9E -> Sbc Read.HLIndirect |> Arithmetic
    | 0x9F -> Sbc (Read.RegDirect A) |> Arithmetic
    | 0xA0 -> And (Read.RegDirect B) |> Logic
    | 0xA1 -> And (Read.RegDirect C) |> Logic
    | 0xA2 -> And (Read.RegDirect D) |> Logic
    | 0xA3 -> And (Read.RegDirect E) |> Logic
    | 0xA4 -> And (Read.RegDirect H) |> Logic
    | 0xA5 -> And (Read.RegDirect L) |> Logic
    | 0xA6 -> And Read.HLIndirect |> Logic
    | 0xA7 -> And (Read.RegDirect A) |> Logic
    | 0xA8 -> Xor (Read.RegDirect B) |> Logic
    | 0xA9 -> Xor (Read.RegDirect C) |> Logic
    | 0xAA -> Xor (Read.RegDirect D) |> Logic
    | 0xAB -> Xor (Read.RegDirect E) |> Logic
    | 0xAC -> Xor (Read.RegDirect H) |> Logic
    | 0xAD -> Xor (Read.RegDirect L) |> Logic
    | 0xAE -> Xor Read.HLIndirect |> Logic
    | 0xAF -> Xor (Read.RegDirect A) |> Logic
    | 0xB0 -> Or (Read.RegDirect B) |> Logic
    | 0xB1 -> Or (Read.RegDirect C) |> Logic
    | 0xB2 -> Or (Read.RegDirect D) |> Logic
    | 0xB3 -> Or (Read.RegDirect E) |> Logic
    | 0xB4 -> Or (Read.RegDirect H) |> Logic
    | 0xB5 -> Or (Read.RegDirect L) |> Logic
    | 0xB6 -> Or Read.HLIndirect |> Logic
    | 0xB7 -> Or (Read.RegDirect A) |> Logic
    | 0xB8 -> Cp (Read.RegDirect B) |> Arithmetic
    | 0xB9 -> Cp (Read.RegDirect C) |> Arithmetic
    | 0xBA -> Cp (Read.RegDirect D) |> Arithmetic
    | 0xBB -> Cp (Read.RegDirect E) |> Arithmetic
    | 0xBC -> Cp (Read.RegDirect H) |> Arithmetic
    | 0xBD -> Cp (Read.RegDirect L) |> Arithmetic
    | 0xBE -> Cp Read.HLIndirect |> Arithmetic
    | 0xBF -> Cp (Read.RegDirect A) |> Arithmetic
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
    | 0xE0 -> LdA (To, AtByteHigh (withUint8 ())) |> Load
    | 0xE1 -> Pop HL |> Load
    | 0xE2 -> LdA (To, AtCHigh) |> Load
    | 0xE5 -> Push HL |> Load
    | 0xE6 -> And (withImmediate ()) |> Logic
    | 0xE7 -> Rst 0x20uy |> Control
    | 0xE8 -> AddSPe (withInt8 ()) |> Arithmetic
    | 0xE9 -> JpHL |> Control
    | 0xEA -> LdA (To, AtWord (withUint16 ())) |> Load
    | 0xEE -> Xor (withImmediate ()) |> Logic
    | 0xEF -> Rst 0x28uy |> Control
    | 0xF0 -> LdA (From, AtByteHigh (withUint8 ())) |> Load
    | 0xF1 -> Pop AF |> Load
    | 0xF2 -> LdA (From, AtCHigh) |> Load
    | 0xF3 -> failwith "0xF3 <di> not implemented"
    | 0xF5 -> Push AF |> Load
    | 0xF6 -> Or (withImmediate ()) |> Logic
    | 0xF7 -> Rst 0x30uy |> Control
    | 0xF8 -> LdHLFromSPe (withInt8 ()) |> Load
    | 0xF9 -> LdSPFromHL |> Load
    | 0xFA -> LdA (From, AtWord (withUint16 ())) |> Load
    | 0xFB -> failwith "0xFB <ei> not implemented"
    | 0xFE -> Cp (withImmediate ()) |> Arithmetic
    | 0xFF -> Rst 0x38uy |> Control
    | _ -> Unknown
    |> withLengthAndCycles
