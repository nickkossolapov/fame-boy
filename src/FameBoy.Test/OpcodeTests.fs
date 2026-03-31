module FameBoy.Test.OpcodeTests

open FameBoy.Cpu.Execute
open FameBoy.Cpu.Instructions
open FameBoy.Cpu.Instructions.Operand
open FameBoy.Cpu.Instructions.LoadTypes
open FameBoy.Cpu.Opcodes
open FameBoy.Cpu.State
open FameBoy.Memory
open FameBoy.Test.TestHelpers
open NUnit.Framework

[<Test>]
let ``Check little endian order for 3-byte instruction - ld hl,n16 (L = PC+1, H = PC+2)`` () =
    let opcode = 0x21uy
    let cpu, io = createTestCpu [||]

    cpu.Pc <- 0x100us
    cpu.Memory.Cartridge.Rom[0x100] <- opcode
    cpu.Memory.Cartridge.Rom[0x101] <- 0x34uy
    cpu.Memory.Cartridge.Rom[0x102] <- 0x12uy

    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu io instr |> ignore

    Assert.That(cpu.Registers.H, Is.EqualTo 0x12uy)
    Assert.That(cpu.Registers.L, Is.EqualTo 0x34uy)
    Assert.That(cpu.Registers.HL, Is.EqualTo 0x1234us)

let instructionMappingCases =
    [| 0x00, "nop", Nop
       0x01, "ld bc,d16", Ld16FromWord(BC, 0x0101us) |> Load
       0x02, "ld (bc),a", LdA(To, AtBC) |> Load
       0x03, "inc bc", IncReg16 BC |> Arithmetic
       0x04, "inc b", Inc(Target.RegDirect B) |> Arithmetic
       0x05, "dec b", Dec(Target.RegDirect B) |> Arithmetic
       0x06, "ld b,d8", Ld8(Target.RegDirect B, Source.Immediate 0x01uy) |> Load
       0x07, "rlca", Rlca |> Bitwise
       0x08, "ld (a16),sp", LdAtWordFromSP 0x0101us |> Load
       0x09, "add hl,bc", AddHL BC |> Arithmetic
       0x0A, "ld a,(bc)", LdA(From, AtBC) |> Load
       0x0B, "dec bc", DecReg16 BC |> Arithmetic
       0x0C, "inc c", Inc(Target.RegDirect C) |> Arithmetic
       0x0D, "dec c", Dec(Target.RegDirect C) |> Arithmetic
       0x0E, "ld c,d8", Ld8(Target.RegDirect C, Source.Immediate 0x01uy) |> Load
       0x0F, "rrca", Rrca |> Bitwise
       0x10, "stop", Stop
       0x11, "ld de,d16", Ld16FromWord(DE, 0x0101us) |> Load
       0x12, "ld (de),a", LdA(To, AtDE) |> Load
       0x13, "inc de", IncReg16 DE |> Arithmetic
       0x14, "inc d", Inc(Target.RegDirect D) |> Arithmetic
       0x15, "dec d", Dec(Target.RegDirect D) |> Arithmetic
       0x16, "ld d,d8", Ld8(Target.RegDirect D, Source.Immediate 0x01uy) |> Load
       0x17, "rla", Rla |> Bitwise
       0x18, "jr r8", Jr 0x01y |> Control
       0x19, "add hl,de", AddHL DE |> Arithmetic
       0x1A, "ld a,(de)", LdA(From, AtDE) |> Load
       0x1B, "dec de", DecReg16 DE |> Arithmetic
       0x1C, "inc e", Inc(Target.RegDirect E) |> Arithmetic
       0x1D, "dec e", Dec(Target.RegDirect E) |> Arithmetic
       0x1E, "ld e,d8", Ld8(Target.RegDirect E, Source.Immediate 0x01uy) |> Load
       0x1F, "rra", Rra |> Bitwise
       0x20, "jr nz,r8", JrCond(Condition.NotZero, 0x01y) |> Control
       0x21, "ld hl,d16", Ld16FromWord(HL, 0x0101us) |> Load
       0x22, "ld (hl+),a", LdA(To, AtHLInc) |> Load
       0x23, "inc hl", IncReg16 HL |> Arithmetic
       0x24, "inc h", Inc(Target.RegDirect H) |> Arithmetic
       0x25, "dec h", Dec(Target.RegDirect H) |> Arithmetic
       0x26, "ld h,d8", Ld8(Target.RegDirect H, Source.Immediate 0x01uy) |> Load
       0x27, "daa", Daa |> Logic
       0x28, "jr z,r8", JrCond(Condition.Zero, 0x01y) |> Control
       0x29, "add hl,hl", AddHL HL |> Arithmetic
       0x2A, "ld a,(hl+)", LdA(From, AtHLInc) |> Load
       0x2B, "dec hl", DecReg16 HL |> Arithmetic
       0x2C, "inc l", Inc(Target.RegDirect L) |> Arithmetic
       0x2D, "dec l", Dec(Target.RegDirect L) |> Arithmetic
       0x2E, "ld l,d8", Ld8(Target.RegDirect L, Source.Immediate 0x01uy) |> Load
       0x2F, "cpl", Cpl |> Logic
       0x30, "jr nc,r8", JrCond(Condition.NoCarry, 0x01y) |> Control
       0x31, "ld sp,d16", Ld16FromWord(SP, 0x0101us) |> Load
       0x32, "ld (hl-),a", LdA(To, AtHLDec) |> Load
       0x33, "inc sp", IncReg16 SP |> Arithmetic
       0x34, "inc (hl)", Inc Target.HLIndirect |> Arithmetic
       0x35, "dec (hl)", Dec Target.HLIndirect |> Arithmetic
       0x36, "ld (hl),d8", Ld8(Target.HLIndirect, Source.Immediate 0x01uy) |> Load
       0x37, "scf", Scf |> Logic
       0x38, "jr c,r8", JrCond(Condition.Carry, 0x01y) |> Control
       0x39, "add hl,sp", AddHL SP |> Arithmetic
       0x3A, "ld a,(hl-)", LdA(From, AtHLDec) |> Load
       0x3B, "dec sp", DecReg16 SP |> Arithmetic
       0x3C, "inc a", Inc(Target.RegDirect A) |> Arithmetic
       0x3D, "dec a", Dec(Target.RegDirect A) |> Arithmetic
       0x3E, "ld a,d8", Ld8(Target.RegDirect A, Source.Immediate 0x01uy) |> Load
       0x3F, "ccf", Ccf |> Logic
       0x40, "ld b,b", Ld8(Target.RegDirect B, Source.RegDirect B) |> Load
       0x41, "ld b,c", Ld8(Target.RegDirect B, Source.RegDirect C) |> Load
       0x42, "ld b,d", Ld8(Target.RegDirect B, Source.RegDirect D) |> Load
       0x43, "ld b,e", Ld8(Target.RegDirect B, Source.RegDirect E) |> Load
       0x44, "ld b,h", Ld8(Target.RegDirect B, Source.RegDirect H) |> Load
       0x45, "ld b,l", Ld8(Target.RegDirect B, Source.RegDirect L) |> Load
       0x46, "ld b,(hl)", Ld8(Target.RegDirect B, Source.HLIndirect) |> Load
       0x47, "ld b,a", Ld8(Target.RegDirect B, Source.RegDirect A) |> Load
       0x48, "ld c,b", Ld8(Target.RegDirect C, Source.RegDirect B) |> Load
       0x49, "ld c,c", Ld8(Target.RegDirect C, Source.RegDirect C) |> Load
       0x4A, "ld c,d", Ld8(Target.RegDirect C, Source.RegDirect D) |> Load
       0x4B, "ld c,e", Ld8(Target.RegDirect C, Source.RegDirect E) |> Load
       0x4C, "ld c,h", Ld8(Target.RegDirect C, Source.RegDirect H) |> Load
       0x4D, "ld c,l", Ld8(Target.RegDirect C, Source.RegDirect L) |> Load
       0x4E, "ld c,(hl)", Ld8(Target.RegDirect C, Source.HLIndirect) |> Load
       0x4F, "ld c,a", Ld8(Target.RegDirect C, Source.RegDirect A) |> Load
       0x50, "ld d,b", Ld8(Target.RegDirect D, Source.RegDirect B) |> Load
       0x51, "ld d,c", Ld8(Target.RegDirect D, Source.RegDirect C) |> Load
       0x52, "ld d,d", Ld8(Target.RegDirect D, Source.RegDirect D) |> Load
       0x53, "ld d,e", Ld8(Target.RegDirect D, Source.RegDirect E) |> Load
       0x54, "ld d,h", Ld8(Target.RegDirect D, Source.RegDirect H) |> Load
       0x55, "ld d,l", Ld8(Target.RegDirect D, Source.RegDirect L) |> Load
       0x56, "ld d,(hl)", Ld8(Target.RegDirect D, Source.HLIndirect) |> Load
       0x57, "ld d,a", Ld8(Target.RegDirect D, Source.RegDirect A) |> Load
       0x58, "ld e,b", Ld8(Target.RegDirect E, Source.RegDirect B) |> Load
       0x59, "ld e,c", Ld8(Target.RegDirect E, Source.RegDirect C) |> Load
       0x5A, "ld e,d", Ld8(Target.RegDirect E, Source.RegDirect D) |> Load
       0x5B, "ld e,e", Ld8(Target.RegDirect E, Source.RegDirect E) |> Load
       0x5C, "ld e,h", Ld8(Target.RegDirect E, Source.RegDirect H) |> Load
       0x5D, "ld e,l", Ld8(Target.RegDirect E, Source.RegDirect L) |> Load
       0x5E, "ld e,(hl)", Ld8(Target.RegDirect E, Source.HLIndirect) |> Load
       0x5F, "ld e,a", Ld8(Target.RegDirect E, Source.RegDirect A) |> Load
       0x60, "ld h,b", Ld8(Target.RegDirect H, Source.RegDirect B) |> Load
       0x61, "ld h,c", Ld8(Target.RegDirect H, Source.RegDirect C) |> Load
       0x62, "ld h,d", Ld8(Target.RegDirect H, Source.RegDirect D) |> Load
       0x63, "ld h,e", Ld8(Target.RegDirect H, Source.RegDirect E) |> Load
       0x64, "ld h,h", Ld8(Target.RegDirect H, Source.RegDirect H) |> Load
       0x65, "ld h,l", Ld8(Target.RegDirect H, Source.RegDirect L) |> Load
       0x66, "ld h,(hl)", Ld8(Target.RegDirect H, Source.HLIndirect) |> Load
       0x67, "ld h,a", Ld8(Target.RegDirect H, Source.RegDirect A) |> Load
       0x68, "ld l,b", Ld8(Target.RegDirect L, Source.RegDirect B) |> Load
       0x69, "ld l,c", Ld8(Target.RegDirect L, Source.RegDirect C) |> Load
       0x6A, "ld l,d", Ld8(Target.RegDirect L, Source.RegDirect D) |> Load
       0x6B, "ld l,e", Ld8(Target.RegDirect L, Source.RegDirect E) |> Load
       0x6C, "ld l,h", Ld8(Target.RegDirect L, Source.RegDirect H) |> Load
       0x6D, "ld l,l", Ld8(Target.RegDirect L, Source.RegDirect L) |> Load
       0x6E, "ld l,(hl)", Ld8(Target.RegDirect L, Source.HLIndirect) |> Load
       0x6F, "ld l,a", Ld8(Target.RegDirect L, Source.RegDirect A) |> Load
       0x70, "ld (hl),b", Ld8(Target.HLIndirect, Source.RegDirect B) |> Load
       0x71, "ld (hl),c", Ld8(Target.HLIndirect, Source.RegDirect C) |> Load
       0x72, "ld (hl),d", Ld8(Target.HLIndirect, Source.RegDirect D) |> Load
       0x73, "ld (hl),e", Ld8(Target.HLIndirect, Source.RegDirect E) |> Load
       0x74, "ld (hl),h", Ld8(Target.HLIndirect, Source.RegDirect H) |> Load
       0x75, "ld (hl),l", Ld8(Target.HLIndirect, Source.RegDirect L) |> Load
       0x76, "halt", Halt
       0x77, "ld (hl),a", Ld8(Target.HLIndirect, Source.RegDirect A) |> Load
       0x78, "ld a,b", Ld8(Target.RegDirect A, Source.RegDirect B) |> Load
       0x79, "ld a,c", Ld8(Target.RegDirect A, Source.RegDirect C) |> Load
       0x7A, "ld a,d", Ld8(Target.RegDirect A, Source.RegDirect D) |> Load
       0x7B, "ld a,e", Ld8(Target.RegDirect A, Source.RegDirect E) |> Load
       0x7C, "ld a,h", Ld8(Target.RegDirect A, Source.RegDirect H) |> Load
       0x7D, "ld a,l", Ld8(Target.RegDirect A, Source.RegDirect L) |> Load
       0x7E, "ld a,(hl)", Ld8(Target.RegDirect A, Source.HLIndirect) |> Load
       0x7F, "ld a,a", Ld8(Target.RegDirect A, Source.RegDirect A) |> Load
       0x80, "add a,b", Add(Source.RegDirect B) |> Arithmetic
       0x81, "add a,c", Add(Source.RegDirect C) |> Arithmetic
       0x82, "add a,d", Add(Source.RegDirect D) |> Arithmetic
       0x83, "add a,e", Add(Source.RegDirect E) |> Arithmetic
       0x84, "add a,h", Add(Source.RegDirect H) |> Arithmetic
       0x85, "add a,l", Add(Source.RegDirect L) |> Arithmetic
       0x86, "add a,(hl)", Add Source.HLIndirect |> Arithmetic
       0x87, "add a,a", Add(Source.RegDirect A) |> Arithmetic
       0x88, "adc a,b", Adc(Source.RegDirect B) |> Arithmetic
       0x89, "adc a,c", Adc(Source.RegDirect C) |> Arithmetic
       0x8A, "adc a,d", Adc(Source.RegDirect D) |> Arithmetic
       0x8B, "adc a,e", Adc(Source.RegDirect E) |> Arithmetic
       0x8C, "adc a,h", Adc(Source.RegDirect H) |> Arithmetic
       0x8D, "adc a,l", Adc(Source.RegDirect L) |> Arithmetic
       0x8E, "adc a,(hl)", Adc Source.HLIndirect |> Arithmetic
       0x8F, "adc a,a", Adc(Source.RegDirect A) |> Arithmetic
       0x90, "sub b", Sub(Source.RegDirect B) |> Arithmetic
       0x91, "sub c", Sub(Source.RegDirect C) |> Arithmetic
       0x92, "sub d", Sub(Source.RegDirect D) |> Arithmetic
       0x93, "sub e", Sub(Source.RegDirect E) |> Arithmetic
       0x94, "sub h", Sub(Source.RegDirect H) |> Arithmetic
       0x95, "sub l", Sub(Source.RegDirect L) |> Arithmetic
       0x96, "sub (hl)", Sub Source.HLIndirect |> Arithmetic
       0x97, "sub a", Sub(Source.RegDirect A) |> Arithmetic
       0x98, "sbc a,b", Sbc(Source.RegDirect B) |> Arithmetic
       0x99, "sbc a,c", Sbc(Source.RegDirect C) |> Arithmetic
       0x9A, "sbc a,d", Sbc(Source.RegDirect D) |> Arithmetic
       0x9B, "sbc a,e", Sbc(Source.RegDirect E) |> Arithmetic
       0x9C, "sbc a,h", Sbc(Source.RegDirect H) |> Arithmetic
       0x9D, "sbc a,l", Sbc(Source.RegDirect L) |> Arithmetic
       0x9E, "sbc a,(hl)", Sbc Source.HLIndirect |> Arithmetic
       0x9F, "sbc a,a", Sbc(Source.RegDirect A) |> Arithmetic
       0xA0, "and b", And(Source.RegDirect B) |> Logic
       0xA1, "and c", And(Source.RegDirect C) |> Logic
       0xA2, "and d", And(Source.RegDirect D) |> Logic
       0xA3, "and e", And(Source.RegDirect E) |> Logic
       0xA4, "and h", And(Source.RegDirect H) |> Logic
       0xA5, "and l", And(Source.RegDirect L) |> Logic
       0xA6, "and (hl)", And Source.HLIndirect |> Logic
       0xA7, "and a", And(Source.RegDirect A) |> Logic
       0xA8, "xor b", Xor(Source.RegDirect B) |> Logic
       0xA9, "xor c", Xor(Source.RegDirect C) |> Logic
       0xAA, "xor d", Xor(Source.RegDirect D) |> Logic
       0xAB, "xor e", Xor(Source.RegDirect E) |> Logic
       0xAC, "xor h", Xor(Source.RegDirect H) |> Logic
       0xAD, "xor l", Xor(Source.RegDirect L) |> Logic
       0xAE, "xor (hl)", Xor Source.HLIndirect |> Logic
       0xAF, "xor a", Xor(Source.RegDirect A) |> Logic
       0xB0, "or b", Or(Source.RegDirect B) |> Logic
       0xB1, "or c", Or(Source.RegDirect C) |> Logic
       0xB2, "or d", Or(Source.RegDirect D) |> Logic
       0xB3, "or e", Or(Source.RegDirect E) |> Logic
       0xB4, "or h", Or(Source.RegDirect H) |> Logic
       0xB5, "or l", Or(Source.RegDirect L) |> Logic
       0xB6, "or (hl)", Or Source.HLIndirect |> Logic
       0xB7, "or a", Or(Source.RegDirect A) |> Logic
       0xB8, "cp b", Cp(Source.RegDirect B) |> Arithmetic
       0xB9, "cp c", Cp(Source.RegDirect C) |> Arithmetic
       0xBA, "cp d", Cp(Source.RegDirect D) |> Arithmetic
       0xBB, "cp e", Cp(Source.RegDirect E) |> Arithmetic
       0xBC, "cp h", Cp(Source.RegDirect H) |> Arithmetic
       0xBD, "cp l", Cp(Source.RegDirect L) |> Arithmetic
       0xBE, "cp (hl)", Cp Source.HLIndirect |> Arithmetic
       0xBF, "cp a", Cp(Source.RegDirect A) |> Arithmetic
       0xC0, "ret nz", RetCond Condition.NotZero |> Control
       0xC1, "pop bc", Pop BC |> Load
       0xC2, "jp nz,a16", JpCond(Condition.NotZero, 0x0101us) |> Control
       0xC3, "jp a16", Jp 0x0101us |> Control
       0xC4, "call nz,a16", CallCond(Condition.NotZero, 0x0101us) |> Control
       0xC5, "push bc", Push BC |> Load
       0xC6, "add a,d8", Add(Source.Immediate 0x01uy) |> Arithmetic
       0xC7, "rst 00h", Rst 0x00uy |> Control
       0xC8, "ret z", RetCond Condition.Zero |> Control
       0xC9, "ret", Ret |> Control
       0xCA, "jp z,a16", JpCond(Condition.Zero, 0x0101us) |> Control
       0xCC, "call z,a16", CallCond(Condition.Zero, 0x0101us) |> Control
       0xCD, "call a16", Call 0x0101us |> Control
       0xCE, "adc a,d8", Adc(Source.Immediate 0x01uy) |> Arithmetic
       0xCF, "rst 08h", Rst 0x08uy |> Control
       0xD0, "ret nc", RetCond Condition.NoCarry |> Control
       0xD1, "pop de", Pop DE |> Load
       0xD2, "jp nc,a16", JpCond(Condition.NoCarry, 0x0101us) |> Control
       0xD4, "call nc,a16", CallCond(Condition.NoCarry, 0x0101us) |> Control
       0xD5, "push de", Push DE |> Load
       0xD6, "sub d8", Sub(Source.Immediate 0x01uy) |> Arithmetic
       0xD7, "rst 10h", Rst 0x10uy |> Control
       0xD8, "ret c", RetCond Condition.Carry |> Control
       0xD9, "reti", Reti |> Control
       0xDA, "jp c,a16", JpCond(Condition.Carry, 0x0101us) |> Control
       0xDC, "call c,a16", CallCond(Condition.Carry, 0x0101us) |> Control
       0xDE, "sbc a,d8", Sbc(Source.Immediate 0x01uy) |> Arithmetic
       0xDF, "rst 18h", Rst 0x18uy |> Control
       0xE0, "ldh (a8),a", Ldh(To, AtByteHigh 0x01uy) |> Load
       0xE1, "pop hl", Pop HL |> Load
       0xE2, "ld (c),a", Ldh(To, AtCHigh) |> Load
       0xE5, "push hl", Push HL |> Load
       0xE6, "and d8", And(Source.Immediate 0x01uy) |> Logic
       0xE7, "rst 20h", Rst 0x20uy |> Control
       0xE8, "add sp,r8", AddSPe 0x01y |> Arithmetic
       0xE9, "jp hl", JpHL |> Control
       0xEA, "ld (a16),a", LdA(To, AtWord 0x0101us) |> Load
       0xEE, "xor d8", Xor(Source.Immediate 0x01uy) |> Logic
       0xEF, "rst 28h", Rst 0x28uy |> Control
       0xF0, "ldh a,(a8)", Ldh(From, AtByteHigh 0x01uy) |> Load
       0xF1, "pop af", Pop AF |> Load
       0xF2, "ld a,(c)", Ldh(From, AtCHigh) |> Load
       0xF3, "di", Di
       0xF5, "push af", Push AF |> Load
       0xF6, "or d8", Or(Source.Immediate 0x01uy) |> Logic
       0xF7, "rst 30h", Rst 0x30uy |> Control
       0xF8, "ld hl,sp+r8", LdHLFromSPe 0x01y |> Load
       0xF9, "ld sp,hl", LdSPFromHL |> Load
       0xFA, "ld a,(a16)", LdA(From, AtWord 0x0101us) |> Load
       0xFB, "ei", Ei
       0xFE, "cp d8", Cp(Source.Immediate 0x01uy) |> Arithmetic
       0xFF, "rst 38h", Rst 0x38uy |> Control |]
    |> Array.map (fun (opcode, label, instr) -> TestCaseData(uint8 opcode, instr).SetName $"Check 0x{opcode:X2} to {label} mapping")

[<Test>]
[<TestCaseSource(nameof instructionMappingCases)>]
let ``Check opcode to instruction mapping`` (opcode: int, expectedInstr: Instruction) =
    let memory = createTestMemory [| uint8 opcode; 0x01uy; 0x01uy |]

    let instr = fetchAndDecode memory 0us

    Assert.That(instr.Instruction, Is.EqualTo expectedInstr)

let bitwiseOrder =
    [| "b", Target.RegDirect B
       "c", Target.RegDirect C
       "d", Target.RegDirect D
       "e", Target.RegDirect E
       "h", Target.RegDirect H
       "l", Target.RegDirect L
       "[hl]", Target.HLIndirect
       "a", Target.RegDirect A |]
    |> Array.mapi (fun i (x, y) -> i, x, y)

let twoByteRotateShiftInstructions =
    [| "rlc", Rlc
       "rrc", Rrc
       "rl", Rl
       "rr", Rr
       "sla", Sla
       "sra", Sra
       "swap", Swap
       "srl", Srl |]
    |> Array.mapi (fun i (x, y) -> i * 8, x, y)

// e.g. rlc b
let rotateSwapCases =
    Array.allPairs twoByteRotateShiftInstructions bitwiseOrder
    |> Array.map (fun ((i1, desc, instr), (i2, reg, target)) -> i1 + i2, $"{desc} {reg}", Bitwise(instr target))

// e.g. bit 0, b
let indexedCases =
    Array.allPairs [| "bit", Bit; "res", Res; "set", Set |] [| 0uy .. 7uy |]
    |> Array.mapi (fun i ((desc, instr), x) -> 0x40 + (i * 8), $"{x} {desc}", x, instr)

let remainingTwoByteTestCases =
    Array.allPairs indexedCases bitwiseOrder
    |> Array.map (fun ((i1, desc, bit, instr), (i2, reg, target)) -> i1 + i2, $"{desc},{reg}", Bitwise(instr (bit, target)))

let twoByteTestCases =
    Array.append rotateSwapCases remainingTwoByteTestCases
    |> Array.map (fun (opcode, label, instr) -> TestCaseData(uint8 opcode, instr).SetName $"Check 0xCB{opcode:X2} to {label} mapping")

[<Test>]
[<TestCaseSource(nameof twoByteTestCases)>]
let ``Check two-byte opcode to instruction mapping`` (opcode: int, expectedInstr: Instruction) =
    let memory = createTestMemory [| 0xCBuy; uint8 opcode |]

    let instr = fetchAndDecode memory 0us

    Assert.That(instr.Instruction, Is.EqualTo expectedInstr)
