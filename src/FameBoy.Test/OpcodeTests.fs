module FameBoy.Test.OpcodeTests

open FameBoy.Cpu.Execute
open FameBoy.Cpu.Instructions
open FameBoy.Cpu.Instructions.LoadTypes
open FameBoy.Cpu.Opcodes
open FameBoy.Cpu.State
open NUnit.Framework

[<Test>]
let ``Check little endian order for 3-byte instruction - ld hl,n16 (L = PC+1, H = PC+2)`` () =
    let opcode = 0x21uy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100us
    cpu.Memory[0x100us] <- opcode
    cpu.Memory[0x101us] <- 0x34uy
    cpu.Memory[0x102us] <- 0x12uy

    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    Assert.That (cpu.Registers.H, Is.EqualTo 0x12uy)
    Assert.That (cpu.Registers.L, Is.EqualTo 0x34uy)
    Assert.That (cpu.Registers.HL, Is.EqualTo 0x1234us)



let instructionMappingCases =
    [| 0x00, "nop", Nop
       0x01, "ld bc,d16", LdReg16FromWord (BC, 0x0101us) |> Load
       0x02, "ld (bc),a", LdAtBCFromA |> Load
       0x03, "inc bc", IncReg16 BC |> Arithmetic
       0x04, "inc b", IncReg8 B |> Arithmetic
       0x05, "dec b", DecReg8 B |> Arithmetic
       0x06, "ld b,d8", LdReg8 (B, Immediate 0x01uy) |> Load
       0x08, "ld (a16),sp", LdAtWordFromSP 0x0101us |> Load
       0x09, "add hl,bc", AddHL BC |> Arithmetic
       0x0A, "ld a,(bc)", LdAFromAtBC |> Load
       0x0B, "dec bc", DecReg16 BC |> Arithmetic
       0x0C, "inc c", IncReg8 C |> Arithmetic
       0x0D, "dec c", DecReg8 C |> Arithmetic
       0x0E, "ld c,d8", LdReg8 (C, Immediate 0x01uy) |> Load
       0x11, "ld de,d16", LdReg16FromWord (DE, 0x0101us) |> Load
       0x12, "ld (de),a", LdAtDEFromA |> Load
       0x13, "inc de", IncReg16 DE |> Arithmetic
       0x14, "inc d", IncReg8 D |> Arithmetic
       0x15, "dec d", DecReg8 D |> Arithmetic
       0x16, "ld d,d8", LdReg8 (D, Immediate 0x01uy) |> Load
       0x18, "jr r8", Jr 0x01y |> Control
       0x19, "add hl,de", AddHL DE |> Arithmetic
       0x1A, "ld a,(de)", LdAFromAtDE |> Load
       0x1B, "dec de", DecReg16 DE |> Arithmetic
       0x1C, "inc e", IncReg8 E |> Arithmetic
       0x1D, "dec e", DecReg8 E |> Arithmetic
       0x1E, "ld e,d8", LdReg8 (E, Immediate 0x01uy) |> Load
       0x20, "jr nz,r8", JrCond (Condition.NotZero, 0x01y) |> Control
       0x21, "ld hl,d16", LdReg16FromWord (HL, 0x0101us) |> Load
       0x22, "ld (hl+),a", LdAtHLIncFromA |> Load
       0x23, "inc hl", IncReg16 HL |> Arithmetic
       0x24, "inc h", IncReg8 H |> Arithmetic
       0x25, "dec h", DecReg8 H |> Arithmetic
       0x26, "ld h,d8", LdReg8 (H, Immediate 0x01uy) |> Load
       0x27, "daa", Daa |> Logic
       0x28, "jr z,r8", JrCond (Condition.Zero, 0x01y) |> Control
       0x29, "add hl,hl", AddHL HL |> Arithmetic
       0x2A, "ld a,(hl+)", LdAFromAtHLInc |> Load
       0x2B, "dec hl", DecReg16 HL |> Arithmetic
       0x2C, "inc l", IncReg8 L |> Arithmetic
       0x2D, "dec l", DecReg8 L |> Arithmetic
       0x2E, "ld l,d8", LdReg8 (L, Immediate 0x01uy) |> Load
       0x2F, "cpl", Cpl |> Logic
       0x30, "jr nc,r8", JrCond (Condition.NoCarry, 0x01y) |> Control
       0x31, "ld sp,d16", LdReg16FromWord (SP, 0x0101us) |> Load
       0x32, "ld (hl-),a", LdAtHLDecFromA |> Load
       0x33, "inc sp", IncReg16 SP |> Arithmetic
       0x34, "inc (hl)", IncAtHL |> Arithmetic
       0x35, "dec (hl)", DecAtHL |> Arithmetic
       0x36, "ld (hl),d8", LdAtHLFromByte 0x01uy |> Load
       0x37, "scf", Scf |> Logic
       0x38, "jr c,r8", JrCond (Condition.Carry, 0x01y) |> Control
       0x39, "add hl,sp", AddHL SP |> Arithmetic
       0x3A, "ld a,(hl-)", LdAFromAtHLDec |> Load
       0x3B, "dec sp", DecReg16 SP |> Arithmetic
       0x3C, "inc a", IncReg8 A |> Arithmetic
       0x3D, "dec a", DecReg8 A |> Arithmetic
       0x3E, "ld a,d8", LdReg8 (A, Immediate 0x01uy) |> Load
       0x3F, "ccf", Ccf |> Logic
       0x40, "ld b,b", LdReg8 (B, RegDirect B) |> Load
       0x41, "ld b,c", LdReg8 (B, RegDirect C) |> Load
       0x42, "ld b,d", LdReg8 (B, RegDirect D) |> Load
       0x43, "ld b,e", LdReg8 (B, RegDirect E) |> Load
       0x44, "ld b,h", LdReg8 (B, RegDirect H) |> Load
       0x45, "ld b,l", LdReg8 (B, RegDirect L) |> Load
       0x46, "ld b,(hl)", LdReg8 (B, HLIndirect) |> Load
       0x47, "ld b,a", LdReg8 (B, RegDirect A) |> Load
       0x48, "ld c,b", LdReg8 (C, RegDirect B) |> Load
       0x49, "ld c,c", LdReg8 (C, RegDirect C) |> Load
       0x4A, "ld c,d", LdReg8 (C, RegDirect D) |> Load
       0x4B, "ld c,e", LdReg8 (C, RegDirect E) |> Load
       0x4C, "ld c,h", LdReg8 (C, RegDirect H) |> Load
       0x4D, "ld c,l", LdReg8 (C, RegDirect L) |> Load
       0x4E, "ld c,(hl)", LdReg8 (C, HLIndirect) |> Load
       0x4F, "ld c,a", LdReg8 (C, RegDirect A) |> Load
       0x50, "ld d,b", LdReg8 (D, RegDirect B) |> Load
       0x51, "ld d,c", LdReg8 (D, RegDirect C) |> Load
       0x52, "ld d,d", LdReg8 (D, RegDirect D) |> Load
       0x53, "ld d,e", LdReg8 (D, RegDirect E) |> Load
       0x54, "ld d,h", LdReg8 (D, RegDirect H) |> Load
       0x55, "ld d,l", LdReg8 (D, RegDirect L) |> Load
       0x56, "ld d,(hl)", LdReg8 (D, HLIndirect) |> Load
       0x57, "ld d,a", LdReg8 (D, RegDirect A) |> Load
       0x58, "ld e,b", LdReg8 (E, RegDirect B) |> Load
       0x59, "ld e,c", LdReg8 (E, RegDirect C) |> Load
       0x5A, "ld e,d", LdReg8 (E, RegDirect D) |> Load
       0x5B, "ld e,e", LdReg8 (E, RegDirect E) |> Load
       0x5C, "ld e,h", LdReg8 (E, RegDirect H) |> Load
       0x5D, "ld e,l", LdReg8 (E, RegDirect L) |> Load
       0x5E, "ld e,(hl)", LdReg8 (E, HLIndirect) |> Load
       0x5F, "ld e,a", LdReg8 (E, RegDirect A) |> Load
       0x60, "ld h,b", LdReg8 (H, RegDirect B) |> Load
       0x61, "ld h,c", LdReg8 (H, RegDirect C) |> Load
       0x62, "ld h,d", LdReg8 (H, RegDirect D) |> Load
       0x63, "ld h,e", LdReg8 (H, RegDirect E) |> Load
       0x64, "ld h,h", LdReg8 (H, RegDirect H) |> Load
       0x65, "ld h,l", LdReg8 (H, RegDirect L) |> Load
       0x66, "ld h,(hl)", LdReg8 (H, HLIndirect) |> Load
       0x67, "ld h,a", LdReg8 (H, RegDirect A) |> Load
       0x68, "ld l,b", LdReg8 (L, RegDirect B) |> Load
       0x69, "ld l,c", LdReg8 (L, RegDirect C) |> Load
       0x6A, "ld l,d", LdReg8 (L, RegDirect D) |> Load
       0x6B, "ld l,e", LdReg8 (L, RegDirect E) |> Load
       0x6C, "ld l,h", LdReg8 (L, RegDirect H) |> Load
       0x6D, "ld l,l", LdReg8 (L, RegDirect L) |> Load
       0x6E, "ld l,(hl)", LdReg8 (L, HLIndirect) |> Load
       0x6F, "ld l,a", LdReg8 (L, RegDirect A) |> Load
       0x70, "ld (hl),b", LdAtHLFromReg8 B |> Load
       0x71, "ld (hl),c", LdAtHLFromReg8 C |> Load
       0x72, "ld (hl),d", LdAtHLFromReg8 D |> Load
       0x73, "ld (hl),e", LdAtHLFromReg8 E |> Load
       0x74, "ld (hl),h", LdAtHLFromReg8 H |> Load
       0x75, "ld (hl),l", LdAtHLFromReg8 L |> Load
       0x77, "ld (hl),a", LdAtHLFromReg8 A |> Load
       0x78, "ld a,b", LdReg8 (A, RegDirect B) |> Load
       0x79, "ld a,c", LdReg8 (A, RegDirect C) |> Load
       0x7A, "ld a,d", LdReg8 (A, RegDirect D) |> Load
       0x7B, "ld a,e", LdReg8 (A, RegDirect E) |> Load
       0x7C, "ld a,h", LdReg8 (A, RegDirect H) |> Load
       0x7D, "ld a,l", LdReg8 (A, RegDirect L) |> Load
       0x7E, "ld a,(hl)", LdReg8 (A, HLIndirect) |> Load
       0x7F, "ld a,a", LdReg8 (A, RegDirect A) |> Load
       0x80, "add a,b", Add (RegDirect B) |> Arithmetic
       0x81, "add a,c", Add (RegDirect C) |> Arithmetic
       0x82, "add a,d", Add (RegDirect D) |> Arithmetic
       0x83, "add a,e", Add (RegDirect E) |> Arithmetic
       0x84, "add a,h", Add (RegDirect H) |> Arithmetic
       0x85, "add a,l", Add (RegDirect L) |> Arithmetic
       0x86, "add a,(hl)", Add HLIndirect |> Arithmetic
       0x87, "add a,a", Add (RegDirect A) |> Arithmetic
       0x88, "adc a,b", Adc (RegDirect B) |> Arithmetic
       0x89, "adc a,c", Adc (RegDirect C) |> Arithmetic
       0x8A, "adc a,d", Adc (RegDirect D) |> Arithmetic
       0x8B, "adc a,e", Adc (RegDirect E) |> Arithmetic
       0x8C, "adc a,h", Adc (RegDirect H) |> Arithmetic
       0x8D, "adc a,l", Adc (RegDirect L) |> Arithmetic
       0x8E, "adc a,(hl)", Adc HLIndirect |> Arithmetic
       0x8F, "adc a,a", Adc (RegDirect A) |> Arithmetic
       0x90, "sub b", Sub (RegDirect B) |> Arithmetic
       0x91, "sub c", Sub (RegDirect C) |> Arithmetic
       0x92, "sub d", Sub (RegDirect D) |> Arithmetic
       0x93, "sub e", Sub (RegDirect E) |> Arithmetic
       0x94, "sub h", Sub (RegDirect H) |> Arithmetic
       0x95, "sub l", Sub (RegDirect L) |> Arithmetic
       0x96, "sub (hl)", Sub HLIndirect |> Arithmetic
       0x97, "sub a", Sub (RegDirect A) |> Arithmetic
       0x98, "sbc a,b", Sbc (RegDirect B) |> Arithmetic
       0x99, "sbc a,c", Sbc (RegDirect C) |> Arithmetic
       0x9A, "sbc a,d", Sbc (RegDirect D) |> Arithmetic
       0x9B, "sbc a,e", Sbc (RegDirect E) |> Arithmetic
       0x9C, "sbc a,h", Sbc (RegDirect H) |> Arithmetic
       0x9D, "sbc a,l", Sbc (RegDirect L) |> Arithmetic
       0x9E, "sbc a,(hl)", Sbc HLIndirect |> Arithmetic
       0x9F, "sbc a,a", Sbc (RegDirect A) |> Arithmetic
       0xA0, "and b", And (RegDirect B) |> Logic
       0xA1, "and c", And (RegDirect C) |> Logic
       0xA2, "and d", And (RegDirect D) |> Logic
       0xA3, "and e", And (RegDirect E) |> Logic
       0xA4, "and h", And (RegDirect H) |> Logic
       0xA5, "and l", And (RegDirect L) |> Logic
       0xA6, "and (hl)", And HLIndirect |> Logic
       0xA7, "and a", And (RegDirect A) |> Logic
       0xA8, "xor b", Xor (RegDirect B) |> Logic
       0xA9, "xor c", Xor (RegDirect C) |> Logic
       0xAA, "xor d", Xor (RegDirect D) |> Logic
       0xAB, "xor e", Xor (RegDirect E) |> Logic
       0xAC, "xor h", Xor (RegDirect H) |> Logic
       0xAD, "xor l", Xor (RegDirect L) |> Logic
       0xAE, "xor (hl)", Xor HLIndirect |> Logic
       0xAF, "xor a", Xor (RegDirect A) |> Logic
       0xB0, "or b", Or (RegDirect B) |> Logic
       0xB1, "or c", Or (RegDirect C) |> Logic
       0xB2, "or d", Or (RegDirect D) |> Logic
       0xB3, "or e", Or (RegDirect E) |> Logic
       0xB4, "or h", Or (RegDirect H) |> Logic
       0xB5, "or l", Or (RegDirect L) |> Logic
       0xB6, "or (hl)", Or HLIndirect |> Logic
       0xB7, "or a", Or (RegDirect A) |> Logic
       0xB8, "cp b", Cp (RegDirect B) |> Arithmetic
       0xB9, "cp c", Cp (RegDirect C) |> Arithmetic
       0xBA, "cp d", Cp (RegDirect D) |> Arithmetic
       0xBB, "cp e", Cp (RegDirect E) |> Arithmetic
       0xBC, "cp h", Cp (RegDirect H) |> Arithmetic
       0xBD, "cp l", Cp (RegDirect L) |> Arithmetic
       0xBE, "cp (hl)", Cp HLIndirect |> Arithmetic
       0xBF, "cp a", Cp (RegDirect A) |> Arithmetic
       0xC0, "ret nz", RetCond Condition.NotZero |> Control
       0xC1, "pop bc", Pop BC |> Load
       0xC2, "jp nz,a16", JpCond (Condition.NotZero, 0x0101us) |> Control
       0xC3, "jp a16", Jp 0x0101us |> Control
       0xC4, "call nz,a16", CallCond (Condition.NotZero, 0x0101us) |> Control
       0xC5, "push bc", Push BC |> Load
       0xC6, "add a,d8", Add (Immediate 0x01uy) |> Arithmetic
       0xC7, "rst 00h", Rst 0x00uy |> Control
       0xC8, "ret z", RetCond Condition.Zero |> Control
       0xC9, "ret", Ret |> Control
       0xCA, "jp z,a16", JpCond (Condition.Zero, 0x0101us) |> Control
       0xCC, "call z,a16", CallCond (Condition.Zero, 0x0101us) |> Control
       0xCD, "call a16", Call 0x0101us |> Control
       0xCE, "adc a,d8", Adc (Immediate 0x01uy) |> Arithmetic
       0xCF, "rst 08h", Rst 0x08uy |> Control
       0xD0, "ret nc", RetCond Condition.NoCarry |> Control
       0xD1, "pop de", Pop DE |> Load
       0xD2, "jp nc,a16", JpCond (Condition.NoCarry, 0x0101us) |> Control
       0xD4, "call nc,a16", CallCond (Condition.NoCarry, 0x0101us) |> Control
       0xD5, "push de", Push DE |> Load
       0xD6, "sub d8", Sub (Immediate 0x01uy) |> Arithmetic
       0xD7, "rst 10h", Rst 0x10uy |> Control
       0xD8, "ret c", RetCond Condition.Carry |> Control
       0xD9, "reti", Reti |> Control
       0xDA, "jp c,a16", JpCond (Condition.Carry, 0x0101us) |> Control
       0xDC, "call c,a16", CallCond (Condition.Carry, 0x0101us) |> Control
       0xDE, "sbc a,d8", Sbc (Immediate 0x01uy) |> Arithmetic
       0xDF, "rst 18h", Rst 0x18uy |> Control
       0xE0, "ldh (a8),a", LdhAtByteFromA 0x01uy |> Load
       0xE1, "pop hl", Pop HL |> Load
       0xE2, "ld (c),a", LdhAtCFromA |> Load
       0xE5, "push hl", Push HL |> Load
       0xE6, "and d8", And (Immediate 0x01uy) |> Logic
       0xE7, "rst 20h", Rst 0x20uy |> Control
       0xE8, "add sp,r8", AddSPe 0x01y |> Arithmetic
       0xE9, "jp hl", JpHL |> Control
       0xEA, "ld (a16),a", LdAtWordFromA 0x0101us |> Load
       0xEE, "xor d8", Xor (Immediate 0x01uy) |> Logic
       0xEF, "rst 28h", Rst 0x28uy |> Control
       0xF0, "ldh a,(a8)", LdhAFromAByte 0x01uy |> Load
       0xF1, "pop af", Pop AF |> Load
       0xF2, "ld a,(c)", LdhAFromAtC |> Load
       0xF5, "push af", Push AF |> Load
       0xF6, "or d8", Or (Immediate 0x01uy) |> Logic
       0xF7, "rst 30h", Rst 0x30uy |> Control
       0xF8, "ld hl,sp+r8", LdHLFromSPe 0x01y |> Load
       0xF9, "ld sp,hl", LdSPFromHL |> Load
       0xFA, "ld a,(a16)", LdAFromAtWord 0x0101us |> Load
       0xFE, "cp d8", Cp (Immediate 0x01uy) |> Arithmetic
       0xFF, "rst 38h", Rst 0x38uy |> Control |]
    |> Array.map (fun (opcode, label, instr) -> TestCaseData(uint8 opcode, instr).SetName $"Check 0x{opcode:X2} to {label} mapping")

[<Test>]
[<TestCaseSource(nameof instructionMappingCases)>]
let ``Check opcode to instruction mapping`` (opcode: int, expectedInstr: Instruction) =
    let memory = Memory [| uint8 opcode; 0x01uy; 0x01uy |]

    let instr = fetchAndDecode memory 0us

    Assert.That (instr.Instruction, Is.EqualTo expectedInstr)
