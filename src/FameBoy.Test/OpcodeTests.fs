module FameBoy.Test.OpcodeTests

open FameBoy.Cpu.Execute
open FameBoy.Cpu.Instructions
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
    [| 0x01, "ld bc,d16", LdReg16FromWord (BC, 0x0101us) |> Load
       0x02, "ld (bc),a", LdAtBCFromA |> Load
       0x06, "ld b,d8", LdReg8FromByte (B, 0x01uy) |> Load
       0x08, "ld (a16),sp", LdAtWordFromSP 0x0101us |> Load
       0x0A, "ld a,(bc)", LdAFromAtBC |> Load
       0x0E, "ld c,d8", LdReg8FromByte (C, 0x01uy) |> Load
       0x11, "ld de,d16", LdReg16FromWord (DE, 0x0101us) |> Load
       0x12, "ld (de),a", LdAtDEFromA |> Load
       0x16, "ld d,d8", LdReg8FromByte (D, 0x01uy) |> Load
       0x1A, "ld a,(de)", LdAFromAtDE |> Load
       0x1E, "ld e,d8", LdReg8FromByte (E, 0x01uy) |> Load
       0x21, "ld hl,d16", LdReg16FromWord (HL, 0x0101us) |> Load
       0x22, "ld (hl+),a", LdAtHLIncFromA |> Load
       0x26, "ld h,d8", LdReg8FromByte (H, 0x01uy) |> Load
       0x2A, "ld a,(hl+)", LdAFromAtHLInc |> Load
       0x2E, "ld l,d8", LdReg8FromByte (L, 0x01uy) |> Load
       0x31, "ld sp,d16", LdReg16FromWord (SP, 0x0101us) |> Load
       0x32, "ld (hl-),a", LdAtHLDecFromA |> Load
       0x36, "ld (hl),d8", LdAtHLFromByte 0x01uy |> Load
       0x3A, "ld a,(hl-)", LdAFromAtHLDec |> Load
       0x3E, "ld a,d8", LdReg8FromByte (A, 0x01uy) |> Load
       0x40, "ld b,b", LdReg8FromReg8 (B, B) |> Load
       0x41, "ld b,c", LdReg8FromReg8 (B, C) |> Load
       0x42, "ld b,d", LdReg8FromReg8 (B, D) |> Load
       0x43, "ld b,e", LdReg8FromReg8 (B, E) |> Load
       0x44, "ld b,h", LdReg8FromReg8 (B, H) |> Load
       0x45, "ld b,l", LdReg8FromReg8 (B, L) |> Load
       0x46, "ld b,(hl)", LdReg8FromAtHL B |> Load
       0x47, "ld b,a", LdReg8FromReg8 (B, A) |> Load
       0x48, "ld c,b", LdReg8FromReg8 (C, B) |> Load
       0x49, "ld c,c", LdReg8FromReg8 (C, C) |> Load
       0x4A, "ld c,d", LdReg8FromReg8 (C, D) |> Load
       0x4B, "ld c,e", LdReg8FromReg8 (C, E) |> Load
       0x4C, "ld c,h", LdReg8FromReg8 (C, H) |> Load
       0x4D, "ld c,l", LdReg8FromReg8 (C, L) |> Load
       0x4E, "ld c,(hl)", LdReg8FromAtHL C |> Load
       0x4F, "ld c,a", LdReg8FromReg8 (C, A) |> Load
       0x50, "ld d,b", LdReg8FromReg8 (D, B) |> Load
       0x51, "ld d,c", LdReg8FromReg8 (D, C) |> Load
       0x52, "ld d,d", LdReg8FromReg8 (D, D) |> Load
       0x53, "ld d,e", LdReg8FromReg8 (D, E) |> Load
       0x54, "ld d,h", LdReg8FromReg8 (D, H) |> Load
       0x55, "ld d,l", LdReg8FromReg8 (D, L) |> Load
       0x56, "ld d,(hl)", LdReg8FromAtHL D |> Load
       0x57, "ld d,a", LdReg8FromReg8 (D, A) |> Load
       0x58, "ld e,b", LdReg8FromReg8 (E, B) |> Load
       0x59, "ld e,c", LdReg8FromReg8 (E, C) |> Load
       0x5A, "ld e,d", LdReg8FromReg8 (E, D) |> Load
       0x5B, "ld e,e", LdReg8FromReg8 (E, E) |> Load
       0x5C, "ld e,h", LdReg8FromReg8 (E, H) |> Load
       0x5D, "ld e,l", LdReg8FromReg8 (E, L) |> Load
       0x5E, "ld e,(hl)", LdReg8FromAtHL E |> Load
       0x5F, "ld e,a", LdReg8FromReg8 (E, A) |> Load
       0x60, "ld h,b", LdReg8FromReg8 (H, B) |> Load
       0x61, "ld h,c", LdReg8FromReg8 (H, C) |> Load
       0x62, "ld h,d", LdReg8FromReg8 (H, D) |> Load
       0x63, "ld h,e", LdReg8FromReg8 (H, E) |> Load
       0x64, "ld h,h", LdReg8FromReg8 (H, H) |> Load
       0x65, "ld h,l", LdReg8FromReg8 (H, L) |> Load
       0x66, "ld h,(hl)", LdReg8FromAtHL H |> Load
       0x67, "ld h,a", LdReg8FromReg8 (H, A) |> Load
       0x68, "ld l,b", LdReg8FromReg8 (L, B) |> Load
       0x69, "ld l,c", LdReg8FromReg8 (L, C) |> Load
       0x6A, "ld l,d", LdReg8FromReg8 (L, D) |> Load
       0x6B, "ld l,e", LdReg8FromReg8 (L, E) |> Load
       0x6C, "ld l,h", LdReg8FromReg8 (L, H) |> Load
       0x6D, "ld l,l", LdReg8FromReg8 (L, L) |> Load
       0x6E, "ld l,(hl)", LdReg8FromAtHL L |> Load
       0x6F, "ld l,a", LdReg8FromReg8 (L, A) |> Load
       0x70, "ld (hl),b", LdAtHLFromReg8 B |> Load
       0x71, "ld (hl),c", LdAtHLFromReg8 C |> Load
       0x72, "ld (hl),d", LdAtHLFromReg8 D |> Load
       0x73, "ld (hl),e", LdAtHLFromReg8 E |> Load
       0x74, "ld (hl),h", LdAtHLFromReg8 H |> Load
       0x75, "ld (hl),l", LdAtHLFromReg8 L |> Load
       0x77, "ld (hl),a", LdAtHLFromReg8 A |> Load
       0x78, "ld a,b", LdReg8FromReg8 (A, B) |> Load
       0x79, "ld a,c", LdReg8FromReg8 (A, C) |> Load
       0x7A, "ld a,d", LdReg8FromReg8 (A, D) |> Load
       0x7B, "ld a,e", LdReg8FromReg8 (A, E) |> Load
       0x7C, "ld a,h", LdReg8FromReg8 (A, H) |> Load
       0x7D, "ld a,l", LdReg8FromReg8 (A, L) |> Load
       0x7E, "ld a,(hl)", LdReg8FromAtHL A |> Load
       0x7F, "ld a,a", LdReg8FromReg8 (A, A) |> Load
       0xC1, "pop bc", Pop BC |> Load
       0xC5, "push bc", Push BC |> Load
       0xD1, "pop de", Pop DE |> Load
       0xD5, "push de", Push DE |> Load
       0xE0, "ldh (a8),a", LdhAtByteFromA 0x01uy |> Load
       0xE1, "pop hl", Pop HL |> Load
       0xE2, "ld (c),a", LdhAtCFromA |> Load
       0xE5, "push hl", Push HL |> Load
       0xEA, "ld (a16),a", LdAtWordFromA 0x0101us |> Load
       0xF0, "ldh a,(a8)", LdhAFromAByte 0x01uy |> Load
       0xF1, "pop af", Pop AF |> Load
       0xF2, "ld a,(c)", LdhAFromAtC |> Load
       0xF5, "push af", Push AF |> Load
       0xF8, "ld hl,sp+r8", LdHLFromSPe 0x01y |> Load
       0xF9, "ld sp,hl", LdSPFromHL |> Load
       0xFA, "ld a,(a16)", LdAFromAtWord 0x0101us |> Load |]
    |> Array.map (fun (opcode, label, instr) -> TestCaseData(uint8 opcode, instr).SetName $"Check 0x{opcode:X2} to {label} mapping")

[<Test>]
[<TestCaseSource(nameof instructionMappingCases)>]
let ``Check opcode to instruction mapping`` (opcode: int, expectedInstr: Instruction) =
    let memory = Memory [| uint8 opcode; 0x01uy; 0x01uy |]

    let instr = fetchAndDecode memory 0us

    Assert.That (instr.Instruction, Is.EqualTo expectedInstr)
