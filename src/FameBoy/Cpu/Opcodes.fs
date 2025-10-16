module FameBoy.Cpu.Opcodes

open FameBoy.Cpu.Instructions
open FameBoy.Cpu.State

let private fetchAndDecode2Byte (memory: Memory) (pc: uint16) =
    let opcode = int memory[pc + 1us]

    match opcode with
    | 0x7C -> Bit (7uy, H) |> Bitwise
    | _ -> failwith $"There are no unused 2-byte opcodes. {opcode:X2} is unimplemented."

let fetchAndDecode (memory: Memory) (pc: uint16) : DecodedInstruction =
    let opcode = int memory[pc]

    let withUint8 () = memory[pc + 1us]
    let withInt8 () = int8 memory[pc + 1us]

    let withUint16 () =
        ((uint16 memory[pc + 2us]) <<< 8) + uint16 memory[pc + 1us]

    match opcode with
    | 0x0C -> IncReg C |> Arithmetic
    | 0x0E -> LdRegFromByte (C, withUint8 ()) |> Load
    | 0x11 -> LdRegFromWord (DE, withUint16 ()) |> Load
    | 0x1A -> LdAFromAtDE |> Load
    | 0x20 -> JrCond (Condition.NotZero, withInt8 ()) |> Control
    | 0x21 -> LdRegFromWord (HL, withUint16 ()) |> Load
    | 0x31 -> LdRegFromWord (SP, withUint16 ()) |> Load
    | 0x32 -> LdAFromAtHLDec |> Load
    | 0x3E -> LdRegFromByte (A, withUint8 ()) |> Load
    | 0x77 -> LdAtHLFromReg A |> Load
    | 0xAF -> Xor8 A |> Logic
    | 0xCB -> fetchAndDecode2Byte memory pc
    | 0xE0 -> LdhAtByteFromA (withUint8 ()) |> Load
    | 0xE2 -> LdhAtCFromA |> Load
    | _ -> Unknown
    |> withLengthAndCycles
