module FameBoy.Cpu.Opcodes

open FameBoy.Cpu.Instructions
open FameBoy.Cpu.State

module private LengthsAndCycles =
    let forArithmetic =
        function
        | IncReg _ -> 1, Fixed 1

    let forBit =
        function
        | Bit _ -> 2, Fixed 2

    let forControl =
        function
        | JrCond _ -> 2, Conditional { Met = 3; NotMet = 2 }

    let forLoad =
        function
        | LdRegFromByte _ -> 2, Fixed 2
        | LdRegFromWord _ -> 3, Fixed 3
        | LdAtHLFromReg _ -> 1, Fixed 2
        | LdAFromAtHLDec -> 1, Fixed 2
        | LdhAtCFromA -> 1, Fixed 2

    let forLogic =
        function
        | Xor8 _ -> 1, Fixed 1

let private withLengthAndCycles (instr: Instruction) =
    let length, cycles =
        match instr with
        | Arithmetic arithmeticInstr -> LengthsAndCycles.forArithmetic arithmeticInstr
        | Bitwise bitInstr -> LengthsAndCycles.forBit bitInstr
        | Control controlInstr -> LengthsAndCycles.forControl controlInstr
        | Load loadInstr -> LengthsAndCycles.forLoad loadInstr
        | Logic logicInstr -> LengthsAndCycles.forLogic logicInstr
        | Unknown unknownInstr ->
            match unknownInstr with
            | OneByte -> 1, Fixed 1
            | TwoByte -> 2, Fixed 2

    { Instruction = instr
      Length = length
      MCycles = cycles }

let private fetchAndDecode2Byte (memory: Memory) (pc: uint16) =
    let opcode = int memory[pc + 1us]

    match opcode with
    | 0x7C -> Bit (7uy, H) |> Bitwise
    | _ -> Unknown TwoByte

let fetchAndDecode (memory: Memory) (pc: uint16) : DecodedInstruction =
    let opcode = int memory[pc]

    let withUint8 () = memory[pc + 1us]
    let withInt8 () = int8 memory[pc + 1us]

    let withUint16 () =
        ((uint16 memory[pc + 2us]) <<< 8) + uint16 memory[pc + 1us]

    match opcode with
    | 0x0C -> IncReg C |> Arithmetic
    | 0x0E -> LdRegFromByte (C, withUint8 ()) |> Load
    | 0x20 -> JrCond (Condition.NotZero, withInt8 ()) |> Control
    | 0x21 -> LdRegFromWord (HL, withUint16 ()) |> Load
    | 0x31 -> LdRegFromWord (SP, withUint16 ()) |> Load
    | 0x32 -> LdAFromAtHLDec |> Load
    | 0x3E -> LdRegFromByte (A, withUint8 ()) |> Load
    | 0x77 -> LdAtHLFromReg A |> Load
    | 0xAF -> Xor8 A |> Logic
    | 0xCB -> fetchAndDecode2Byte memory pc
    | 0xE2 -> LdhAtCFromA |> Load
    | _ -> Unknown OneByte
    |> withLengthAndCycles
