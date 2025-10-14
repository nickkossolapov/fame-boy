module FameBoy.Cpu.Opcodes

open FameBoy.Cpu.Instructions

module private LengthsAndCycles =
    let forBit =
        function
        | TestBit _ -> 2, Fixed 2

    let forControl =
        function
        | JumpRelativeConditional _ -> 2, Conditional { Met = 3; NotMet = 2 }

    let forLoad =
        function
        | ToReg16 _ -> 3, Fixed 3
        | ToReg8 _ -> 2, Fixed 2
        | StoreAToHLDecrement -> 1, Fixed 2

    let forLogic =
        function
        | Xor8 _ -> 1, Fixed 1

let private withLengthAndCycles (instr: Instruction) =
    let length, cycles =
        match instr with
        | Bit bitInstr -> LengthsAndCycles.forBit bitInstr
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

let private fetchAndDecode2Byte (memory: uint8 array) (pc: int) =
    let opcode = int memory[pc + 1]

    match opcode with
    | 0x7C -> TestBit (7uy, H) |> Bit
    | _ -> Unknown TwoByte

let fetchAndDecode (memory: uint8 array) (pc: int) : DecodedInstruction =
    let opcode = int memory[pc]

    let withUint8 () = memory[pc + 1]
    let withInt8 () = int8 memory[pc + 1]

    let withUint16 () =
        ((uint16 memory[pc + 1]) <<< 8) + uint16 memory[pc + 2]

    match opcode with
    | 0x0E -> ToReg8 (C, withUint8 ()) |> Load
    | 0x20 -> JumpRelativeConditional (Condition.NotZero, withInt8 ()) |> Control
    | 0x21 -> ToReg16 (HL, withUint16 ()) |> Load
    | 0x31 -> ToReg16 (SP, withUint16 ()) |> Load
    | 0x32 -> StoreAToHLDecrement |> Load
    | 0x3E -> ToReg8 (A, withUint8 ()) |> Load
    | 0xAF -> Xor8 A |> Logic
    | 0xCB -> fetchAndDecode2Byte memory pc
    | _ -> Unknown OneByte
    |> withLengthAndCycles
