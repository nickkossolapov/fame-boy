module FameBoy.Cpu.Opcodes

open FameBoy.Cpu.Instructions

module private Create =
    let load16Instr (memory: uint8 array) (pc: int) (reg: Reg16) =
        let word = ((uint16 memory[pc + 1]) <<< 8) + uint16 memory[pc + 2]

        { Instruction = Load (Reg16Word (reg, word))
          Length = 3
          MCycles = Fixed 3 }

    let testBit value reg =
        { Instruction = Bit (TestBit (value, reg))
          Length = 2
          MCycles = Fixed 2 }

let private fetchAndDecode2Byte (memory: uint8 array) (pc: int) : DecodedInstruction =
    let opcode = int memory[pc + 1]

    match opcode with
    | 0x7C -> Create.testBit 7uy H
    | _ ->
        { Instruction = Unknown
          Length = 2
          MCycles = Fixed 1 }

let fetchAndDecode (memory: uint8 array) (pc: int) : DecodedInstruction =
    let opcode = int memory[pc]

    match opcode with
    | 0x21 -> Create.load16Instr memory pc HL
    | 0x31 -> Create.load16Instr memory pc SP
    | 0x32 ->
        { Instruction = Load HLToADecrement
          Length = 1
          MCycles = Fixed 2 }
    | 0xAF ->
        { Instruction = Logic (Xor8 A)
          Length = 1
          MCycles = Fixed 1 }
    | 0xCB -> fetchAndDecode2Byte memory pc
    | _ ->
        { Instruction = Unknown
          Length = 1
          MCycles = Fixed 1 }
