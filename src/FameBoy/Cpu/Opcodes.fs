﻿module FameBoy.Cpu.Opcodes

open FameBoy.Cpu.Instructions

let private getWord (memory: uint8 array) (pc: int) =
    ((uint16 memory[pc + 1]) <<< 8) + uint16 memory[pc + 2]

let fetchAndDecode (memory: uint8 array) (pc: int) : DecodedInstruction =
    let opcode = int memory[pc]

    match opcode with
    | 0x31 ->
        let word = getWord memory pc

        { Instruction = Load (Reg16Word (SP, word))
          Length = 3
          MCycles = 3 }
    | _ ->
        { Instruction = Unknown
          Length = 1
          MCycles = 1 }
