module FameBoy.Cpu.Instructions

open FameBoy.Cpu.State

type uint3 = uint8 // todo maybe expand on this type for additional type safety

type Reg8 =
    | A
    | B
    | C
    | D
    | E
    | H
    | L
    | F

    member this.readFromCpu(cpu: Cpu) =
        match this with
        | A -> cpu.Registers.A
        | B -> cpu.Registers.B
        | C -> cpu.Registers.C
        | D -> cpu.Registers.D
        | E -> cpu.Registers.E
        | H -> cpu.Registers.H
        | L -> cpu.Registers.L
        | F -> cpu.Registers.F

type Reg16 =
    | AF
    | BC
    | DE
    | HL
    | SP

type Condition =
    | Zero
    | NotZero
    | Carry
    | NoCarry

type ArithmeticInstr =
    | AddReg

type BitwiseInstr = Bit of uint3 * Reg8

type ControlInstr = JrCond of Condition * int8

type LoadInstr =
    | LdRegFromByte of Reg8 * uint8
    | LdRegFromWord of Reg16 * uint16
    | LdAFromAtHLDec

type LogicInstr = Xor8 of Reg8

type UnknownInstr =
    | OneByte
    | TwoByte

type Instruction =
    | Bitwise of BitwiseInstr
    | Control of ControlInstr
    | Load of LoadInstr
    | Logic of LogicInstr
    | Unknown of UnknownInstr

type ConditionalCycle = { Met: int; NotMet: int }

type MCycles =
    | Fixed of int
    | Conditional of ConditionalCycle

type DecodedInstruction =
    { Instruction: Instruction
      Length: int
      MCycles: MCycles }
