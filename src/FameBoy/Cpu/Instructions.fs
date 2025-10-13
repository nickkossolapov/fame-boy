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
    | BC
    | DE
    | HL
    | PC
    | SP

type Condition =
    | Zero
    | NotZero
    | Carry
    | NoCarry

type BitInstr = TestBit of uint3 * Reg8

type ControlInstr = JumpRelativeConditional of Condition * int8

type LoadInstr =
    | ToReg8 of Reg8 * uint8
    | ToReg16 of Reg16 * uint16
    | StoreAToHLDecrement

type LogicInstr = Xor8 of Reg8

type Instruction =
    | Bit of BitInstr
    | Control of ControlInstr
    | Load of LoadInstr
    | Logic of LogicInstr
    | Unknown

type ConditionalCycle = { Met: int; NotMet: int }

type MCycles =
    | Fixed of int
    | Conditional of ConditionalCycle

type DecodedInstruction =
    { Instruction: Instruction
      Length: int
      MCycles: MCycles }
