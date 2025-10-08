module FameBoy.Cpu.Instructions

type Reg8 =
    | A
    | B
    | C
    | D
    | E
    | H
    | L
    | F

type Reg16 =
    | BC
    | DE
    | HL
    | PC
    | SP

type LoadInstr = Reg16Word of Reg16 * uint16

type Instruction =
    | Load of LoadInstr
    | Unknown

type DecodedInstruction =
    { Instruction: Instruction
      Length: int
      MCycles: int }
