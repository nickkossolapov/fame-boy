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

    member this.GetFromCpu(cpu: Cpu) =
        match this with
        | A -> cpu.Registers.A
        | B -> cpu.Registers.B
        | C -> cpu.Registers.C
        | D -> cpu.Registers.D
        | E -> cpu.Registers.E
        | H -> cpu.Registers.H
        | L -> cpu.Registers.L
        | F -> cpu.Registers.F

    member this.SetToCpu (cpu: Cpu) (value: uint8) =
        match this with
        | A -> cpu.Registers.A <- value
        | B -> cpu.Registers.B <- value
        | C -> cpu.Registers.C <- value
        | D -> cpu.Registers.D <- value
        | E -> cpu.Registers.E <- value
        | H -> cpu.Registers.H <- value
        | L -> cpu.Registers.L <- value
        | F -> cpu.Registers.F <- value

type Reg16 =
    | AF
    | BC
    | DE
    | HL
    | SP

    member this.GetFromCpu(cpu: Cpu) =
        match this with
        | AF -> cpu.Registers.AF
        | BC -> cpu.Registers.BC
        | DE -> cpu.Registers.DE
        | HL -> cpu.Registers.HL
        | SP -> cpu.Sp

    member this.SetToCpu (cpu: Cpu) (value: uint16) =
        match this with
        | AF -> cpu.Registers.AF <- value
        | BC -> cpu.Registers.BC <- value
        | DE -> cpu.Registers.DE <- value
        | HL -> cpu.Registers.HL <- value
        | SP -> cpu.Sp <- value

type Condition =
    | Zero
    | NotZero
    | Carry
    | NoCarry

type ArithmeticInstr =
    | IncReg8 of Reg8
    | DecReg8 of Reg8

type BitwiseInstr =
    | Bit of uint3 * Reg8
    | RlA
    | RlReg8 of Reg8

type ControlInstr =
    | Call of uint16
    | JrCond of Condition * int8

type LoadInstr =
    | LdReg8FromByte of Reg8 * uint8
    | LdReg8FromReg of Reg8 * Reg8
    | LdReg16FromWord of Reg16 * uint16
    | LdAtHLFromReg8 of Reg8
    | LdAFromAtDE
    | LdAFromAtHLDec
    | LdhAtCFromA
    | LdhAtByteFromA of uint8
    | Push of Reg16
    | Pop of Reg16

type LogicInstr = Xor8 of Reg8

type Instruction =
    | Arithmetic of ArithmeticInstr
    | Bitwise of BitwiseInstr
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

module private LengthsAndCycles =
    let forArithmetic =
        function
        | IncReg8 _ -> 1, Fixed 1
        | DecReg8 _ -> 1, Fixed 1

    let forBit =
        function
        | Bit _ -> 2, Fixed 2
        | RlReg8 _ -> 2, Fixed 2
        | RlA -> 1, Fixed 1

    let forControl =
        function
        | Call _ -> 3, Fixed 6
        | JrCond _ -> 2, Conditional { Met = 3; NotMet = 2 }

    let forLoad =
        function
        | LdReg8FromByte _ -> 2, Fixed 2
        | LdReg8FromReg _ -> 1, Fixed 1
        | LdReg16FromWord _ -> 3, Fixed 3
        | LdAtHLFromReg8 _ -> 1, Fixed 2
        | LdAFromAtDE -> 1, Fixed 2
        | LdAFromAtHLDec -> 1, Fixed 2
        | LdhAtCFromA -> 1, Fixed 2
        | LdhAtByteFromA _ -> 2, Fixed 3
        | Push _ -> 1, Fixed 4
        | Pop _ -> 1, Fixed 3

    let forLogic =
        function
        | Xor8 _ -> 1, Fixed 1

let withLengthAndCycles (instr: Instruction) =
    let length, cycles =
        match instr with
        | Arithmetic arithmeticInstr -> LengthsAndCycles.forArithmetic arithmeticInstr
        | Bitwise bitInstr -> LengthsAndCycles.forBit bitInstr
        | Control controlInstr -> LengthsAndCycles.forControl controlInstr
        | Load loadInstr -> LengthsAndCycles.forLoad loadInstr
        | Logic logicInstr -> LengthsAndCycles.forLogic logicInstr
        | Unknown -> 1, Fixed 1

    { Instruction = instr
      Length = length
      MCycles = cycles }
