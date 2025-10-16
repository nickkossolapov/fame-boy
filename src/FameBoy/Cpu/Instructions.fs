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

type Condition =
    | Zero
    | NotZero
    | Carry
    | NoCarry

type ArithmeticInstr = IncReg of Reg8

type BitwiseInstr = Bit of uint3 * Reg8

type ControlInstr = JrCond of Condition * int8

type LoadInstr =
    | LdRegFromByte of Reg8 * uint8
    | LdRegFromWord of Reg16 * uint16
    | LdAtHLFromReg of Reg8
    | LdAFromAtDE
    | LdAFromAtHLDec
    | LdhAtCFromA
    | LdhAtByteFromA of uint8

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
        | LdAFromAtDE -> 1, Fixed 2
        | LdAFromAtHLDec -> 1, Fixed 2
        | LdhAtCFromA -> 1, Fixed 2
        | LdhAtByteFromA _ -> 2, Fixed 3

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
