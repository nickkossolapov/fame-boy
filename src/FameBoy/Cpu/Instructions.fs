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

    member this.GetFrom(cpu: Cpu) =
        match this with
        | A -> cpu.Registers.A
        | B -> cpu.Registers.B
        | C -> cpu.Registers.C
        | D -> cpu.Registers.D
        | E -> cpu.Registers.E
        | H -> cpu.Registers.H
        | L -> cpu.Registers.L
        | F -> cpu.Registers.F

    member this.SetTo (cpu: Cpu) (value: uint8) =
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

    member this.GetFrom(cpu: Cpu) =
        match this with
        | AF -> cpu.Registers.AF
        | BC -> cpu.Registers.BC
        | DE -> cpu.Registers.DE
        | HL -> cpu.Registers.HL
        | SP -> cpu.Sp

    member this.SetTo (cpu: Cpu) (value: uint16) =
        match this with
        | AF -> cpu.Registers.AF <- value
        | BC -> cpu.Registers.BC <- value
        | DE -> cpu.Registers.DE <- value
        | HL -> cpu.Registers.HL <- value
        | SP -> cpu.Sp <- value

module LoadTypes =
    type Condition =
        | Zero
        | NotZero
        | Carry
        | NoCarry

    type ByteSource =
        | Immediate of uint8
        | RegDirect of Reg8
        | HLIndirect

        member this.GetValue(cpu: Cpu) =
            match this with
            | Immediate b -> b
            | RegDirect reg ->  reg.GetFrom cpu
            | HLIndirect -> cpu.Memory[cpu.Registers.HL]

open LoadTypes

type ArithmeticInstr =
    | IncReg8 of Reg8
    | DecReg8 of Reg8

type BitwiseInstr =
    | Bit of uint3 * Reg8
    | RlA
    | RlReg8 of Reg8

type ControlInstr =
    | Jp of uint16
    | JpHL
    | Jr of int8
    | JpCond of Condition * uint16
    | JrCond of Condition * int8
    | Call of uint16
    | CallCond of Condition * uint16
    | Ret
    | RetCond of Condition
    | Reti
    | Rst of uint8

type LoadInstr =
    | LdReg8 of Reg8 * ByteSource
    | LdAtHLFromReg8 of Reg8
    | LdAtHLFromByte of uint8
    | LdAFromAtBC
    | LdAFromAtDE
    | LdAtBCFromA
    | LdAtDEFromA
    | LdAFromAtWord of uint16
    | LdAtWordFromA of uint16
    | LdhAFromAtC
    | LdhAtCFromA
    | LdhAFromAByte of uint8
    | LdhAtByteFromA of uint8
    | LdAFromAtHLDec
    | LdAtHLDecFromA
    | LdAFromAtHLInc
    | LdAtHLIncFromA
    | LdReg16FromWord of Reg16 * uint16
    | LdAtWordFromSP of uint16
    | LdSPFromHL
    | Push of Reg16
    | Pop of Reg16
    | LdHLFromSPe of int8

type LogicInstr = Xor8 of Reg8

type Instruction =
    | Nop
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
        | Jp _ -> 3, Fixed 4
        | JpHL -> 1, Fixed 1
        | JpCond _ -> 3, Conditional { Met = 4; NotMet = 3 }
        | Jr _ -> 2, Fixed 3
        | JrCond _ -> 2, Conditional { Met = 3; NotMet = 2 }
        | Call _ -> 3, Fixed 6
        | CallCond _ -> 3, Conditional { Met = 6; NotMet = 3 }
        | Ret -> 1, Fixed 4
        | RetCond _ -> 1, Conditional { Met = 5; NotMet = 2 }
        | Reti -> 1, Fixed 4
        | Rst _ -> 1, Fixed 4

    let forLoad =
        function
        | LdReg8 (_, s) ->
            match s with
            | Immediate _ -> 2, Fixed 2
            | RegDirect _ -> 1, Fixed 1
            | HLIndirect -> 1, Fixed 2
        | LdAtHLFromReg8 _ -> 1, Fixed 2
        | LdAtHLFromByte _ -> 2, Fixed 3
        | LdAFromAtBC -> 1, Fixed 2
        | LdAFromAtDE -> 1, Fixed 2
        | LdAtBCFromA -> 1, Fixed 2
        | LdAtDEFromA -> 1, Fixed 2
        | LdAFromAtWord _ -> 3, Fixed 4
        | LdAtWordFromA _ -> 3, Fixed 4
        | LdhAFromAtC -> 1, Fixed 2
        | LdhAtCFromA -> 1, Fixed 2
        | LdhAtByteFromA _ -> 2, Fixed 3
        | LdhAFromAByte _ -> 2, Fixed 3
        | LdAFromAtHLDec -> 1, Fixed 2
        | LdAtHLDecFromA -> 1, Fixed 2
        | LdAFromAtHLInc -> 1, Fixed 2
        | LdAtHLIncFromA -> 1, Fixed 2
        | LdReg16FromWord _ -> 3, Fixed 3
        | LdAtWordFromSP _ -> 3, Fixed 5
        | LdSPFromHL -> 1, Fixed 2
        | Push _ -> 1, Fixed 4
        | Pop _ -> 1, Fixed 3
        | LdHLFromSPe _ -> 2, Fixed 3

    let forLogic =
        function
        | Xor8 _ -> 1, Fixed 1

let withLengthAndCycles (instr: Instruction) =
    let length, cycles =
        match instr with
        | Nop -> 1, Fixed 1
        | Arithmetic arithmeticInstr -> LengthsAndCycles.forArithmetic arithmeticInstr
        | Bitwise bitInstr -> LengthsAndCycles.forBit bitInstr
        | Control controlInstr -> LengthsAndCycles.forControl controlInstr
        | Load loadInstr -> LengthsAndCycles.forLoad loadInstr
        | Logic logicInstr -> LengthsAndCycles.forLogic logicInstr
        | Unknown -> 1, Fixed 1

    { Instruction = instr
      Length = length
      MCycles = cycles }
