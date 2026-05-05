module FameBoy.Cpu.Instructions

open FameBoy.Cpu.State

[<Struct>]
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

[<Struct>]
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
    [<Struct>]
    type Condition =
        | Zero
        | NotZero
        | Carry
        | NoCarry

    [<Struct>]
    type LoadA =
        | To
        | From

    [<Struct>]
    type ASource =
        | AtBC
        | AtDE
        | AtWord of uint16
        | AtHLInc
        | AtHLDec

    [<Struct>]
    type AHighSource =
        | AtCHigh
        | AtByteHigh of uint8

open LoadTypes

module Operand =
    [<RequireQualifiedAccess; Struct>]
    type Source =
        | Immediate of immediate: uint8
        | RegDirect of reg: Reg8
        | HLIndirect

        member this.GetFrom(cpu: Cpu) =
            match this with
            | Immediate b -> b
            | RegDirect reg -> reg.GetFrom cpu
            | HLIndirect -> cpu.Memory[cpu.Registers.HL]

    [<RequireQualifiedAccess; Struct>]
    type Target =
        | RegDirect of Reg8
        | HLIndirect

        member this.SetTo (cpu: Cpu) (value: uint8) =
            match this with
            | RegDirect reg -> reg.SetTo cpu value
            | HLIndirect -> cpu.Memory[cpu.Registers.HL] <- value

        member this.GetFrom(cpu: Cpu) =
            match this with
            | RegDirect reg -> reg.GetFrom cpu
            | HLIndirect -> cpu.Memory[cpu.Registers.HL]


open Operand

type ArithmeticInstr =
    | Add of Source
    | Adc of Source
    | Sub of Source
    | Sbc of Source
    | Cp of Source
    | Inc of Target
    | Dec of Target
    | IncReg16 of Reg16
    | DecReg16 of Reg16
    | AddHL of Reg16
    | AddSPe of int8

type BitwiseInstr =
    | Rlca
    | Rrca
    | Rra
    | Rla
    | Rlc of Target
    | Rrc of Target
    | Rl of Target
    | Rr of Target
    | Sla of Target
    | Sra of Target
    | Srl of Target
    | Swap of Target
    | Bit of uint8 * Target
    | Res of uint8 * Target
    | Set of uint8 * Target

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
    | Ld8 of Target * Source
    | LdA of LoadA * ASource
    | Ldh of LoadA * AHighSource
    | Ld16FromWord of Reg16 * uint16
    | LdAtWordFromSP of uint16
    | LdSPFromHL
    | Push of Reg16
    | Pop of Reg16
    | LdHLFromSPe of int8

type LogicInstr =
    | And of Source
    | Or of Source
    | Xor of Source
    | Ccf // Complementing carry flag
    | Scf // Set carry flag
    | Daa // Decimal adjust accumulator
    | Cpl // Complement accumulator

type Instruction =
    | Halt
    | Stop
    | Di
    | Ei
    | Nop
    | Arithmetic of ArithmeticInstr
    | Bitwise of BitwiseInstr
    | Control of ControlInstr
    | Load of LoadInstr
    | Logic of LogicInstr
    | Unknown

[<Struct>]
type ConditionalCycle = { Met: int; NotMet: int }

[<Struct>]
type MCycles =
    | Fixed of fix: int
    | Conditional of cond: ConditionalCycle

[<Struct>]
type DecodedInstruction =
    { Instruction: Instruction
      Length: int
      MCycles: MCycles }

module private LengthsAndCycles =
    let forReadByte =
        function
        | Source.Immediate _ -> 2, Fixed 2
        | Source.HLIndirect -> 1, Fixed 2
        | Source.RegDirect _ -> 1, Fixed 1

    let forArithmetic =
        function
        | Add rb -> forReadByte rb
        | Adc rb -> forReadByte rb
        | Sub rb -> forReadByte rb
        | Sbc rb -> forReadByte rb
        | Cp rb -> forReadByte rb
        | Inc wb
        | Dec wb ->
            match wb with
            | Target.HLIndirect -> 1, Fixed 3
            | Target.RegDirect _ -> 1, Fixed 1
        | IncReg16 _ -> 1, Fixed 2
        | DecReg16 _ -> 1, Fixed 2
        | AddHL _ -> 1, Fixed 2
        | AddSPe _ -> 2, Fixed 4

    let forBitwise =
        let forTargetByte =
            function
            | Target.RegDirect _ -> 2, Fixed 2
            | Target.HLIndirect -> 2, Fixed 4

        function
        | Rlca
        | Rrca -> 1, Fixed 1
        | Rra
        | Rla -> 1, Fixed 1
        | Rlc w
        | Rrc w -> forTargetByte w
        | Rl w
        | Rr w -> forTargetByte w
        | Sla w
        | Sra w -> forTargetByte w
        | Srl w
        | Swap w -> forTargetByte w
        | Bit(_, w) ->
            match w with
            | Target.RegDirect _ -> 2, Fixed 2
            | Target.HLIndirect -> 2, Fixed 3
        | Res(_, w) -> forTargetByte w
        | Set(_, w) -> forTargetByte w

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
        | Ld8(w, s) ->
            match w with
            | Target.RegDirect _ -> forReadByte s
            | Target.HLIndirect ->
                match s with
                | Source.Immediate _ -> 2, Fixed 3
                | Source.RegDirect _ -> 1, Fixed 2
                | Source.HLIndirect -> 1, Fixed 1 // ld [hl],[hl] is actually decoded as HALT
        | LdA(_, s) ->
            match s with
            | AtBC -> 1, Fixed 2
            | AtDE -> 1, Fixed 2
            | AtWord _ -> 3, Fixed 4
            | AtHLInc
            | AtHLDec -> 1, Fixed 2
        | Ldh(_, s) ->
            match s with
            | AtCHigh -> 1, Fixed 2
            | AtByteHigh _ -> 2, Fixed 3
        | Ld16FromWord _ -> 3, Fixed 3
        | LdAtWordFromSP _ -> 3, Fixed 5
        | LdSPFromHL -> 1, Fixed 2
        | Push _ -> 1, Fixed 4
        | Pop _ -> 1, Fixed 3
        | LdHLFromSPe _ -> 2, Fixed 3

    let forLogic =
        function
        | And bs -> forReadByte bs
        | Or bs -> forReadByte bs
        | Xor bs -> forReadByte bs
        | Ccf -> 1, Fixed 1
        | Scf -> 1, Fixed 1
        | Daa -> 1, Fixed 1
        | Cpl -> 1, Fixed 1

let withLengthAndCycles (instr: Instruction) =
    let length, cycles =
        match instr with
        | Halt -> 1, Fixed 1
        | Stop -> 2, Fixed 2
        | Di -> 1, Fixed 1
        | Ei -> 1, Fixed 1
        | Nop -> 1, Fixed 1
        | Arithmetic arithmeticInstr -> LengthsAndCycles.forArithmetic arithmeticInstr
        | Bitwise bitwiseInstr -> LengthsAndCycles.forBitwise bitwiseInstr
        | Control controlInstr -> LengthsAndCycles.forControl controlInstr
        | Load loadInstr -> LengthsAndCycles.forLoad loadInstr
        | Logic logicInstr -> LengthsAndCycles.forLogic logicInstr
        | Unknown -> 1, Fixed 1

    { Instruction = instr
      Length = length
      MCycles = cycles }
