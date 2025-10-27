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

    type LoadA =
        | To
        | From

    type ASource =
        | AtBC
        | AtDE
        | AtCHigh
        | AtByteHigh of uint8
        | AtWord of uint16
        | AtHLInc
        | AtHLDec

open LoadTypes

module ByteSource =
    [<RequireQualifiedAccess>]
    type Read =
        | Immediate of uint8
        | RegDirect of Reg8
        | HLIndirect

        member this.GetFrom(cpu: Cpu) =
            match this with
            | Immediate b -> b
            | RegDirect reg -> reg.GetFrom cpu
            | HLIndirect -> cpu.Memory[cpu.Registers.HL]

    [<RequireQualifiedAccess>]
    type Write =
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

open ByteSource

type ArithmeticInstr =
    | Add of Read
    | Adc of Read
    | Sub of Read
    | Sbc of Read
    | Cp of Read
    | Inc of Write
    | Dec of Write
    | IncReg16 of Reg16
    | DecReg16 of Reg16
    | AddHL of Reg16
    | AddSPe of int8

type BitwiseInstr =
    | Rlca
    | Rrca
    | Rra
    | Rla
    | Rlc of Write
    | Rrc of Write
    | Rl of Write
    | Rr of Write
    | Sla of Write
    | Sra of Write
    | Srl of Write
    | Swap of Write
    | Bit of uint3 * Write
    | Res of uint3 * Write
    | Set of uint3 * Write

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
    | LdReg8 of Reg8 * Read
    | LdAtHLFromReg8 of Reg8
    | LdAtHLFromByte of uint8
    | LdA of LoadA * ASource
    | LdReg16FromWord of Reg16 * uint16
    | LdAtWordFromSP of uint16
    | LdSPFromHL
    | Push of Reg16
    | Pop of Reg16
    | LdHLFromSPe of int8

type LogicInstr =
    | And of Read
    | Or of Read
    | Xor of Read
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

type ConditionalCycle = { Met: int; NotMet: int }

type MCycles =
    | Fixed of int
    | Conditional of ConditionalCycle

type DecodedInstruction =
    { Instruction: Instruction
      Length: int
      MCycles: MCycles }

module private LengthsAndCycles =
    let forReadByte =
        function
        | Read.Immediate _ -> 2, Fixed 2
        | Read.HLIndirect -> 1, Fixed 2
        | Read.RegDirect _ -> 1, Fixed 1

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
            | Write.HLIndirect -> 1, Fixed 3
            | Write.RegDirect _ -> 1, Fixed 1
        | IncReg16 _ -> 1, Fixed 2
        | DecReg16 _ -> 1, Fixed 2
        | AddHL _ -> 1, Fixed 2
        | AddSPe _ -> 2, Fixed 4

    let forBitwise =
        let forWriteByte =
            function
            | Write.RegDirect _ -> 2, Fixed 2
            | Write.HLIndirect -> 2, Fixed 4

        function
        | Rlca
        | Rrca -> 1, Fixed 1
        | Rra
        | Rla -> 1, Fixed 1
        | Rlc w
        | Rrc w -> forWriteByte w
        | Rl w
        | Rr w -> forWriteByte w
        | Sla w
        | Sra w -> forWriteByte w
        | Srl w
        | Swap w -> forWriteByte w
        | Bit (_, w) ->
            match w with
            | Write.RegDirect _ -> 2, Fixed 2
            | Write.HLIndirect -> 2, Fixed 3
        | Res (_, w) -> forWriteByte w
        | Set (_, w) -> forWriteByte w

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
        | LdReg8 (_, s) -> forReadByte s
        | LdAtHLFromReg8 _ -> 1, Fixed 2
        | LdAtHLFromByte _ -> 2, Fixed 3
        | LdA (_, s) ->
            match s with
            | AtBC -> 1, Fixed 2
            | AtDE -> 1, Fixed 2
            | AtWord _ -> 3, Fixed 4
            | AtCHigh -> 1, Fixed 2
            | AtByteHigh _ -> 2, Fixed 3
            | AtHLInc
            | AtHLDec -> 1, Fixed 2
        | LdReg16FromWord _ -> 3, Fixed 3
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
