module FameBoy.Cpu.State

open FameBoy.Memory

type Flag =
    | Zero // z
    | Subtract // n
    | HalfCarry // h
    | Carry // c

module private Flags =
    let ZMask = 0b10000000uy
    let NMask = 0b01000000uy
    let HMask = 0b00100000uy
    let CMask = 0b00010000uy

    let getFlag flag (reg: uint8) : bool =
        match flag with
        | Zero -> (reg &&& ZMask) <> 0uy
        | Subtract -> (reg &&& NMask) <> 0uy
        | HalfCarry -> (reg &&& HMask) <> 0uy
        | Carry -> (reg &&& CMask) <> 0uy

    let applyFlag flag (reg: uint8) (value: bool) =
        let mask =
            match flag with
            | Zero -> ZMask
            | Subtract -> NMask
            | HalfCarry -> HMask
            | Carry -> CMask

        if value then reg ||| mask else reg &&& ~~~mask

type Registers() =
    // Flags register is unique in that lowest 4 bits are always 0, so private state is needed
    // TODO maybe make underlying types Flags instead of uint8, and have F member expose it as a uint8 instead
    let mutable f = 0uy

    member val A = 0uy with get, set
    member val B = 0uy with get, set
    member val C = 0uy with get, set
    member val D = 0uy with get, set
    member val E = 0uy with get, set
    member val H = 0uy with get, set
    member val L = 0uy with get, set


    member this.F
        with get () = f
        and set (value: uint8) = f <- uint8 (value &&& 0xF0uy)

    member this.AF
        with get () = (uint16 this.A <<< 8) ||| uint16 this.F
        and set (value: uint16) =
            this.A <- uint8 (value >>> 8)
            this.F <- uint8 (value &&& 0xFFus)

    member this.BC
        with get () = (uint16 this.B <<< 8) ||| uint16 this.C
        and set (value: uint16) =
            this.B <- uint8 (value >>> 8)
            this.C <- uint8 (value &&& 0xFFus)

    member this.DE
        with get () = (uint16 this.D <<< 8) ||| uint16 this.E
        and set (value: uint16) =
            this.D <- uint8 (value >>> 8)
            this.E <- uint8 (value &&& 0xFFus)

    member this.HL
        with get () = (uint16 this.H <<< 8) ||| uint16 this.L
        and set (value: uint16) =
            this.H <- uint8 (value >>> 8)
            this.L <- uint8 (value &&& 0xFFus)


type Cpu =
    { Memory: Memory
      Registers: Registers
      mutable Pc: uint16
      mutable Sp: uint16
      mutable Ime: bool
      mutable Halted: bool
      mutable EnableImeNextInstr: bool }

    member this.setFlag flag value =
        this.Registers.F <- Flags.applyFlag flag this.Registers.F value

    member this.getFlag flag = Flags.getFlag flag this.Registers.F

    member this.setFlags(flags: (Flag * bool) list) =
        for flag, value in flags do
            this.setFlag flag value

let createCpu (memory: Memory) : Cpu =
    let registers = Registers ()

    { Memory = memory
      Registers = registers
      Pc = 0us
      Sp = 0us
      Ime = true
      Halted = false
      EnableImeNextInstr = false }
