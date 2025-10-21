module FameBoy.Cpu.State

let private memorySize = 65535us

type Memory(arr: uint8 array) =
    member _.Item
        with get (i: uint16) = arr[int i]
        and set (i: uint16) (v: uint8) = arr[int i] <- v

    member _.Array = arr

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

type Registers =
    { mutable A: uint8
      mutable B: uint8
      mutable C: uint8
      mutable D: uint8
      mutable E: uint8
      mutable F: uint8
      mutable H: uint8
      mutable L: uint8 }

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
      mutable Ime: bool }

    member this.setFlag flag value =
        this.Registers.F <- Flags.applyFlag flag this.Registers.F value

    member this.getFlag flag = Flags.getFlag flag this.Registers.F

    member this.getFlagBit flag =
        if (Flags.getFlag flag this.Registers.F) then 1uy else 0uy

    member this.setFlags(flags: (Flag * bool) list) =
        for flag, value in flags do
            this.setFlag flag value

let createCpu (bootRom: uint8 array) : Cpu =
    let memory = Array.zeroCreate (int memorySize)
    Array.blit bootRom 0 memory 0 bootRom.Length

    let registers =
        { A = 0uy
          F = 0uy
          B = 0uy
          C = 0uy
          D = 0uy
          E = 0uy
          H = 0uy
          L = 0uy }

    { Memory = Memory (memory)
      Registers = registers
      Pc = 0us
      Sp = 0us
      Ime = true }
