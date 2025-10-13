module FameBoy.Cpu.State

let private memorySize = 65536

type Flag =
    | Zero // z
    | Subtraction // n
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
        | Subtraction -> (reg &&& NMask) <> 0uy
        | HalfCarry -> (reg &&& HMask) <> 0uy
        | Carry -> (reg &&& CMask) <> 0uy

    let applyFlag flag (reg: uint8) (value: bool) =
        let mask =
            match flag with
            | Zero -> ZMask
            | Subtraction -> NMask
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

    member this.getBC() = (uint16 this.B <<< 8) ||| uint16 this.C

    member this.setBC(value: uint16) =
        this.B <- uint8 (value >>> 8)
        this.C <- uint8 (value &&& 0xFFus)

    member this.getDE() = (uint16 this.D <<< 8) ||| uint16 this.E

    member this.setDE(value: uint16) =
        this.D <- uint8 (value >>> 8)
        this.E <- uint8 (value &&& 0xFFus)

    member this.getHL() = (uint16 this.H <<< 8) ||| uint16 this.L

    member this.setHL(value: uint16) =
        this.H <- uint8 (value >>> 8)
        this.L <- uint8 (value &&& 0xFFus)

type Cpu =
    { Memory: uint8 array
      Registers: Registers
      mutable Pc: int // Actually a 16-bit unsigned int, but making it an int for easier use in array indexing
      mutable Sp: uint16 }

    member this.setFlag flag value =
        this.Registers.F <- Flags.applyFlag flag this.Registers.F value

    member this.getFlag flag = Flags.getFlag flag this.Registers.F

    /// <summary>
    /// Sets the F register to the given 4-bit value (only last 4 bits of the input int are used).
    /// Bits are in the order ZNHC (zero, negative, half-carry, carry).
    /// </summary>
    member this.setFlags(value: int) =
        this.Registers.F <- (uint8 (value &&& 0b1111) <<< 4)


let createCpu (bootRom: uint8 array) : Cpu =
    let memory = Array.zeroCreate memorySize
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

    { Memory = memory
      Registers = registers
      Pc = 0
      Sp = 0us }
