module FameBoy.Cpu.State

let private memorySize = 65536

type Registers =
    { mutable A: uint8
      mutable F: uint8
      mutable B: uint8
      mutable C: uint8
      mutable D: uint8
      mutable E: uint8
      mutable H: uint8
      mutable L: uint8 }

type Cpu =
    { Memory: uint8 array
      Registers: Registers
      mutable Pc: int // Actually a 16-bit unsigned int, but making it an int for easier use in array indexing
      mutable Sp: uint16 }

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
