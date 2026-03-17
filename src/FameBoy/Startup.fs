module FameBoy.Startup

open FameBoy.Cpu.State
open FameBoy.Cpu.State.Flags
open FameBoy.Hardware
open FameBoy.Memory

// Set the CPU and hardware registers so a boot ROM isn't needed (https://gbdev.io/pandocs/Power_Up_Sequence.html#cpu-registers)
let createDmgCpu (memory: Memory) =
    let cpu = createCpu memory

    cpu.Registers.A <- 0x01uy
    cpu.Flags <- cpu.Flags |> setC true |> setH true |> setZ true
    cpu.Registers.B <- 0x00uy
    cpu.Registers.C <- 0x13uy
    cpu.Registers.D <- 0x00uy
    cpu.Registers.E <- 0xD8uy
    cpu.Registers.H <- 0x01uy
    cpu.Registers.L <- 0x4Duy
    cpu.Pc <- 0x0100us
    cpu.Sp <- 0xFFFEus

    let ioRegisters =
        [ IoRegisters.Joyp, 0xCFuy
          IoRegisters.Sb, 0x00uy
          IoRegisters.Sc, 0x7Euy
          IoRegisters.Div, 0x18uy
          IoRegisters.Tima, 0x00uy
          IoRegisters.Tma, 0x00uy
          IoRegisters.Tac, 0xF8uy
          IoRegisters.If, 0xE1uy
          IoRegisters.Nr10, 0x80uy
          IoRegisters.Nr11, 0xBFuy
          IoRegisters.Nr12, 0xF3uy
          IoRegisters.Nr13, 0xFFuy
          IoRegisters.Nr14, 0xBFuy
          IoRegisters.Nr21, 0x3Fuy
          IoRegisters.Nr22, 0x00uy
          IoRegisters.Nr23, 0xFFuy
          IoRegisters.Nr24, 0xBFuy
          IoRegisters.Nr30, 0x7Fuy
          IoRegisters.Nr31, 0xFFuy
          IoRegisters.Nr32, 0x9Fuy
          IoRegisters.Nr33, 0xFFuy
          IoRegisters.Nr34, 0xBFuy
          IoRegisters.Nr41, 0xFFuy
          IoRegisters.Nr42, 0x00uy
          IoRegisters.Nr43, 0x00uy
          IoRegisters.Nr44, 0xBFuy
          IoRegisters.Nr50, 0x77uy
          IoRegisters.Nr51, 0xF3uy
          IoRegisters.Nr52, 0xF1uy
          IoRegisters.Lcdc, 0x91uy
          IoRegisters.Stat, 0x81uy
          IoRegisters.Scy, 0x00uy
          IoRegisters.Scx, 0x00uy
          IoRegisters.Ly, 0x91uy
          IoRegisters.Lyc, 0x00uy
          IoRegisters.Dma, 0xFFuy
          IoRegisters.Bgp, 0xFCuy
          IoRegisters.Obp0, 0x00uy
          IoRegisters.Obp1, 0x00uy
          IoRegisters.Wy, 0x00uy
          IoRegisters.Wx, 0x00uy ]

    for reg, value in ioRegisters do
        cpu.Memory.IoRegisters[int reg - 0xFF00] <- value

    memory.InterruptEnable <- 0x00uy
    memory.PpuMode <- LanguagePrimitives.EnumOfValue(memory[IoRegisters.Stat] &&& 0b0011uy)

    cpu
