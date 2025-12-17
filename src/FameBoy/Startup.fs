module FameBoy.Startup

open FameBoy.Cpu.State
open FameBoy.Hardware
open FameBoy.Memory

// Set the CPU and hardware registers so a boot ROM isn't needed (https://gbdev.io/pandocs/Power_Up_Sequence.html#cpu-registers)
let createDmgCpu (memory: Memory) =
    let cpu = createCpu memory

    cpu.Registers.A <- 0x01uy
    cpu.setFlags [ Carry, true; HalfCarry, true; Zero, true ]
    cpu.Registers.B <- 0x00uy
    cpu.Registers.C <- 0x13uy
    cpu.Registers.D <- 0x00uy
    cpu.Registers.E <- 0xD8uy
    cpu.Registers.H <- 0x01uy
    cpu.Registers.L <- 0x4Duy
    cpu.Pc <- 0x0100us
    cpu.Sp <- 0xFFFEus

    cpu.Memory[IoRegisters.P1] <- 0xCFuy
    cpu.Memory[IoRegisters.Sb] <- 0x00uy
    cpu.Memory[IoRegisters.Sc] <- 0x7Euy
    cpu.Memory[IoRegisters.Div] <- 0x18uy
    cpu.Memory[IoRegisters.Tima] <- 0x00uy
    cpu.Memory[IoRegisters.Tma] <- 0x00uy
    cpu.Memory[IoRegisters.Tac] <- 0xF8uy
    cpu.Memory[IoRegisters.If] <- 0xE1uy
    cpu.Memory[IoRegisters.Nr10] <- 0x80uy
    cpu.Memory[IoRegisters.Nr11] <- 0xBFuy
    cpu.Memory[IoRegisters.Nr12] <- 0xF3uy
    cpu.Memory[IoRegisters.Nr13] <- 0xFFuy
    cpu.Memory[IoRegisters.Nr14] <- 0xBFuy
    cpu.Memory[IoRegisters.Nr21] <- 0x3Fuy
    cpu.Memory[IoRegisters.Nr22] <- 0x00uy
    cpu.Memory[IoRegisters.Nr23] <- 0xFFuy
    cpu.Memory[IoRegisters.Nr24] <- 0xBFuy
    cpu.Memory[IoRegisters.Nr30] <- 0x7Fuy
    cpu.Memory[IoRegisters.Nr31] <- 0xFFuy
    cpu.Memory[IoRegisters.Nr32] <- 0x9Fuy
    cpu.Memory[IoRegisters.Nr33] <- 0xFFuy
    cpu.Memory[IoRegisters.Nr34] <- 0xBFuy
    cpu.Memory[IoRegisters.Nr41] <- 0xFFuy
    cpu.Memory[IoRegisters.Nr42] <- 0x00uy
    cpu.Memory[IoRegisters.Nr43] <- 0x00uy
    cpu.Memory[IoRegisters.Nr44] <- 0xBFuy
    cpu.Memory[IoRegisters.Nr50] <- 0x77uy
    cpu.Memory[IoRegisters.Nr51] <- 0xF3uy
    cpu.Memory[IoRegisters.Nr52] <- 0xF1uy
    cpu.Memory[IoRegisters.Lcdc] <- 0x91uy
    cpu.Memory[IoRegisters.Stat] <- 0x81uy
    cpu.Memory[IoRegisters.Scy] <- 0x00uy
    cpu.Memory[IoRegisters.Scx] <- 0x00uy
    cpu.Memory[IoRegisters.Ly] <- 0x91uy
    cpu.Memory[IoRegisters.Lyc] <- 0x00uy
    cpu.Memory[IoRegisters.Dma] <- 0xFFuy
    cpu.Memory[IoRegisters.Bgp] <- 0xFCuy
    cpu.Memory[IoRegisters.Obp0] <- 0x00uy
    cpu.Memory[IoRegisters.Obp1] <- 0x00uy
    cpu.Memory[IoRegisters.Wy] <- 0x00uy
    cpu.Memory[IoRegisters.Wx] <- 0x00uy
    cpu.Memory[IoRegisters.Ie] <- 0x00uy

    cpu
