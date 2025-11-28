module FameBoy.Startup

open FameBoy.Cpu.State
open FameBoy.Hardware
open FameBoy.Memory

// Set the CPU and hardware registers so a boot ROM isn't needed (https://gbdev.io/pandocs/Power_Up_Sequence.html#cpu-registers)
let createDmgCpu (memory: Memory) =
    let cpu = createCpu memory

    cpu.Registers.A <- 0x01uy
    cpu.setFlag Zero true
    cpu.Registers.B <- 0x11uy
    cpu.Registers.C <- 0x13uy
    cpu.Registers.D <- 0x00uy
    cpu.Registers.E <- 0xD8uy
    cpu.Registers.H <- 0x01uy
    cpu.Registers.L <- 0x4Duy
    cpu.Pc <- 0x0100us
    cpu.Sp <- 0xFFFEus

    cpu.Memory[Registers.P1] <- 0xCFuy
    cpu.Memory[Registers.Sb] <- 0x00uy
    cpu.Memory[Registers.Sc] <- 0x7Euy
    cpu.Memory[Registers.Div] <- 0x18uy
    cpu.Memory[Registers.Tima] <- 0x00uy
    cpu.Memory[Registers.Tma] <- 0x00uy
    cpu.Memory[Registers.Tac] <- 0xF8uy
    cpu.Memory[Registers.If] <- 0xE1uy
    cpu.Memory[Registers.Nr10] <- 0x80uy
    cpu.Memory[Registers.Nr11] <- 0xBFuy
    cpu.Memory[Registers.Nr12] <- 0xF3uy
    cpu.Memory[Registers.Nr13] <- 0xFFuy
    cpu.Memory[Registers.Nr14] <- 0xBFuy
    cpu.Memory[Registers.Nr21] <- 0x3Fuy
    cpu.Memory[Registers.Nr22] <- 0x00uy
    cpu.Memory[Registers.Nr23] <- 0xFFuy
    cpu.Memory[Registers.Nr24] <- 0xBFuy
    cpu.Memory[Registers.Nr30] <- 0x7Fuy
    cpu.Memory[Registers.Nr31] <- 0xFFuy
    cpu.Memory[Registers.Nr32] <- 0x9Fuy
    cpu.Memory[Registers.Nr33] <- 0xFFuy
    cpu.Memory[Registers.Nr34] <- 0xBFuy
    cpu.Memory[Registers.Nr41] <- 0xFFuy
    cpu.Memory[Registers.Nr42] <- 0x00uy
    cpu.Memory[Registers.Nr43] <- 0x00uy
    cpu.Memory[Registers.Nr44] <- 0xBFuy
    cpu.Memory[Registers.Nr50] <- 0x77uy
    cpu.Memory[Registers.Nr51] <- 0xF3uy
    cpu.Memory[Registers.Nr52] <- 0xF1uy
    cpu.Memory[Registers.Lcdc] <- 0x91uy
    cpu.Memory[Registers.Stat] <- 0x81uy
    cpu.Memory[Registers.Scy] <- 0x00uy
    cpu.Memory[Registers.Scx] <- 0x00uy
    cpu.Memory[Registers.Ly] <- 0x91uy
    cpu.Memory[Registers.Lyc] <- 0x00uy
    cpu.Memory[Registers.Dma] <- 0xFFuy
    cpu.Memory[Registers.Bgp] <- 0xFCuy
    cpu.Memory[Registers.Obp0] <- 0x00uy
    cpu.Memory[Registers.Obp1] <- 0x00uy
    cpu.Memory[Registers.Wy] <- 0x00uy
    cpu.Memory[Registers.Wx] <- 0x00uy
    cpu.Memory[Registers.Ie] <- 0x00uy

    cpu
