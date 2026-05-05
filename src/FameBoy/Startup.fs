module FameBoy.Startup

open FameBoy.Cpu.State
open FameBoy.Cpu.State.Flags
open FameBoy.Hardware
open FameBoy.IoController
open FameBoy.Memory
open FameBoy.CgbBootPalettes

// Set the CPU and hardware registers so a boot ROM isn't needed (https://gbdev.io/pandocs/Power_Up_Sequence.html#cpu-registers)
let createDmgCpu (memory: Memory) (io: IoController) =
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
        [ Io.Joyp, 0xCFuy
          Io.Sb, 0x00uy
          Io.Sc, 0x7Euy
          Io.Div, 0x18uy
          Io.Tima, 0x00uy
          Io.Tma, 0x00uy
          Io.Tac, 0xF8uy
          Io.If, 0xE1uy
          Io.Nr10, 0x80uy
          Io.Nr11, 0xBFuy
          Io.Nr12, 0xF3uy
          Io.Nr13, 0xFFuy
          Io.Nr14, 0xBFuy
          Io.Nr21, 0x3Fuy
          Io.Nr22, 0x00uy
          Io.Nr23, 0xFFuy
          Io.Nr24, 0xBFuy
          Io.Nr30, 0x7Fuy
          Io.Nr31, 0xFFuy
          Io.Nr32, 0x9Fuy
          Io.Nr33, 0xFFuy
          Io.Nr34, 0xBFuy
          Io.Nr41, 0xFFuy
          Io.Nr42, 0x00uy
          Io.Nr43, 0x00uy
          Io.Nr44, 0xBFuy
          Io.Nr50, 0x77uy
          Io.Nr51, 0xF3uy
          Io.Nr52, 0xF1uy
          Io.Lcdc, 0x91uy
          Io.Stat, 0x81uy
          Io.Scy, 0x00uy
          Io.Scx, 0x00uy
          Io.Ly, 0x91uy
          Io.Lyc, 0x00uy
          Io.Dma, 0xFFuy
          Io.Bgp, 0xFCuy
          Io.Obp0, 0x00uy
          Io.Obp1, 0x00uy
          Io.Wy, 0x00uy
          Io.Wx, 0x00uy ]

    for reg, value in ioRegisters do
        if reg >= Io.Nr10 && reg <= 0x3F then
            io.ApuRegisters[reg] <- value
        else
            io.Registers[reg] <- value

    io.InterruptEnable <- 0x00uy
    io.PpuMode <- LanguagePrimitives.EnumOfValue(io.Registers[Io.Stat] &&& 0b0011uy)

    cpu

// CGB boot state (https://gbdev.io/pandocs/Power_Up_Sequence.html#cpu-registers)
let createCgbCpu (memory: Memory) (io: IoController) =
    let cpu = createCpu memory

    cpu.Registers.A <- 0x11uy // 0x11 indicates CGB
    cpu.Flags <- cpu.Flags |> setC false |> setH false |> setZ true |> setN false
    cpu.Registers.B <- 0x00uy
    cpu.Registers.C <- 0x00uy
    cpu.Registers.D <- 0xFFuy
    cpu.Registers.E <- 0x56uy
    cpu.Registers.H <- 0x00uy
    cpu.Registers.L <- 0x0Duy
    cpu.Pc <- 0x0100us
    cpu.Sp <- 0xFFFEus

    let ioRegisters =
        [ Io.Joyp, 0xCFuy
          Io.Sb, 0x00uy
          Io.Sc, 0x7Fuy
          Io.Div, 0x18uy
          Io.Tima, 0x00uy
          Io.Tma, 0x00uy
          Io.Tac, 0xF8uy
          Io.If, 0xE1uy
          Io.Nr10, 0x80uy
          Io.Nr11, 0xBFuy
          Io.Nr12, 0xF3uy
          Io.Nr13, 0xFFuy
          Io.Nr14, 0xBFuy
          Io.Nr21, 0x3Fuy
          Io.Nr22, 0x00uy
          Io.Nr23, 0xFFuy
          Io.Nr24, 0xBFuy
          Io.Nr30, 0x7Fuy
          Io.Nr31, 0xFFuy
          Io.Nr32, 0x9Fuy
          Io.Nr33, 0xFFuy
          Io.Nr34, 0xBFuy
          Io.Nr41, 0xFFuy
          Io.Nr42, 0x00uy
          Io.Nr43, 0x00uy
          Io.Nr44, 0xBFuy
          Io.Nr50, 0x77uy
          Io.Nr51, 0xF3uy
          Io.Nr52, 0xF1uy
          Io.Lcdc, 0x91uy
          Io.Stat, 0x81uy
          Io.Scy, 0x00uy
          Io.Scx, 0x00uy
          Io.Ly, 0x91uy
          Io.Lyc, 0x00uy
          Io.Dma, 0xFFuy
          Io.Bgp, 0xFCuy
          Io.Obp0, 0x00uy
          Io.Obp1, 0x00uy
          Io.Wy, 0x00uy
          Io.Wx, 0x00uy
          Io.Key1, 0x00uy
          Io.Vbk, 0xFEuy
          Io.Svbk, 0x01uy ]

    for reg, value in ioRegisters do
        if reg >= Io.Nr10 && reg <= 0x3F then
            io.ApuRegisters[reg] <- value
        else
            io.Registers[reg] <- value

    io.InterruptEnable <- 0x00uy
    io.PpuMode <- LanguagePrimitives.EnumOfValue(io.Registers[Io.Stat] &&& 0b0011uy)

    // Initialize CGB palette RAM
    // For CGB-only games (0xC0), set all BG palettes to white - the game will set them up
    // For dual-mode games (0x80), apply compatibility palettes based on title checksum
    let rom = memory.Cartridge.Rom

    if rom[0x143] = 0x80uy then
        applyCompatibilityPalettes rom io.BgPaletteRam io.ObjPaletteRam
    else
        // CGB-only: set palette 0 color 0 to white (game will initialize properly)
        for i in 0..7 do
            io.BgPaletteRam[i * 8] <- 0xFFuy
            io.BgPaletteRam[i * 8 + 1] <- 0x7Fuy

    cpu
