module FameBoy.Ppu

open FameBoy.Hardware
open FameBoy.Memory

type GpuMode =
    | HBlank
    | VBlank
    | OamScan
    | Drawing

module private ScanlineTimings =
    let lineEnd = 455 // dots, end of blanks
    let frameEnd = 153uy // lines per screen
    let vBlankStart = 144uy // lines
    let oamScanEnd = 80 // dots

open ScanlineTimings

type Shade =
    | White = 0uy
    | Light = 1uy
    | Dark = 2uy
    | Black = 3uy

type Ppu =
    { Framebuffer: Shade array
      mutable Mode: GpuMode
      mutable Dot: int
      Memory: Memory
      mutable Disabled: bool }

    member this.Ly
        with get () = this.Memory[Registers.Ly]
        and set v = this.Memory[Registers.Ly] <- v

let createPpu (memory: Memory) =
    { Framebuffer = Array.zeroCreate (Screen.width * Screen.height)
      Mode = OamScan
      Dot = 0
      Memory = memory
      Disabled = false }

module private statRegister =
    let getModeMask =
        function
        | HBlank -> 0uy
        | VBlank -> 1uy
        | OamScan -> 2uy
        | Drawing -> 3uy

    // Based on https://gbdev.io/pandocs/STAT.html#ff41--stat-lcd-status
    let getUpdatedStatRegister gpu =
        let modeMask = getModeMask gpu.Mode

        let lycMask =
            if gpu.Ly = gpu.Memory[Registers.Lyc] then
                0b0100uy
            else
                0b0uy

        (gpu.Memory[Registers.Stat] &&& 0b11111000uy) + modeMask + lycMask

open statRegister

let stepPpu (Ppu: Ppu) =
    Ppu.Dot <- Ppu.Dot + 1

    let nextStat = getUpdatedStatRegister Ppu

    match Ppu.Mode with
    | HBlank ->
        if Ppu.Dot > lineEnd then
            Ppu.Ly <- Ppu.Ly + 1uy
            Ppu.Dot <- 0

        if Ppu.Ly >= vBlankStart then
            Ppu.Mode <- VBlank
    | VBlank ->
        if Ppu.Dot > lineEnd then
            Ppu.Ly <- Ppu.Ly + 1uy
            Ppu.Dot <- 0

        if Ppu.Ly >= frameEnd then
            Ppu.Ly <- 0uy
            Ppu.Mode <- OamScan
    | OamScan ->
        if Ppu.Dot >= oamScanEnd then
            Ppu.Mode <- Drawing
    | Drawing ->
        if Ppu.Dot >= 289 then
            Ppu.Mode <- HBlank

    Ppu.Memory[Registers.Stat] <- nextStat
