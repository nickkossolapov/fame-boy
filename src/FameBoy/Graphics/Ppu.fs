module FameBoy.Graphics.Ppu

open FameBoy.Hardware
open FameBoy.Memory

type PpuMode =
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
    | White
    | Light
    | Dark
    | Black

    static member ofByte =
        function
        | 0uy -> White
        | 1uy -> Light
        | 2uy -> Dark
        | _ -> Black

type Ppu =
    { Framebuffer: Shade array
      mutable Mode: PpuMode
      mutable Dot: int
      Memory: Memory
      mutable Disabled: bool }

    member this.Ly
        with get () = this.Memory[Registers.Ly]
        and set v = this.Memory[Registers.Ly] <- v

let createPpu (memory: Memory) =
    { Framebuffer = Array.create (Screen.width * Screen.height) White
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

let stepPpu (ppu: Ppu) =
    ppu.Dot <- ppu.Dot + 1

    let nextStat = getUpdatedStatRegister ppu

    match ppu.Mode with
    | HBlank ->
        if ppu.Dot > lineEnd then
            ppu.Ly <- ppu.Ly + 1uy
            ppu.Dot <- 0

        if ppu.Ly >= vBlankStart then
            ppu.Mode <- VBlank
    | VBlank ->
        if ppu.Dot > lineEnd then
            ppu.Ly <- ppu.Ly + 1uy
            ppu.Dot <- 0

        if ppu.Ly >= frameEnd then
            ppu.Ly <- 0uy
            ppu.Mode <- OamScan
    | OamScan ->
        if ppu.Dot >= oamScanEnd then
            ppu.Mode <- Drawing
    | Drawing ->
        if ppu.Dot >= 289 then
            ppu.Mode <- HBlank

    ppu.Memory[Registers.Stat] <- nextStat
