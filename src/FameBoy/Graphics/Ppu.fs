module FameBoy.Graphics.Ppu

open FameBoy.Cpu.Interrupts
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
        with get () = this.Memory[IoRegisters.Ly]
        and set v = this.Memory[IoRegisters.Ly] <- v

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
            if gpu.Ly = gpu.Memory[IoRegisters.Lyc] then
                0b0100uy
            else
                0b0uy

        (gpu.Memory[IoRegisters.Stat] &&& 0b11111000uy) + modeMask + lycMask

let createPpu (memory: Memory) =
    let mode = if memory[IoRegisters.Ly] >= 144uy then VBlank else OamScan

    { Framebuffer = Array.create (Screen.width * Screen.height) White
      Mode = mode
      Dot = 0
      Memory = memory
      Disabled = false }

open statRegister


module private scanline =
    let mapWidth = 32 * 8
    let mapHeight = 32 * 8

    let getBgTileMemLoc tileX tileY (memory: Memory) = 0x84e0us
    // let start =
    //     if memory[IoRegisters.Lcdc] &&& 0b1000uy <> 0uy then
    //         0x9C00
    //     else
    //         0x9800
    //
    // let getLoc byte =
    //     if memory[IoRegisters.Lcdc] &&& 0b10000uy <> 0uy then
    //         0x10us * (uint16 byte) + 0x8000us
    //     else
    //         0x10us * uint16 (int8 byte) + 0x9000us
    //
    // let mapIndex = uint16 (start + (tileX * 32) + tileY)
    //
    // getLoc memory[mapIndex]

    let getBgPixel x y (memory: Memory) =
        // let tileX = mapWidth / (x + 1) - 1 // ???
        // let bitX = mapWidth % (x + 1) - 1 // ???
        //
        // let tileY = mapHeight / (y + 1) // ???
        // let bitY = mapHeight % (y + 1) - 1 // ???
        //
        // let tileLoc = getBgTileMemLoc tileX tileY memory
        // let pixelLoc = tileLoc + uint16 (bitY * 2) // 1 line is 2 bytes
        //
        // let left = memory[pixelLoc]
        // let right = memory[(pixelLoc + 1us)]
        //
        // let leftBit = left >>> bitX &&& 1uy
        // let rightBit = (right >>> bitX &&& 1uy) <<< 1
        //
        // leftBit ||| rightBit
        // |> Shade.ofByte

        (x + y) % 4 |> uint8 |> Shade.ofByte

    let renderScanline (buffer: Shade array) ly (memory: Memory) =
        let line = int ly
        let y = int line
        let xStart = int memory[IoRegisters.Scx]

        for x in xStart .. xStart + Screen.width - 1 do
            let bufferLoc = y * Screen.width + x
            buffer[bufferLoc] <- getBgPixel x y memory

open scanline

let stepPpu (ppu: Ppu) =
    ppu.Dot <- ppu.Dot + 1

    match ppu.Mode with
    | HBlank ->
        if ppu.Dot > lineEnd then
            ppu.Ly <- ppu.Ly + 1uy
            ppu.Dot <- 0
            ppu.Mode <- OamScan

        if ppu.Ly >= vBlankStart then
            ppu.Mode <- VBlank
            triggerInterrupt ppu.Memory InterruptType.VBlank
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
        if ppu.Dot = oamScanEnd + 1 then
            renderScanline ppu.Framebuffer ppu.Ly ppu.Memory

        if ppu.Dot >= 289 then
            ppu.Mode <- HBlank

    ppu.Memory[IoRegisters.Stat] <- getUpdatedStatRegister ppu
