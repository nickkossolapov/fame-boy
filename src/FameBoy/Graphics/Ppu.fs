module FameBoy.Graphics.Ppu

open FameBoy.Cpu.Interrupts
open FameBoy.Hardware
open FameBoy.Memory

[<Struct>]
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
    | White = 0
    | Light = 1
    | Dark = 2
    | Black = 3

module Shade =
    let ofByte (i: uint8) = LanguagePrimitives.EnumOfValue(int i)

type Ppu =
    { Framebuffer: Shade array
      mutable Mode: PpuMode
      mutable Dot: int
      Memory: Memory
      mutable Disabled: bool }

    member this.Ly
        with get () = this.Memory[IoRegisters.Ly]
        and set v = this.Memory[IoRegisters.Ly] <- v

module private Oam =
    // 0uy = OBP0, 1uy = OBP1
    type DmgPalette = byte

    [<Literal>]
    let OBP0: DmgPalette = 0b00000uy

    [<Literal>]
    let OBP1: DmgPalette = 0b10000uy

    type OamAttributes =
        { Priority: bool
          XFlip: bool
          YFlip: bool
          DmgPalette: DmgPalette }

        static member ofByte b =
            { Priority = b &&& 0b10000000uy <> 0uy
              YFlip = b &&& 0b01000000uy <> 0uy
              XFlip = b &&& 0b00100000uy <> 0uy
              DmgPalette = b &&& OBP1 }

open Oam

module private StatRegister =
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

    { Framebuffer = Array.create (Screen.width * Screen.height) Shade.White
      Mode = mode
      Dot = 0
      Memory = memory
      Disabled = false }

open StatRegister


// TODO palettes
module private scanline =
    let getBgTileMemLoc tileX tileY (memory: Memory) =
        let start =
            if memory[IoRegisters.Lcdc] &&& 0b1000uy <> 0uy then
                0x9C00
            else
                0x9800

        let getLoc byte =
            if memory[IoRegisters.Lcdc] &&& 0b10000uy <> 0uy then
                0x8000us + 0x10us * (uint16 byte)
            else
                0x8800us + ((uint16 byte + 0x80us) &&& 0xFFus) * 0x10us

        let mapIndex = uint16 (start + (tileY * 32) + tileX)

        getLoc memory[mapIndex]

    let fetchBgPixel screenX screenY (memory: Memory) =
        let bgX = screenX + int memory[IoRegisters.Scx]
        let bgY = screenY + int memory[IoRegisters.Scy]

        let tileX = bgX / 8
        let tileY = bgY / 8

        let bitX = 7 - bgX % 8
        let bitY = bgY % 8

        let tileLoc = getBgTileMemLoc tileX tileY memory
        let pixelLoc = tileLoc + uint16 (bitY * 2)

        let left = memory[pixelLoc]
        let right = memory[(pixelLoc + 1us)]

        let leftBit = left >>> bitX &&& 1uy
        let rightBit = (right >>> bitX &&& 1uy) <<< 1

        leftBit ||| rightBit |> Shade.ofByte

    let oamMap = [ 0xFE00us .. 4us .. 0xFE9Fus ]

    let fetchTransformedObjectPixel screenX screenY oamLoc (memory: Memory) =
        let objX = int memory[oamLoc + 1us]
        let objY = int memory[oamLoc]
        let objLoc = 0x8000us + 0x10us * (uint16 memory[oamLoc + 2us])
        let attributes = OamAttributes.ofByte memory[oamLoc + 3us]

        let localX = screenX - (objX - 8)
        let localY = screenY - (objY - 16)

        let bitX = if attributes.XFlip then localX else 7 - localX
        let bitY = if attributes.YFlip then 7 - localY else localY

        let pixelLoc = objLoc + uint16 (bitY * 2)

        let left = memory[pixelLoc]
        let right = memory[(pixelLoc + 1us)]

        let leftBit = left >>> bitX &&& 1uy
        let rightBit = (right >>> bitX &&& 1uy) <<< 1

        leftBit ||| rightBit |> Shade.ofByte

    // TODO 8x16 tiles
    let fetchObjectPixel x y filteredOam (memory: Memory) =
        let mutable found = false // mutable makes me sad, but this is a hot path
        let mutable i = 0
        let len = List.length filteredOam

        while i < len && not found do
            let objX = int memory[filteredOam[i] + 1us]

            if x >= objX - 8 && x < objX then
                found <- true
            else
                i <- i + 1

        if found then
            fetchTransformedObjectPixel x y filteredOam[i] memory
        else
            Shade.White

    let renderScanline (buffer: Shade array) (ppu: Ppu) =
        let screenY = int ppu.Ly

        let objectsInLine =
            oamMap
            |> List.where (fun loc -> ppu.Ly >= ppu.Memory[loc] - 16uy && ppu.Ly < ppu.Memory[loc] - 8uy)
            |> List.sortBy (fun loc -> ppu.Memory[loc + 1us]) // DMG prioritises by X coordinate. TODO GCB prioritise by OAM only
        // TODO List.take 10? Do I want to be hardware accurate?

        for screenX in 0 .. Screen.width - 1 do
            let bufferLoc = screenY * Screen.width + screenX

            let objPixel = fetchObjectPixel screenX screenY objectsInLine ppu.Memory

            let pixel =
                if objPixel = Shade.White then
                    fetchBgPixel screenX screenY ppu.Memory
                else
                    objPixel

            buffer[bufferLoc] <- pixel

open scanline

let stepPpu (ppu: Ppu) =
    ppu.Dot <- ppu.Dot + 1

    match ppu.Mode with
    | HBlank ->
        if ppu.Dot > lineEnd then
            ppu.Ly <- (ppu.Ly + 1uy) &&& 0xFFuy
            ppu.Dot <- 0
            ppu.Mode <- OamScan

            if ppu.Ly >= vBlankStart then
                ppu.Mode <- VBlank
                triggerInterrupt ppu.Memory InterruptType.VBlank
    | VBlank ->
        if ppu.Dot > lineEnd then
            ppu.Ly <- (ppu.Ly + 1uy) &&& 0xFFuy
            ppu.Dot <- 0

            if ppu.Ly >= frameEnd then
                ppu.Ly <- 0uy
                ppu.Mode <- OamScan
    | OamScan ->
        if ppu.Dot >= oamScanEnd then
            ppu.Mode <- Drawing
    | Drawing ->
        if ppu.Dot = oamScanEnd + 1 then
            renderScanline ppu.Framebuffer ppu

        if ppu.Dot >= 289 then
            ppu.Mode <- HBlank

    ppu.Memory[IoRegisters.Stat] <- getUpdatedStatRegister ppu
