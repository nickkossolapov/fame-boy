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

module Lcdc =
    let PpuEnable = 0b1000_0000uy
    let WindowMapArea = 0b0100_0000uy
    let WindowEnable = 0b0010_0000uy
    let TileDataArea = 0b0001_0000uy
    let BgMapArea = 0b0000_1000uy
    let ObjSize = 0b0000_0100uy
    let ObjEnable = 0b0000_0010uy
    let BgPriority = 0b0000_00001uy

    let inline isEnabled control (memory: Memory) =
        memory[IoRegisters.Lcdc] &&& control <> 0uy


// TODO palettes
module private scanline =
    let fetchPixel vramOffset offset (vram: uint8 array) =
        let left = vram[vramOffset]
        let right = vram[vramOffset + 1]

        let leftBit = left >>> offset &&& 1uy
        let rightBit = (right >>> offset &&& 1uy) <<< 1

        leftBit ||| rightBit |> Shade.ofByte

    let getTileVramOffset tileX tileY areaBit (vram: uint8 array) (memory: Memory) =
        let mapStart = if areaBit then 0x1C00 else 0x1800 // VRAM local addresses, actual: 0x9C00 and 0x9800

        let getVramOffset byte =
            if Lcdc.isEnabled Lcdc.TileDataArea memory then
                0x10 * int byte
            else
                0x800 + ((int byte + 0x80) &&& 0xFF) * 0x10 // VRAM local address, actual: 0x8800us

        let mapIndex = mapStart + (tileY * 32) + tileX

        getVramOffset vram[mapIndex]

    let decodeTileMapPixel mapX mapY areaBit (vram: uint8 array) (memory: Memory) =
        let tileX = mapX / 8
        let tileY = mapY / 8

        let bitX = 7 - mapX % 8
        let bitY = mapY % 8

        let tileOffset = getTileVramOffset tileX tileY areaBit vram memory
        let pixelOffset = tileOffset + bitY * 2

        fetchPixel pixelOffset bitX vram

    let fetchTileMapPixel screenX screenY windowOnLine (vram: uint8 array) (memory: Memory) =
        if windowOnLine && screenX >= int memory[IoRegisters.Wx] - 7 then
            let wX = screenX - (int memory[IoRegisters.Wx] - 7)
            let wY = screenY - int memory[IoRegisters.Wy]
            let areaBit = Lcdc.isEnabled Lcdc.WindowMapArea memory

            decodeTileMapPixel wX wY areaBit vram memory
        else
            let bgX = screenX + int memory[IoRegisters.Scx]
            let bgY = screenY + int memory[IoRegisters.Scy]
            let areaBit = Lcdc.isEnabled Lcdc.BgMapArea memory

            decodeTileMapPixel bgX bgY areaBit vram memory

    let oamAddresses = [| 0..4..0x9C |] // OAM local addresses, actual: 0xFE00 ... 0xFE9C

    let decodeObjectPixel screenX screenY oamAddr (oam: uint8 array) (vram: uint8 array) =
        let objX = int oam[oamAddr + 1]
        let objY = int oam[oamAddr]
        let tileOffset = 0x10 * int oam[oamAddr + 2]
        let attributes = OamAttributes.ofByte oam[oamAddr + 3]

        let localX = screenX - (objX - 8)
        let localY = screenY - (objY - 16)

        let bitX = if attributes.XFlip then localX else 7 - localX
        let bitY = if attributes.YFlip then 7 - localY else localY

        let pixelOffset = tileOffset + bitY * 2

        fetchPixel pixelOffset bitX vram

    // TODO 8x16 tiles
    let fetchObjectPixel x y (filteredOam: int array) (oam: uint8 array) (vram: uint8 array) =
        let mutable found = false // mutable makes me sad, but this is a hot path, and it's needed for an early return
        let mutable i = 0

        while i < filteredOam.Length && not found do
            let objX = int oam[filteredOam[i] + 1]

            if x >= objX - 8 && x < objX then
                found <- true
            else
                i <- i + 1

        if found then
            decodeObjectPixel x y filteredOam[i] oam vram
        else
            Shade.White

    let renderScanline (buffer: Shade array) (ppu: Ppu) =
        let vram = ppu.Memory.VideoRam
        let oam = ppu.Memory.OamRam
        let memory = ppu.Memory
        let screenY = int ppu.Ly

        let windowOnLine =
            Lcdc.isEnabled Lcdc.WindowEnable memory && screenY >= int memory[IoRegisters.Wy]

        let objectsInLine =
            oamAddresses
            |> Array.filter (fun offset -> ppu.Ly >= oam[offset] - 16uy && ppu.Ly < oam[offset] - 8uy)
            |> Array.sortBy (fun offset -> oam[offset + 1]) // DMG prioritises by X coordinate
            |> Array.truncate 10

        for screenX in 0 .. Screen.width - 1 do
            let bufferAddr = screenY * Screen.width + screenX

            let objPixel = fetchObjectPixel screenX screenY objectsInLine oam vram

            let pixel =
                if objPixel = Shade.White then
                    fetchTileMapPixel screenX screenY windowOnLine vram memory
                else
                    objPixel

            buffer[bufferAddr] <- pixel

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
