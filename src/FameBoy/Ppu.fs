module FameBoy.Graphics.Ppu

open System
open FameBoy.Cpu.Interrupts
open FameBoy.Hardware
open FameBoy.Memory

module private ScanlineTimings =
    let lineEnd = 456 // dots, end of blanks
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
    let ofByte (i: uint8) : Shade = LanguagePrimitives.EnumOfValue(int i)

type Ppu =
    { Framebuffer: Shade array
      Backbuffer: Shade array
      mutable Dot: int
      mutable WindowLine: int
      mutable StatSignal: bool
      Memory: Memory
      mutable Disabled: bool }

    member this.Ly
        with get () = this.Memory.IoRegisters[IoRegisterOffsets.Ly]
        and set v = this.Memory.IoRegisters[IoRegisterOffsets.Ly] <- v

module private Oam =

    [<Literal>]
    let private OBP1 = 0b0001_0000uy

    type OamAttributes =
        { Priority: bool
          XFlip: bool
          YFlip: bool
          UseObp1: bool }

        static member ofByte b =
            { Priority = b &&& 0b1000_0000uy <> 0uy
              YFlip = b &&& 0b0100_0000uy <> 0uy
              XFlip = b &&& 0b0010_0000uy <> 0uy
              UseObp1 = b &&& OBP1 <> 0uy }

open Oam

module private StatRegister =
    // Based on https://gbdev.io/pandocs/STAT.html#ff41--stat-lcd-status
    let getUpdatedStatRegister (ppu: Ppu) =
        let lycMask =
            if ppu.Ly = ppu.Memory[IoRegisters.Lyc] then
                0b0100uy
            else
                0b0uy

        (ppu.Memory[IoRegisters.Stat] &&& 0b1111_1000uy)
        + (uint8 ppu.Memory.PpuMode)
        + lycMask

let private bufferWidth = (Screen.width * Screen.height)

let createPpu (memory: Memory) =
    { Framebuffer = Array.create bufferWidth Shade.White
      Backbuffer = Array.create bufferWidth Shade.White
      Dot = 0
      WindowLine = 0
      StatSignal = false
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
    let BgEnable = 0b0000_00001uy

    let inline isEnabled control (memory: Memory) =
        memory[IoRegisters.Lcdc] &&& control <> 0uy

module private Palettes =
    let private parsePaletteData byte =
        [| byte >>> 0 &&& 0b0011uy
           byte >>> 2 &&& 0b0011uy
           byte >>> 4 &&& 0b0011uy
           byte >>> 6 &&& 0b0011uy |]
        |> Array.map (int >> LanguagePrimitives.EnumOfValue)

    let fetchPaletteMaps (memory: Memory) =
        parsePaletteData memory[IoRegisters.Bgp], parsePaletteData memory[IoRegisters.Obp0], parsePaletteData memory[IoRegisters.Obp1]

open Palettes

module private scanline =
    type ObjPixel =
        { Shade: Shade
          UseObp1: bool
          BgPriority: bool }

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

    let fetchTileMapPixel screenX screenY windowOnLine (ppu: Ppu) =
        if windowOnLine && screenX >= int ppu.Memory[IoRegisters.Wx] - 7 then
            let wX = screenX - (int ppu.Memory[IoRegisters.Wx] - 7)
            let areaBit = Lcdc.isEnabled Lcdc.WindowMapArea ppu.Memory

            decodeTileMapPixel wX ppu.WindowLine areaBit ppu.Memory.VideoRam ppu.Memory
        else
            let bgX = (screenX + int ppu.Memory[IoRegisters.Scx]) % 256
            let bgY = (screenY + int ppu.Memory[IoRegisters.Scy]) % 256
            let areaBit = Lcdc.isEnabled Lcdc.BgMapArea ppu.Memory

            decodeTileMapPixel bgX bgY areaBit ppu.Memory.VideoRam ppu.Memory

    let decodeObjectPixel screenX screenY isDoubleHeight oamAddr (oam: uint8 array) (vram: uint8 array) =
        let objX = int oam[oamAddr + 1]
        let objY = int oam[oamAddr]
        let tileNum = int oam[oamAddr + 2]
        let attributes = OamAttributes.ofByte oam[oamAddr + 3]

        let spriteHeight = if isDoubleHeight then 16 else 8
        let spriteY = screenY - (objY - 16)

        let flippedY =
            if attributes.YFlip then
                (spriteHeight - 1) - spriteY
            else
                spriteY

        let isBottomTile = isDoubleHeight && flippedY >= 8

        let effectiveTileNum =
            if isDoubleHeight then
                if isBottomTile then tileNum ||| 0x01 else tileNum &&& 0xFE
            else
                tileNum

        let tileOffset = 0x10 * effectiveTileNum

        let localX = screenX - (objX - 8)
        let localY = if isBottomTile then flippedY - 8 else flippedY

        let bitX = if attributes.XFlip then localX else 7 - localX
        let bitY = localY

        let pixelOffset = tileOffset + bitY * 2

        { Shade = fetchPixel pixelOffset bitX vram
          UseObp1 = attributes.UseObp1
          BgPriority = attributes.Priority }

    let fetchObjectPixel x y isDoubleHeight (filteredOam: int array) (oam: uint8 array) (vram: uint8 array) =
        let mutable found = ValueNone // mutable makes me sad, but this is a hot path, and it's needed for an early return
        let mutable i = 0

        while i < filteredOam.Length && found.IsNone do
            let objX = int oam[filteredOam[i] + 1]

            if x >= objX - 8 && x < objX then
                let pixel = decodeObjectPixel x y isDoubleHeight filteredOam[i] oam vram

                if pixel.Shade <> Shade.White then
                    found <- ValueSome pixel

            i <- i + 1

        found

    let oamAddresses = [| 0..4..0x9C |] // OAM local addresses, actual: 0xFE00 ... 0xFE9C

    let renderScanline (buffer: Shade array) (ppu: Ppu) =
        let vram = ppu.Memory.VideoRam
        let oam = ppu.Memory.OamRam
        let memory = ppu.Memory
        let screenY = int ppu.Ly

        let bgpMap, obp0Map, obp1Map = fetchPaletteMaps memory

        let windowOnLine =
            Lcdc.isEnabled Lcdc.WindowEnable memory && screenY >= int memory[IoRegisters.Wy]

        let objEnable = Lcdc.isEnabled Lcdc.ObjEnable memory
        let bgEnable = Lcdc.isEnabled Lcdc.BgEnable memory

        let isDoubleHeight = Lcdc.isEnabled Lcdc.ObjSize ppu.Memory
        let objBottom = if isDoubleHeight then 0 else 8

        let objectsInLine =
            oamAddresses
            |> Array.filter (fun offset -> int ppu.Ly >= int oam[offset] - 16 && int ppu.Ly < int oam[offset] - objBottom)
            |> Array.sortBy (fun offset -> oam[offset + 1]) // DMG prioritises by X coordinate
            |> Array.truncate 10

        let inline mapObjPixel pixel =
            if pixel.UseObp1 then
                obp1Map[int pixel.Shade]
            else
                obp0Map[int pixel.Shade]

        let fetchPrioritisedPixels screenX =
            let objPixel =
                fetchObjectPixel screenX screenY isDoubleHeight objectsInLine oam vram

            match objPixel with
            | ValueSome p when p.Shade <> Shade.White ->
                if p.BgPriority then
                    let bgPixel = fetchTileMapPixel screenX screenY windowOnLine ppu

                    if bgPixel <> Shade.White then
                        bgpMap[int bgPixel]
                    else
                        mapObjPixel p
                else
                    mapObjPixel p
            | _ -> bgpMap[int (fetchTileMapPixel screenX screenY windowOnLine ppu)]

        for screenX in 0 .. Screen.width - 1 do
            let bufferAddr = screenY * Screen.width + screenX

            let pixel =
                match objEnable, bgEnable with
                | true, true -> fetchPrioritisedPixels screenX
                | true, false ->
                    fetchObjectPixel screenX screenY isDoubleHeight objectsInLine oam vram
                    |> ValueOption.map mapObjPixel
                    |> ValueOption.defaultValue Shade.White
                | false, true -> bgpMap[int (fetchTileMapPixel screenX screenY windowOnLine ppu)]
                | false, false -> Shade.White

            buffer[bufferAddr] <- pixel

        if windowOnLine then
            ppu.WindowLine <- ppu.WindowLine + 1

open scanline

let private disablePpu (ppu: Ppu) =
    if not ppu.Disabled then
        ppu.Disabled <- true
        ppu.Memory.PpuMode <- PpuMode.HBlank
        ppu.Ly <- 0uy
        ppu.Dot <- 0

        for i in 0 .. (bufferWidth - 1) do
            ppu.Framebuffer[i] <- Shade.White
            ppu.Backbuffer[i] <- Shade.White

let stepPpu (ppu: Ppu) =
    if not (Lcdc.isEnabled Lcdc.PpuEnable ppu.Memory) then
        disablePpu ppu
    else if ppu.Disabled then
        ppu.Disabled <- false
        ppu.Memory.PpuMode <- PpuMode.OamScan

    if ppu.Disabled then
        ()
    else
        ppu.Dot <- ppu.Dot + 1

        match ppu.Memory.PpuMode with
        | PpuMode.HBlank ->
            if ppu.Dot >= lineEnd then
                ppu.Ly <- (ppu.Ly + 1uy) &&& 0xFFuy
                ppu.Dot <- 0
                ppu.Memory.PpuMode <- PpuMode.OamScan

                if ppu.Ly >= vBlankStart then
                    ppu.Memory.PpuMode <- PpuMode.VBlank
                    ppu.WindowLine <- 0

                    triggerInterrupt ppu.Memory InterruptType.VBlank
        | PpuMode.VBlank ->
            if ppu.Dot >= lineEnd then
                ppu.Ly <- (ppu.Ly + 1uy) &&& 0xFFuy
                ppu.Dot <- 0

                if ppu.Ly > frameEnd then
                    ppu.Ly <- 0uy
                    ppu.Memory.PpuMode <- PpuMode.OamScan

                    Array.blit ppu.Backbuffer 0 ppu.Framebuffer 0 ppu.Framebuffer.Length
        | PpuMode.OamScan ->
            if ppu.Dot >= oamScanEnd then
                ppu.Memory.PpuMode <- PpuMode.Drawing

        | PpuMode.Drawing ->
            if ppu.Dot = oamScanEnd + 1 then
                renderScanline ppu.Backbuffer ppu

            if ppu.Dot >= 253 then // Since it's scanline rendering, have the shortest drawing phase: 80+172 dots
                ppu.Memory.PpuMode <- PpuMode.HBlank
        | _ -> ArgumentOutOfRangeException(nameof PpuMode) |> raise

        let stat = getUpdatedStatRegister ppu

        let newLine =
            (stat &&& 0b0000_1000uy <> 0uy && ppu.Memory.PpuMode = PpuMode.HBlank)
            || (stat &&& 0b0001_0000uy <> 0uy && ppu.Memory.PpuMode = PpuMode.VBlank)
            || (stat &&& 0b0010_0000uy <> 0uy && ppu.Memory.PpuMode = PpuMode.OamScan)
            || (stat &&& 0b0100_0000uy <> 0uy && ppu.Ly = ppu.Memory[IoRegisters.Lyc])

        // Only trigger interrupt on the rising edge of the interrupt signal, needed for STAT blocking
        if newLine && not ppu.StatSignal then
            triggerInterrupt ppu.Memory InterruptType.LcdStat

        ppu.StatSignal <- newLine
        ppu.Memory.IoRegisters[IoRegisterOffsets.Stat] <- stat
