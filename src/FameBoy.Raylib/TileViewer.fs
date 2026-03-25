module FameBoy.Raylib.TileViewer

open FameBoy.Hardware
open FameBoy.Ppu

let renderTile x y vramOffset (ppu: Ppu) (buffer: Shade array) =
    for row in 0..7 do
        let addr = vramOffset + (row * 2)
        let left = ppu.VideoRam[addr]
        let right = ppu.VideoRam[addr + 1]

        for col in 0..7 do
            let bit = 7 - col
            let leftBit = left >>> bit &&& 1uy
            let rightBit = (right >>> bit &&& 1uy) <<< 1
            let net = leftBit ||| rightBit

            let bufferPos = ((y + row) * 256) + (x + col)

            buffer[bufferPos] <- Shade.ofByte net

let dumpBackground (buffer: Shade array) (ppu: Ppu) =
    let start =
        if ppu.IoController.Registers[Io.Lcdc] &&& 0b1000uy <> 0uy then
            0x1C00
        else
            0x1800

    let getVramOffset byte =
        if Lcdc.isEnabled Lcdc.TileDataArea ppu.IoController then
            0x10 * int byte
        else
            0x800 + ((int byte + 0x80) &&& 0xFF) * 0x10

    for row in 0..31 do
        for col in 0..31 do
            let mapIndex = start + (row * 32) + col
            let tileIndex = getVramOffset ppu.VideoRam[mapIndex]

            renderTile (col * 8) (row * 8) (int tileIndex) ppu buffer

let dumpTiles (buffer: Shade array) (ppu: Ppu) =
    for row in 0..11 do // 384 total tiles -> 384/32 = 12 rows
        for col in 0..31 do
            let mapIndex = ((row * 32) + col) * 16

            renderTile (col * 8) (row * 8) (int mapIndex) ppu buffer
