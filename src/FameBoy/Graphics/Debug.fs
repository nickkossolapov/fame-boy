module FameBoy.Ppu.Debug

open FameBoy.Hardware
open FameBoy.Memory
open FameBoy.Graphics.Ppu

let renderTile x y loc (memory: Memory) (buffer: Shade array) =
    for row in 0..7 do
        let addr = loc + (row * 2)
        let left = memory.Array[addr]
        let right = memory.Array[(addr + 1)]

        for col in 0..7 do
            let bit = 7 - col
            let leftBit = left >>> bit &&& 1uy
            let rightBit = (right >>> bit &&& 1uy) <<< 1
            let net = leftBit ||| rightBit

            let bufferPos = ((y + row) * 256) + (x + col)

            buffer[bufferPos] <- Shade.ofByte net

let dumpBackground (buffer: Shade array) (memory: Memory) =
    let start =
        if memory[Registers.Lcdc] &&& 0b1000uy <> 0uy then
            0x9C00
        else
            0x9800

    let getLoc byte =
        if memory[Registers.Lcdc] &&& 0b10000uy <> 0uy then
            0x10us * (uint16 byte) + 0x8000us
        else
            0x10us * uint16 (int8 byte) + 0x9000us

    for row in 0..31 do
        for col in 0..31 do
            let mapIndex = start + (row * 32) + col
            let tileIndex = getLoc memory.Array[mapIndex]

            renderTile (col * 8) (row * 8) (int tileIndex) memory buffer

let dumpTiles (buffer: Shade array) (memory: Memory) =
    for row in 0..11 do // 384 total tiles -> 384/32 = 12 rows
        for col in 0..31 do
            let mapIndex = 0x8000 + ((row * 32) + col) * 16

            renderTile (col * 8) (row * 8) (int mapIndex) memory buffer
