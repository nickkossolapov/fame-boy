module FameBoy.Memory

open FameBoy.Hardware

module private Helpers =
    let memorySizes =
        {| romBank = 0x4000
           vram = 0x2000
           wram = 0x2000
           externalRam = 0x2000
           oam = 0xA0
           ioRegisters = 0x80
           hram = 0x7F |}

    let getRomBanks (arr: uint8 array) =
        match (Array.chunkBySize memorySizes.romBank arr) with
        | [||] -> Array.zeroCreate memorySizes.romBank, Array.zeroCreate memorySizes.romBank
        | [| bank |] -> bank, Array.zeroCreate memorySizes.romBank
        | banks -> banks[0], banks[1..]

open Helpers

/// NOTE Access via indexer syntax: memory[addr]
type Memory =
    { RomBase: uint8 array
      RomBanks: uint8 array array
      mutable CurrentBank: int
      VideoRam: uint8 array
      ExternalRam: uint8 array
      WorkRam: uint8 array // TODO: GCB: split this and have banking
      OamRam: uint8 array
      IoRegisters: uint8 array
      HighRam: uint8 array
      mutable InterruptEnable: uint8 }

    member private this.writeIoRegisters address (value: uint8) =
        match address with
        | IoRegisterOffsets.Joyp ->
            // Lower nibble in Joypad register is read only
            this.IoRegisters[address] <- (value &&& 0b1111_0000uy) ||| (this.IoRegisters[address] &&& 0b0000_1111uy)
        | IoRegisterOffsets.Stat ->
            // LYC == LY and PPU mode are read only
            this.IoRegisters[address] <- (value &&& 0b1111_1000uy) ||| (this.IoRegisters[address] &&& 0b0000_0111uy)
        | IoRegisterOffsets.Ly -> () // Read only, set directly in PPU
        | IoRegisterOffsets.Dma ->
            this.IoRegisters[address] <- value
            this.doDmaTransfer value
        | _ -> this.IoRegisters[address] <- value

    // TODO maybe move this out and do a m-cycle accurate transfer?
    member private this.doDmaTransfer(startPrefix: uint8) =
        let start = (int startPrefix) * 0x100

        for i in 0..0x9F do
            let src = uint16 (start + i)
            let dst = uint16 (0xFE00 + i)

            this[dst] <- this[src]

    member this.Item
        with get (i: uint16) =
            let address = int i

            if address < 0x4000 then
                this.RomBase[address]
            elif address < 0x8000 then
                this.RomBanks[this.CurrentBank][address - 0x4000]
            elif address < 0xA000 then
                this.VideoRam[address - 0x8000]
            elif address < 0xC000 then
                this.ExternalRam[address - 0xA000]
            elif address < 0xE000 then
                this.WorkRam[address - 0xC000]
            elif address < 0xFE00 then
                this.WorkRam[address - 0xE000]
            elif address < 0xFEA0 then
                this.OamRam[address - 0xFE00]
            elif address < 0xFF00 then
                0xFFuy
            elif address < 0xFF80 then
                this.IoRegisters[address - 0xFF00]
            elif address < 0xFFFF then
                this.HighRam[address - 0xFF80]
            else
                this.InterruptEnable

        and set (i: uint16) (v: uint8) =
            let address = int i

            if address < 0x4000 then
                ()
            elif address < 0x8000 then
                ()
            elif address < 0xA000 then
                this.VideoRam[address - 0x8000] <- v
            elif address < 0xC000 then
                this.ExternalRam[address - 0xA000] <- v
            elif address < 0xE000 then
                this.WorkRam[address - 0xC000] <- v
            elif address < 0xFE00 then
                this.WorkRam[address - 0xE000] <- v
            elif address < 0xFEA0 then
                this.OamRam[address - 0xFE00] <- v
            elif address < 0xFF00 then
                ()
            elif address < 0xFF80 then
                this.writeIoRegisters (address - 0xFF00) v
            elif address < 0xFFFF then
                this.HighRam[address - 0xFF80] <- v
            else
                this.InterruptEnable <- v

let createMemory (rom: uint8 array) : Memory =
    let romBase, romBanks = getRomBanks rom

    { RomBase = romBase
      RomBanks = romBanks
      CurrentBank = 0
      VideoRam = Array.zeroCreate memorySizes.vram
      ExternalRam = Array.zeroCreate memorySizes.externalRam
      WorkRam = Array.zeroCreate memorySizes.wram
      OamRam = Array.zeroCreate memorySizes.oam
      IoRegisters = Array.zeroCreate memorySizes.ioRegisters
      HighRam = Array.zeroCreate memorySizes.hram
      InterruptEnable = 0uy }

let createTestMemory (arr: uint8 array) : Memory =
    let memory = Array.zeroCreate 0x10000
    Array.blit arr 0 memory 0 arr.Length

    let romBase, romBanks = getRomBanks memory[0..0x7FFF]

    { RomBase = romBase
      RomBanks = romBanks
      CurrentBank = 0
      VideoRam = memory[0x8000..0x9FFF]
      ExternalRam = memory[0xA000..0xBFFF]
      WorkRam = memory[0xC000..0xDFFF]
      OamRam = memory[0xFE00..0xFE9F]
      IoRegisters = memory[0xFF00..0xFF7F]
      HighRam = memory[0xFF80..0xFFFE]
      InterruptEnable = memory[0xFFFF] }
