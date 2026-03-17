module FameBoy.Memory

open FameBoy.Cartridge
open FameBoy.IoController

/// NOTE Access via indexer syntax: memory[addr]
type Memory =
    { VideoRam: uint8 array
      WorkRam: uint8 array
      OamRam: uint8 array
      IoController: IoController
      HighRam: uint8 array
      Cartridge: Cartridge }

    member this.Item
        with get (i: uint16) =
            let address = int i

            if address < 0x4000 then
                this.Cartridge.Rom[address]
            elif address < 0x8000 then
                this.Cartridge.Rom[this.Cartridge.RomOffset + address - 0x4000]
            elif address < 0xA000 then
                if this.IoController.PpuMode <> PpuMode.Drawing then
                    this.VideoRam[address - 0x8000]
                else
                    0xFFuy
            elif address < 0xC000 then
                if this.Cartridge.RamEnabled then
                    this.Cartridge.Ram[this.Cartridge.RamOffset + address - 0xA000]
                else
                    0xFFuy
            elif address < 0xE000 then
                this.WorkRam[address - 0xC000]
            elif address < 0xFE00 then
                this.WorkRam[address - 0xE000]
            elif address < 0xFEA0 then
                match this.IoController.PpuMode with
                | PpuMode.HBlank
                | PpuMode.VBlank -> this.OamRam[address - 0xFE00]
                | _ -> 0xFFuy
            elif address < 0xFF00 then
                0xFFuy
            elif address < 0xFF80 then
                this.IoController.CpuRead address
            elif address < 0xFFFF then
                this.HighRam[address - 0xFF80]
            else
                this.IoController.InterruptEnable

        and set (i: uint16) (v: uint8) =
            let address = int i

            if address < 0x8000 then
                handleCartridgeWrite this.Cartridge address v
            elif address < 0xA000 then
                if this.IoController.PpuMode <> PpuMode.Drawing then
                    this.VideoRam[address - 0x8000] <- v
            elif address < 0xC000 then
                if this.Cartridge.RamEnabled then
                    this.Cartridge.Ram[this.Cartridge.RamOffset + address - 0xA000] <- v
            elif address < 0xE000 then
                this.WorkRam[address - 0xC000] <- v
            elif address < 0xFE00 then
                this.WorkRam[address - 0xE000] <- v
            elif address < 0xFEA0 then
                match this.IoController.PpuMode with
                | PpuMode.HBlank
                | PpuMode.VBlank -> this.OamRam[address - 0xFE00] <- v
                | _ -> ()
            elif address < 0xFF00 then
                ()
            elif address < 0xFF80 then
                this.IoController.CpuWrite address v
            elif address < 0xFFFF then
                this.HighRam[address - 0xFF80] <- v
            else
                this.IoController.InterruptEnable <- v

// Not implementing a cycle-accurate DMA transfer, I couldn't find any games that require it
let doDmaTransfer (memory: Memory) (startPrefix: uint8) =
    let start = (int startPrefix) * 0x100

    for i in 0..0x9F do
        memory.OamRam[i] <- memory[uint16 (start + i)]

let createMemory (rom: uint8 array) ioController : Memory =
    let cartridge = createCartridge rom

    { VideoRam = Array.zeroCreate 0x2000
      WorkRam = Array.zeroCreate 0x2000
      OamRam = Array.zeroCreate 0xA0
      IoController = ioController
      HighRam = Array.zeroCreate 0x7F
      Cartridge = cartridge }
