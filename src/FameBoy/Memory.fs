module FameBoy.Memory

open FameBoy.Cartridge
open FameBoy.IoController

/// NOTE Access via indexer syntax: memory[addr]
type Memory =
    { VideoRam: uint8 array // 2 banks of 8KB for GBC (16KB total)
      WorkRam: uint8 array // 8 banks of 4KB for GBC (32KB total)
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
                    let bankOffset = this.IoController.VramBank * 0x2000
                    this.VideoRam[bankOffset + address - 0x8000]
                else
                    0xFFuy
            elif address < 0xC000 then
                if this.Cartridge.RamEnabled then
                    readCartRam this.Cartridge address
                else
                    0xFFuy
            elif address < 0xD000 then
                // WRAM bank 0 is always at the first 4KB
                this.WorkRam[address - 0xC000]
            elif address < 0xE000 then
                // WRAM bank 1-7 (switchable in CGB mode)
                let bankOffset = this.IoController.WramBank * 0x1000
                this.WorkRam[bankOffset + address - 0xD000]
            elif address < 0xF000 then
                // Echo of WRAM bank 0
                this.WorkRam[address - 0xE000]
            elif address < 0xFE00 then
                // Echo of WRAM bank 1-7
                let bankOffset = this.IoController.WramBank * 0x1000
                this.WorkRam[bankOffset + address - 0xF000]
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
                    let bankOffset = this.IoController.VramBank * 0x2000
                    this.VideoRam[bankOffset + address - 0x8000] <- v
            elif address < 0xC000 then
                if this.Cartridge.RamEnabled then
                    writeCartRam this.Cartridge address v
            elif address < 0xD000 then
                this.WorkRam[address - 0xC000] <- v
            elif address < 0xE000 then
                let bankOffset = this.IoController.WramBank * 0x1000
                this.WorkRam[bankOffset + address - 0xD000] <- v
            elif address < 0xF000 then
                this.WorkRam[address - 0xE000] <- v
            elif address < 0xFE00 then
                let bankOffset = this.IoController.WramBank * 0x1000
                this.WorkRam[bankOffset + address - 0xF000] <- v
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

// GBC HDMA transfer (general purpose - copies all bytes at once)
let doHdmaTransfer (memory: Memory) =
    let io = memory.IoController

    if io.HdmaActive && not io.HdmaHblank then
        let mutable src = int io.HdmaSource
        let mutable dst = int io.HdmaDest

        for _ in 0 .. io.HdmaLength - 1 do
            let srcAddr = uint16 src
            let dstOffset = (dst &&& 0x1FFF) + io.VramBank * 0x2000
            if dstOffset < memory.VideoRam.Length then
                memory.VideoRam[dstOffset] <- memory[srcAddr]
            src <- src + 1
            dst <- dst + 1

        io.HdmaSource <- uint16 src
        io.HdmaDest <- uint16 (dst &&& 0x1FFF)
        io.HdmaActive <- false
        io.HdmaLength <- 0

// GBC HDMA HBlank transfer (copies 16 bytes per HBlank)
let doHdmaHblankBlock (memory: Memory) =
    let io = memory.IoController

    if io.HdmaActive && io.HdmaHblank && io.HdmaLength > 0 then
        let mutable src = int io.HdmaSource
        let mutable dst = int io.HdmaDest

        for _ in 0..15 do
            let srcAddr = uint16 src
            let dstOffset = (dst &&& 0x1FFF) + io.VramBank * 0x2000
            if dstOffset < memory.VideoRam.Length then
                memory.VideoRam[dstOffset] <- memory[srcAddr]
            src <- src + 1
            dst <- dst + 1

        io.HdmaSource <- uint16 src
        io.HdmaDest <- uint16 (dst &&& 0x1FFF)
        io.HdmaLength <- io.HdmaLength - 0x10

        if io.HdmaLength <= 0 then
            io.HdmaActive <- false

let isCgbRom (rom: uint8 array) =
    rom.Length > 0x143 && (rom[0x143] = 0x80uy || rom[0x143] = 0xC0uy)

let createMemory (rom: uint8 array) ioController : Memory =
    let cartridge = createCartridge rom
    let cgbMode = isCgbRom rom
    ioController.CgbMode <- cgbMode
    ioController.CgbCompatMode <- cgbMode && rom.Length > 0x143 && rom[0x143] = 0x80uy

    // GBC: 2 VRAM banks (16KB), 8 WRAM banks (32KB)
    // DMG: 1 VRAM bank (8KB), 2 WRAM banks (8KB) - but allocate full size for simplicity
    let vramSize = if cgbMode then 0x4000 else 0x2000
    let wramSize = if cgbMode then 0x8000 else 0x2000

    { VideoRam = Array.zeroCreate vramSize
      WorkRam = Array.zeroCreate wramSize
      OamRam = Array.zeroCreate 0xA0
      IoController = ioController
      HighRam = Array.zeroCreate 0x7F
      Cartridge = cartridge }
