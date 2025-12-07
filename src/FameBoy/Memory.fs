module FameBoy.Memory

module private Helpers =
    type MemoryRegion =
        | RomBase of offset: int
        | RomBank of offset: int
        | VideoRam of offset: int
        | ExternalRam of offset: int
        | WorkRam of offset: int
        | OamRam of offset: int
        | IoRegisters of offset: int
        | HighRam of offset: int
        | InterruptEnable
        | Unusable

    let mapAddress (addr: int) : MemoryRegion =
        match addr with
        | a when a < 0x4000 -> RomBase a
        | a when a < 0x8000 -> RomBank (a - 0x4000)
        | a when a < 0xA000 -> VideoRam (a - 0x8000)
        | a when a < 0xC000 -> ExternalRam (a - 0xA000)
        | a when a < 0xE000 -> WorkRam (a - 0xC000)
        | a when a < 0xFE00 -> WorkRam (a - 0xE000) // Echo RAM
        | a when a < 0xFEA0 -> OamRam (a - 0xFE00)
        | a when a < 0xFF00 -> Unusable
        | a when a < 0xFF80 -> IoRegisters (a - 0xFF00)
        | a when a < 0xFFFF -> HighRam (a - 0xFF80)
        | _ -> InterruptEnable

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

module IoRegistersOffsets =
    [<Literal>]
    let ioOffset = 0xFF00

    [<Literal>]
    let Joypad = 0xFF00 - ioOffset

    [<Literal>]
    let Dma = 0xFF46 - ioOffset

type Memory =
    abstract member writeIoDirect: uint16 -> uint8 -> unit
    abstract member Item: uint16 -> uint8 with get
    abstract member Item: uint16 -> uint8 with set

type DmgMemory(arr: uint8 array) =
    let romBase, romBanks = getRomBanks arr
    let mutable currentBank = 0
    let videoRam = Array.zeroCreate<uint8> memorySizes.vram
    let externalRam = Array.zeroCreate<uint8> memorySizes.externalRam
    let workRam = Array.zeroCreate<uint8> memorySizes.wram // TODO: GCB: split this and have banking
    let oamRam = Array.zeroCreate<uint8> memorySizes.oam
    let ioRegisters = Array.zeroCreate<uint8> memorySizes.ioRegisters
    let highRam = Array.zeroCreate<uint8> memorySizes.hram
    let mutable interruptEnable = 0uy

    member private this.writeIoRegisters address (value: uint8) =
        match address with
        | IoRegistersOffsets.Joypad ->
            // Lower nibble in Joypad register is read only
            // ioRegisters[address] <- (value &&& 0b11110000uy) ||| (ioRegisters[address] &&& 0b00001111uy)
            let s: string = $"{System.Convert.ToString(value, 2).PadLeft (8, '0')}"
            ioRegisters[address] <- value
        | IoRegistersOffsets.Dma ->
            ioRegisters[address] <- value
            this.doDmaTransfer value
        | _ -> ioRegisters[int address] <- value

    // TODO maybe move this out and do a m-cycle accurate transfer?
    member private this.doDmaTransfer(startPrefix: uint8) =
        let start = (int startPrefix) * 0x100

        for i in 0..0xA0 do
            let src = start + i
            let dst = 0xFE00 + i

            this.read src |> this.write dst

    member private this.read(address: int) =
        match mapAddress address with
        | RomBase i -> romBase[i]
        | RomBank i -> romBanks[currentBank][i]
        | VideoRam i -> videoRam[i]
        | ExternalRam i -> externalRam[i]
        | WorkRam i -> workRam[i]
        | OamRam i -> oamRam[i]
        | IoRegisters i -> ioRegisters[i]
        | HighRam i -> highRam[i]
        | InterruptEnable -> interruptEnable
        | Unusable -> 0xFFuy

    member private this.write address value =
        match mapAddress address with
        | RomBase _ -> ()
        | RomBank _ -> ()
        | VideoRam i -> videoRam[i] <- value
        | ExternalRam i -> externalRam[i] <- value
        | WorkRam i -> workRam[i] <- value
        | OamRam i -> oamRam[i] <- value
        | IoRegisters i -> this.writeIoRegisters i value
        | HighRam i -> highRam[i] <- value
        | InterruptEnable -> interruptEnable <- value
        | Unusable -> ()

    interface Memory with
        member this.writeIoDirect (address: uint16) value =
            let offset = int address - IoRegistersOffsets.ioOffset

            ioRegisters[offset] <- value

        member this.Item
            with get (i: uint16) = this.read (int i)
            and set (i: uint16) (v: uint8) = this.write (int i) v

type TestMemory(arr: uint8 array) =
    interface Memory with
        member _.Item
            with get (i: uint16) = arr[int i]
            and set (i: uint16) (v: uint8) = arr[int i] <- v

        member _.writeIoDirect i value = arr[int i] <- value

let createMemory rom : Memory = DmgMemory rom

let createTestMemory arr : Memory =
    let memory = Array.zeroCreate 0x10000

    Array.blit arr 0 memory 0 arr.Length

    TestMemory memory
