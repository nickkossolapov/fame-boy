module FameBoy.IoController

open FameBoy.Interrupts
open FameBoy.Hardware
open FameBoy.Joypad

type PpuMode =
    | HBlank = 0uy
    | VBlank = 1uy
    | OamScan = 2uy
    | Drawing = 3uy

type IoController =
    { Registers: uint8 array
      mutable PpuMode: PpuMode
      mutable InterruptEnable: uint8
      mutable JoypadState: JoypadState
      mutable DmaRequest: uint8 voption
      mutable ApuRegisters: uint8 array
      mutable StatDirty: bool
      // GBC color palette RAM (64 bytes each for BG and OBJ)
      BgPaletteRam: uint8 array
      ObjPaletteRam: uint8 array
      mutable BgPaletteIndex: uint8
      mutable BgPaletteAutoIncrement: bool
      mutable ObjPaletteIndex: uint8
      mutable ObjPaletteAutoIncrement: bool
      // GBC VRAM/WRAM bank
      mutable VramBank: int
      mutable WramBank: int
      // GBC mode flag
      mutable CgbMode: bool
      // CGB compatibility mode (DMG game running on CGB - uses BGP remapping)
      mutable CgbCompatMode: bool
      // HDMA
      mutable HdmaSource: uint16
      mutable HdmaDest: uint16
      mutable HdmaLength: int
      mutable HdmaActive: bool
      mutable HdmaHblank: bool
      // Double speed mode (CGB)
      mutable DoubleSpeed: bool }

    member this.CpuWrite fullAddress value =
        let offset = fullAddress - Io.IoMemoryOffset

        match offset with
        | Io.Joyp ->
            // Lower nibble in Joypad register is read only
            this.Registers[offset] <- (value &&& 0b1111_0000uy) ||| (this.Registers[offset] &&& 0b0000_1111uy)
        | Io.Stat ->
            // LYC == LY and PPU mode are read only
            this.Registers[offset] <- (value &&& 0b1111_1000uy) ||| (this.Registers[offset] &&& 0b0000_0111uy)
            this.StatDirty <- true
        | Io.Ly -> () // Read only, set directly in PPU
        | Io.Lyc ->
            this.Registers[offset] <- value
            this.StatDirty <- true
        | Io.Dma ->
            this.Registers[offset] <- value
            this.DmaRequest <- ValueSome value
        | Io.Nr52 ->
            // Only bit 7 (power) is writable - lower bits are read-only channel status
            this.ApuRegisters[offset] <- (value &&& 0x80uy) ||| (this.ApuRegisters[offset] &&& 0x7Fuy)
        | offset when offset >= Io.Nr10 && offset <= 0x3F -> this.ApuRegisters[offset] <- value
        // GBC registers
        | Io.Vbk when this.CgbMode ->
            this.VramBank <- int (value &&& 0x01uy)
            this.Registers[offset] <- value ||| 0xFEuy
        | Io.Svbk when this.CgbMode ->
            let bank = int (value &&& 0x07uy)
            this.WramBank <- if bank = 0 then 1 else bank
            this.Registers[offset] <- value
        | Io.Bcps when this.CgbMode ->
            this.BgPaletteIndex <- value &&& 0x3Fuy
            this.BgPaletteAutoIncrement <- value &&& 0x80uy <> 0uy
            this.Registers[offset] <- value
        | Io.Bcpd when this.CgbMode ->
            this.BgPaletteRam[int this.BgPaletteIndex] <- value
            if this.BgPaletteAutoIncrement then
                this.BgPaletteIndex <- (this.BgPaletteIndex + 1uy) &&& 0x3Fuy
        | Io.Ocps when this.CgbMode ->
            this.ObjPaletteIndex <- value &&& 0x3Fuy
            this.ObjPaletteAutoIncrement <- value &&& 0x80uy <> 0uy
            this.Registers[offset] <- value
        | Io.Ocpd when this.CgbMode ->
            this.ObjPaletteRam[int this.ObjPaletteIndex] <- value
            if this.ObjPaletteAutoIncrement then
                this.ObjPaletteIndex <- (this.ObjPaletteIndex + 1uy) &&& 0x3Fuy
        | Io.Hdma1 when this.CgbMode ->
            this.HdmaSource <- (this.HdmaSource &&& 0x00FFus) ||| (uint16 value <<< 8)
        | Io.Hdma2 when this.CgbMode ->
            this.HdmaSource <- (this.HdmaSource &&& 0xFF00us) ||| (uint16 (value &&& 0xF0uy))
        | Io.Hdma3 when this.CgbMode ->
            this.HdmaDest <- (this.HdmaDest &&& 0x00FFus) ||| (uint16 (value &&& 0x1Fuy) <<< 8)
        | Io.Hdma4 when this.CgbMode ->
            this.HdmaDest <- (this.HdmaDest &&& 0xFF00us) ||| (uint16 (value &&& 0xF0uy))
        | Io.Hdma5 when this.CgbMode ->
            let length = (int (value &&& 0x7Fuy) + 1) * 0x10
            let hblankMode = value &&& 0x80uy <> 0uy

            if this.HdmaActive && not hblankMode then
                // Writing 0 to bit 7 while HBlank HDMA active cancels it
                this.HdmaActive <- false
                this.Registers[offset] <- value ||| 0x80uy
            else
                this.HdmaLength <- length
                this.HdmaHblank <- hblankMode
                this.HdmaActive <- true

                if not hblankMode then
                    // General-purpose DMA: transfer immediately
                    this.Registers[offset] <- 0xFFuy
                else
                    this.Registers[offset] <- value &&& 0x7Fuy
        | Io.Key1 when this.CgbMode ->
            // Only bit 0 (prepare speed switch) is writable
            this.Registers[offset] <- value &&& 0x01uy
        | _ -> this.Registers[offset] <- value

    member this.CpuRead fullAddress =
        let offset = fullAddress - Io.IoMemoryOffset

        if offset = Io.Joyp then
            this.Registers[Io.Joyp] <- applyJoypadState this.JoypadState this.Registers[Io.Joyp] this.TriggerInterrupt

        if offset >= Io.Nr10 && offset <= 0x3F then
            this.ApuRegisters[offset]
        elif offset = Io.Bcpd && this.CgbMode then
            this.BgPaletteRam[int this.BgPaletteIndex]
        elif offset = Io.Ocpd && this.CgbMode then
            this.ObjPaletteRam[int this.ObjPaletteIndex]
        elif offset = Io.Vbk && this.CgbMode then
            0xFEuy ||| uint8 this.VramBank
        elif offset = Io.Hdma5 && this.CgbMode then
            if this.HdmaActive then
                uint8 ((this.HdmaLength / 0x10) - 1)
            else
                0xFFuy
        elif offset = Io.Key1 && this.CgbMode then
            let speedBit = if this.DoubleSpeed then 0x80uy else 0x00uy
            speedBit ||| (this.Registers[Io.Key1] &&& 0x01uy)
        else
            this.Registers[offset]

    member this.TriggerInterrupt(t: InterruptType) =
        let mask = getInterruptMask t

        this.Registers[Io.If] <- this.Registers[Io.If] ||| mask

    member this.ClearInterruptFlag(flag: InterruptType) =
        this.Registers[Io.If] <- this.Registers[Io.If] &&& ~~~(getInterruptMask flag)

let createIoController () =
    { Registers = Array.zeroCreate 0x80
      PpuMode = PpuMode.HBlank
      InterruptEnable = 0x0uy
      JoypadState =
        { Up = false
          Down = false
          Left = false
          Right = false
          A = false
          B = false
          Start = false
          Select = false }
      DmaRequest = ValueNone
      ApuRegisters = Array.zeroCreate 0x80
      StatDirty = false
      BgPaletteRam = Array.zeroCreate 64
      ObjPaletteRam = Array.zeroCreate 64
      BgPaletteIndex = 0uy
      BgPaletteAutoIncrement = false
      ObjPaletteIndex = 0uy
      ObjPaletteAutoIncrement = false
      VramBank = 0
      WramBank = 1
      CgbMode = false
      CgbCompatMode = false
      HdmaSource = 0us
      HdmaDest = 0us
      HdmaLength = 0
      HdmaActive = false
      HdmaHblank = false
      DoubleSpeed = false }
