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
      mutable GetApuChannelStates: unit -> uint8
      mutable OnApuTrigger: int -> unit }

    member this.CpuWrite fullAddress value =
        let offset = fullAddress - Io.IoMemoryOffset

        match offset with
        | Io.Joyp ->
            // Lower nibble in Joypad register is read only
            this.Registers[offset] <- (value &&& 0b1111_0000uy) ||| (this.Registers[offset] &&& 0b0000_1111uy)
        | Io.Stat ->
            // LYC == LY and PPU mode are read only
            this.Registers[offset] <- (value &&& 0b1111_1000uy) ||| (this.Registers[offset] &&& 0b0000_0111uy)
        | Io.Ly -> () // Read only, set directly in PPU
        | Io.Dma ->
            this.Registers[offset] <- value
            this.DmaRequest <- ValueSome value
        | Io.Nr52 ->
            // Only bit 7 (power) is writable - lower bits are read-only channel status
            this.Registers[offset] <- (value &&& 0x80uy) ||| (this.Registers[offset] &&& 0x7Fuy)
        | _ ->
            this.Registers[offset] <- value

            if value &&& 0b1000_0000uy <> 0uy
               && (offset = Io.Nr14 || offset = Io.Nr24 || offset = Io.Nr34 || offset = Io.Nr44) then
                this.OnApuTrigger offset

    member this.CpuRead fullAddress =
        let offset = fullAddress - Io.IoMemoryOffset

        if offset = Io.Joyp then
            this.Registers[Io.Joyp] <- applyJoypadState this.JoypadState this.Registers[Io.Joyp] this.TriggerInterrupt

        if offset = Io.Nr52 then
            this.Registers[Io.Nr52] <- (this.Registers[Io.Nr52] &&& 0x80uy) ||| this.GetApuChannelStates()

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
      GetApuChannelStates = fun () -> 0uy
      OnApuTrigger = ignore }
