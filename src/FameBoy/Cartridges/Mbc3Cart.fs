[<RequireQualifiedAccess>]
module FameBoy.Cartridges.Mbc3Cart

open FameBoy.Hardware

type RtcRegisters =
    { mutable Seconds: uint8
      mutable Minutes: uint8
      mutable Hours: uint8
      mutable DaysLow: uint8
      mutable DaysHigh: uint8 }

type State =
    { mutable RomBank: int
      mutable RamBank: int
      RomBankCount: int
      HasRtc: bool
      Rtc: RtcRegisters
      mutable LatchReady: bool }

let isRtcSelected (state: State) =
    state.HasRtc && state.RamBank >= 0x08 && state.RamBank <= 0x0C

let readRtcRegister (state: State) =
    match state.RamBank with
    | 0x08 -> state.Rtc.Seconds
    | 0x09 -> state.Rtc.Minutes
    | 0x0A -> state.Rtc.Hours
    | 0x0B -> state.Rtc.DaysLow
    | 0x0C -> state.Rtc.DaysHigh
    | _ -> 0xFFuy

let writeRtcRegister (state: State) (value: uint8) =
    match state.RamBank with
    | 0x08 -> state.Rtc.Seconds <- value
    | 0x09 -> state.Rtc.Minutes <- value
    | 0x0A -> state.Rtc.Hours <- value
    | 0x0B -> state.Rtc.DaysLow <- value
    | 0x0C -> state.Rtc.DaysHigh <- value
    | _ -> ()

let private getBankOffsets (state: State) =
    let romBank =
        let bank = if state.RomBank = 0 then 1 else state.RomBank
        bank &&& (state.RomBankCount - 1)

    let romOffset = romBank * CartRomBankSize

    let ramOffset =
        if state.RamBank <= 0x03 then
            state.RamBank * CartRamBankSize
        else
            0

    struct (romOffset, ramOffset)

let handleCartWrite (state: State) address value =
    if address >= 0x2000 && address < 0x4000 then
        state.RomBank <- value &&& 0x7F
    elif address >= 0x4000 && address < 0x6000 then
        state.RamBank <- value
    elif address >= 0x6000 && address < 0x8000 then
        if state.HasRtc then
            if value = 0x01 && state.LatchReady then
                let now = System.DateTime.Now
                let dayOfYear = now.DayOfYear - 1
                
                state.Rtc.Seconds <- byte now.Second
                state.Rtc.Minutes <- byte now.Minute
                state.Rtc.Hours <- byte now.Hour
                state.Rtc.DaysLow <- byte (dayOfYear &&& 0xFF)
                state.Rtc.DaysHigh <- (state.Rtc.DaysHigh &&& 0xFEuy) ||| byte ((dayOfYear >>> 8) &&& 0x01)

            state.LatchReady <- (value = 0x00)

    getBankOffsets state

let createState romBankCount hasRtc =
    { RomBank = 1
      RamBank = 0
      RomBankCount = romBankCount
      HasRtc = hasRtc
      Rtc =
        { Seconds = 0uy
          Minutes = 0uy
          Hours = 0uy
          DaysLow = 0uy
          DaysHigh = 0uy }
      LatchReady = false }
