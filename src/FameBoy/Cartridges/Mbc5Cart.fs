[<RequireQualifiedAccess>]
module FameBoy.Cartridges.Mbc5Cart

open FameBoy.Hardware

type State =
    { mutable RomBankLow: int
      mutable RomBankHigh: int
      mutable RamBank: int
      RomBankCount: int
      HasRumble: bool }

let private getBankOffsets (state: State) =
    let romBank =
        ((state.RomBankHigh <<< 8) ||| state.RomBankLow) &&& (state.RomBankCount - 1)

    let romOffset = romBank * CartRomBankSize
    let ramOffset = state.RamBank * CartRamBankSize

    struct (romOffset, ramOffset)

let handleCartWrite (state: State) address value =
    if address >= 0x2000 && address < 0x3000 then
        state.RomBankLow <- value &&& 0xFF
    elif address >= 0x3000 && address < 0x4000 then
        state.RomBankHigh <- value &&& 0x01
    elif address >= 0x4000 && address < 0x6000 then
        if state.HasRumble then
            state.RamBank <- value &&& 0x07
        else
            state.RamBank <- value &&& 0x0F

    getBankOffsets state

let createState romBankCount hasRumble =
    { RomBankLow = 1
      RomBankHigh = 0
      RamBank = 0
      RomBankCount = romBankCount
      HasRumble = hasRumble }
