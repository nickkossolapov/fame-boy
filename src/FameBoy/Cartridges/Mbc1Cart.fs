[<RequireQualifiedAccess>]
module FameBoy.Cartridges.Mbc1Cart

open FameBoy.Hardware

type State =
    { mutable RomReg: int
      mutable RomUpperOrRamReg: int
      RomBankCount: int
      mutable AdvancedMode: bool }

let private getMbc1BankOffsets ramCount (state: State) =
    let romBank =
        let baseBank = if state.RomReg = 0x0 then 0x1 else state.RomReg

        ((state.RomUpperOrRamReg <<< 5) ||| baseBank) &&& (state.RomBankCount - 1)

    let romOffset = romBank * CartRomBankSize

    let ramOffset =
        if state.AdvancedMode && ramCount > 1 then
            state.RomUpperOrRamReg * CartRamBankSize
        else
            0

    struct (romOffset, ramOffset)

let handleCartWrite (state: State) ramCount address value =
    if address < 0x4000 && address >= 0x2000 then
        state.RomReg <- value &&& 0x1F
    elif address < 0x6000 then
        state.RomUpperOrRamReg <- value &&& 0b0011
    elif address < 0x8000 then
        state.AdvancedMode <- value &&& 0x01 <> 0

    getMbc1BankOffsets ramCount state

let createState romBankCount =
    { RomReg = 0
      RomUpperOrRamReg = 0
      RomBankCount = romBankCount
      AdvancedMode = false }
