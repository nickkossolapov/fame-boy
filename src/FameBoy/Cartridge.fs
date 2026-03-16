module FameBoy.Cartridge


let private RomBankSize = 0x4000
let private RamBankSize = 0x2000


module Mbcs =
    type Mbc1State =
        { mutable RomReg: int
          mutable RomUpperOrRamReg: int
          RomBankCount: int
          mutable AdvancedMode: bool }

    type Mbc =
        | None
        | Mbc1 of Mbc1State

open Mbcs

module private MbcRegisters =
    let getMbc1RomBank (state: Mbc1State) =
        let baseBank = if state.RomReg = 0x0 then 0x1 else state.RomReg

        ((state.RomUpperOrRamReg <<< 5) ||| baseBank) &&& (state.RomBankCount - 1)

    let createMbc1State romBankCount =
        { RomReg = 0
          RomUpperOrRamReg = 0
          RomBankCount = romBankCount
          AdvancedMode = false }

open MbcRegisters

module private Headers =
    // https://gbdev.io/pandocs/The_Cartridge_Header.html#the-cartridge-header

    let getRomBankCount (rom: uint8 array) = rom.Length / RomBankSize

    let getRamBankCount (rom: uint8 array) hasRam =
        if not hasRam then
            0
        else
            match rom[0x149] with
            | 0x00uy // Type says RAM exists but size says 0 - assume 1 bank for safety
            | 0x01uy
            | 0x02uy -> 1
            | 0x03uy -> 4
            | 0x04uy -> 16
            | 0x05uy -> 8
            | _ -> 1 // Unknown - assume 1 bank for safety

    let getMbc (rom: uint8 array) romSize =
        match rom[0x147] with
        | 0x0uy -> Mbc.None, false
        | 0x01uy -> createMbc1State romSize |> Mbc1, false
        | 0x02uy
        | 0x03uy -> createMbc1State romSize |> Mbc1, true
        | 0x08uy
        | 0x09uy -> None, true
        // | _ -> None, false
        | _ -> failwith "Unimplemented MBC"

open Headers

type Cartridge =
    { Mbc: Mbc
      Rom: uint8 array
      RomCount: int
      mutable RomOffset: int
      Ram: uint8 array
      RamCount: int
      mutable RamOffset: int
      mutable RamEnabled: bool }

let createCartridge (rom: uint8 array) =
    let romBankCount = getRomBankCount rom
    let mbc, hasRam = getMbc rom romBankCount
    let ramBankCount = getRamBankCount rom hasRam

    { Mbc = mbc
      Rom = rom
      RomCount = romBankCount
      RomOffset = 0x4000
      Ram = Array.zeroCreate (ramBankCount * RamBankSize)
      RamCount = ramBankCount
      RamOffset = 0
      RamEnabled = false }

let private updateMbc1BankOffsets (cart: Cartridge) (state: Mbc1State) =
    let romBank = getMbc1RomBank state
    cart.RomOffset <- romBank * RomBankSize

    if state.AdvancedMode && cart.RamCount > 1 then
        cart.RamOffset <- state.RomUpperOrRamReg * RamBankSize
    else
        cart.RamOffset <- 0

let handleCartridgeWrite (cart: Cartridge) address (byte: uint8) =
    let value = int byte

    match cart.Mbc with
    | None -> ()
    | Mbc1 state ->
        if address < 0x2000 then
            if cart.RamCount > 0 then
                cart.RamEnabled <- (value &&& 0xF) = 0xA
        elif address < 0x4000 then
            state.RomReg <- value &&& 0x1F
            updateMbc1BankOffsets cart state
        elif address < 0x6000 then
            state.RomUpperOrRamReg <- value &&& 0b0011
            updateMbc1BankOffsets cart state
        elif address < 0x8000 then
            state.AdvancedMode <- value &&& 0x01 <> 0
            updateMbc1BankOffsets cart state
