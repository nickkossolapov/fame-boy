module FameBoy.Cartridge

open FameBoy.Cartridges
open FameBoy.Hardware

type Mbc =
    | None
    | Mbc1 of Mbc1Cart.State
    | Mbc3 of Mbc3Cart.State
    | Mbc5 of Mbc5Cart.State

module private Headers =
    // https://gbdev.io/pandocs/The_Cartridge_Header.html#the-cartridge-header

    let getRomBankCount (rom: uint8 array) = rom.Length / CartRomBankSize

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
        | 0x01uy -> Mbc1Cart.createState romSize |> Mbc1, false
        | 0x02uy
        | 0x03uy -> Mbc1Cart.createState romSize |> Mbc1, true
        | 0x08uy
        | 0x09uy -> None, true
        | 0x0Fuy -> Mbc3Cart.createState romSize true |> Mbc3, false
        | 0x10uy -> Mbc3Cart.createState romSize true |> Mbc3, true
        | 0x11uy -> Mbc3Cart.createState romSize false |> Mbc3, false
        | 0x12uy
        | 0x13uy -> Mbc3Cart.createState romSize false |> Mbc3, true
        | 0x19uy -> Mbc5Cart.createState romSize false |> Mbc5, false
        | 0x1Auy
        | 0x1Buy -> Mbc5Cart.createState romSize false |> Mbc5, true
        | 0x1Cuy -> Mbc5Cart.createState romSize true |> Mbc5, false
        | 0x1Duy
        | 0x1Euy -> Mbc5Cart.createState romSize true |> Mbc5, true
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
      Ram = Array.zeroCreate (ramBankCount * CartRamBankSize)
      RamCount = ramBankCount
      RamOffset = 0
      RamEnabled = false }

let handleCartridgeWrite (cart: Cartridge) address (value: uint8) =
    let value = int value

    match cart.Mbc with
    | None -> ()
    | Mbc1 state ->
        if address < 0x2000 then
            if cart.RamCount > 0 then
                cart.RamEnabled <- (value &&& 0xF) = 0xA
        else
            let struct (romOffset, ramOffset) =
                Mbc1Cart.handleCartWrite state cart.RamCount address value

            cart.RomOffset <- romOffset
            cart.RamOffset <- ramOffset
    | Mbc3 state ->
        if address < 0x2000 then
            cart.RamEnabled <- (value &&& 0xF) = 0xA
        else
            let struct (romOffset, ramOffset) =
                Mbc3Cart.handleCartWrite state address value

            cart.RomOffset <- romOffset
            cart.RamOffset <- ramOffset
    | Mbc5 state ->
        if address < 0x2000 then
            if cart.RamCount > 0 then
                cart.RamEnabled <- (value &&& 0xF) = 0xA
        else
            let struct (romOffset, ramOffset) =
                Mbc5Cart.handleCartWrite state address value

            cart.RomOffset <- romOffset
            cart.RamOffset <- ramOffset

let readCartRam (cart: Cartridge) address =
    if not cart.RamEnabled then
        0xFFuy
    else
        match cart.Mbc with
        | Mbc3 state when Mbc3Cart.isRtcSelected state ->
            Mbc3Cart.readRtcRegister state
        | _ ->
            cart.Ram[cart.RamOffset + address - 0xA000]

let writeCartRam (cart: Cartridge) address (value: uint8) =
    if cart.RamEnabled then
        match cart.Mbc with
        | Mbc3 state when Mbc3Cart.isRtcSelected state ->
            Mbc3Cart.writeRtcRegister state value
        | _ ->
            cart.Ram[cart.RamOffset + address - 0xA000] <- value
