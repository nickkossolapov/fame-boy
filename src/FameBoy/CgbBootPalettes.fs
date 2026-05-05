module FameBoy.CgbBootPalettes

// CGB boot ROM assigns color palettes to DMG games based on the title checksum
// Reference: https://gbdev.io/pandocs/Power_Up_Sequence.html#compatibility-palettes

/// Compute the title checksum used by the CGB boot ROM
let computeTitleChecksum (rom: uint8 array) =
    let mutable checksum = 0uy

    for i in 0x134..0x143 do
        checksum <- checksum + rom[i]

    checksum

/// RGB555 color type used by CGB palettes
let private rgb555 r g b =
    let lo = (r &&& 0x1F) ||| ((g &&& 0x07) <<< 5)
    let hi = ((g >>> 3) &&& 0x03) ||| ((b &&& 0x1F) <<< 2)
    (uint8 lo, uint8 hi)

/// Predefined palette sets used by the CGB boot ROM
/// Each set contains (BG palette, OBJ0 palette, OBJ1 palette)
/// Each palette is 4 colors as (low byte, high byte) pairs in RGB555

// Common palettes - based on CGB boot ROM reverse engineering
let private white = rgb555 0x1F 0x1F 0x1F
let private lightGray = rgb555 0x15 0x15 0x15
let private darkGray = rgb555 0x0A 0x0A 0x0A
let private black = rgb555 0x00 0x00 0x00

let private grayscale = [| white; lightGray; darkGray; black |]

// Classic green (like DMG)
let private greenWhite = rgb555 0x1F 0x1F 0x0F
let private greenLight = rgb555 0x15 0x1C 0x05
let private greenDark = rgb555 0x0A 0x12 0x02
let private greenBlack = rgb555 0x02 0x04 0x00

let private classicGreen = [| greenWhite; greenLight; greenDark; greenBlack |]

// Brown/red palette (used by many games)
let private brownWhite = rgb555 0x1F 0x1F 0x1F
let private brownLight = rgb555 0x1F 0x1A 0x09
let private brownDark = rgb555 0x1B 0x05 0x00
let private brownBlack = rgb555 0x00 0x00 0x00

let private brownPalette = [| brownWhite; brownLight; brownDark; brownBlack |]

// Blue palette
let private blueWhite = rgb555 0x1F 0x1F 0x1F
let private blueLight = rgb555 0x0F 0x1B 0x1F
let private blueDark = rgb555 0x00 0x0A 0x1F
let private blueBlack = rgb555 0x00 0x00 0x00

let private bluePalette = [| blueWhite; blueLight; blueDark; blueBlack |]

// Pastel palette
let private pastelWhite = rgb555 0x1F 0x1F 0x1F
let private pastelLight = rgb555 0x1F 0x1F 0x00
let private pastelDark = rgb555 0x1F 0x00 0x00
let private pastelBlack = rgb555 0x00 0x00 0x00

let private pastelPalette = [| pastelWhite; pastelLight; pastelDark; pastelBlack |]

// Red palette
let private redWhite = rgb555 0x1F 0x1F 0x1F
let private redLight = rgb555 0x1F 0x10 0x10
let private redDark = rgb555 0x1B 0x00 0x00
let private redBlack = rgb555 0x00 0x00 0x00

let private redPalette = [| redWhite; redLight; redDark; redBlack |]

// Dark blue palette
let private dkBlueWhite = rgb555 0x1F 0x1F 0x1F
let private dkBlueLight = rgb555 0x12 0x12 0x1F
let private dkBlueDark = rgb555 0x05 0x05 0x14
let private dkBlueBlack = rgb555 0x00 0x00 0x00

let private darkBluePalette = [| dkBlueWhite; dkBlueLight; dkBlueDark; dkBlueBlack |]

// Orange palette
let private orangeWhite = rgb555 0x1F 0x1F 0x1F
let private orangeLight = rgb555 0x1F 0x18 0x05
let private orangeDark = rgb555 0x1A 0x08 0x00
let private orangeBlack = rgb555 0x00 0x00 0x00

let private orangePalette = [| orangeWhite; orangeLight; orangeDark; orangeBlack |]

/// Map title checksum to palette assignment
/// Returns (bgPalette, obj0Palette, obj1Palette)
let getPalettesForChecksum (checksum: uint8) =
    // This is a simplified mapping - real CGB boot ROM has a more complex table
    // with additional checks on specific title bytes
    match checksum with
    | 0x00uy -> (grayscale, grayscale, grayscale)
    | 0x01uy -> (brownPalette, redPalette, darkBluePalette)
    | 0x0Duy -> (bluePalette, bluePalette, bluePalette) // Pokemon Blue
    | 0x10uy -> (redPalette, redPalette, redPalette)
    | 0x14uy -> (brownPalette, brownPalette, brownPalette)
    | 0x15uy -> (classicGreen, classicGreen, classicGreen)
    | 0x16uy -> (brownPalette, darkBluePalette, orangePalette)
    | 0x17uy -> (brownPalette, brownPalette, brownPalette)
    | 0x19uy -> (brownPalette, brownPalette, brownPalette)
    | 0x1Duy -> (brownPalette, brownPalette, brownPalette)
    | 0x27uy -> (pastelPalette, pastelPalette, pastelPalette)
    | 0x28uy -> (redPalette, bluePalette, classicGreen)
    | 0x29uy -> (brownPalette, brownPalette, brownPalette)
    | 0x34uy -> (brownPalette, brownPalette, brownPalette)
    | 0x36uy -> (brownPalette, brownPalette, brownPalette)
    | 0x39uy -> (brownPalette, brownPalette, brownPalette)
    | 0x43uy -> (brownPalette, brownPalette, brownPalette)
    | 0x46uy -> (bluePalette, bluePalette, bluePalette)
    | 0x4Euy -> (brownPalette, brownPalette, brownPalette)
    | 0x58uy -> (redPalette, redPalette, redPalette)
    | 0x59uy -> (brownPalette, brownPalette, brownPalette)
    | 0x5Duy -> (brownPalette, brownPalette, brownPalette)
    | 0x61uy -> (brownPalette, brownPalette, brownPalette)
    | 0x66uy -> (brownPalette, brownPalette, brownPalette) // Tetris
    | 0x67uy -> (brownPalette, brownPalette, brownPalette)
    | 0x69uy -> (classicGreen, classicGreen, classicGreen)
    | 0x6Auy -> (classicGreen, classicGreen, classicGreen)
    | 0x6Buy -> (classicGreen, classicGreen, classicGreen)
    | 0x70uy -> (brownPalette, brownPalette, brownPalette)
    | 0x71uy -> (brownPalette, brownPalette, brownPalette)
    | 0x86uy -> (pastelPalette, pastelPalette, pastelPalette)
    | 0x88uy -> (bluePalette, bluePalette, bluePalette)
    | 0x8Cuy -> (brownPalette, brownPalette, brownPalette)
    | 0x92uy -> (orangePalette, orangePalette, orangePalette)
    | 0x95uy -> (brownPalette, brownPalette, brownPalette)
    | 0x97uy -> (brownPalette, brownPalette, brownPalette)
    | 0x99uy -> (brownPalette, brownPalette, brownPalette)
    | 0x9Auy -> (brownPalette, brownPalette, brownPalette)
    | 0x9Duy -> (brownPalette, brownPalette, brownPalette)
    | 0xA2uy -> (brownPalette, brownPalette, brownPalette)
    | 0xA5uy -> (redPalette, redPalette, redPalette) // Pokemon Red
    | 0xAAuy -> (brownPalette, brownPalette, brownPalette)
    | 0xB3uy -> (brownPalette, brownPalette, brownPalette)
    | 0xBFuy -> (darkBluePalette, darkBluePalette, darkBluePalette)
    | 0xC6uy -> (brownPalette, brownPalette, brownPalette)
    | 0xCEuy -> (brownPalette, brownPalette, brownPalette)
    | 0xD1uy -> (brownPalette, brownPalette, brownPalette)
    | 0xDBuy -> (brownPalette, brownPalette, brownPalette)
    | 0xE0uy -> (brownPalette, brownPalette, brownPalette)
    | 0xE8uy -> (brownPalette, brownPalette, brownPalette)
    | 0xF0uy -> (brownPalette, brownPalette, brownPalette)
    | 0xF1uy -> (classicGreen, classicGreen, classicGreen)
    | 0xF2uy -> (brownPalette, brownPalette, brownPalette)
    | 0xFFuy -> (brownPalette, brownPalette, brownPalette)
    | _ -> (grayscale, grayscale, grayscale) // Default: neutral grayscale

/// Apply a palette array (4 colors) to CGB palette RAM starting at given palette number
let private writePalette (ram: uint8 array) paletteNum (colors: (uint8 * uint8) array) =
    for i in 0..3 do
        let lo, hi = colors[i]
        let offset = paletteNum * 8 + i * 2
        ram[offset] <- lo
        ram[offset + 1] <- hi

/// Initialize CGB palette RAM for a DMG game based on its title checksum
let applyCompatibilityPalettes (rom: uint8 array) (bgPaletteRam: uint8 array) (objPaletteRam: uint8 array) =
    let checksum = computeTitleChecksum rom
    let bgPal, obj0Pal, obj1Pal = getPalettesForChecksum checksum

    // BG palette 0 is used for background
    writePalette bgPaletteRam 0 bgPal

    // OBJ palette 0 and 1 are used for sprites
    writePalette objPaletteRam 0 obj0Pal
    writePalette objPaletteRam 1 obj1Pal
