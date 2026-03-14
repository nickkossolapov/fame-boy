module FameBoy.Test.Mbc5CartTests

open FameBoy.Cartridge
open NUnit.Framework

let private makeRom (bankCount: int) (cartType: byte) (ramSizeCode: byte) =
    let rom = Array.zeroCreate (bankCount * 0x4000)
    rom.[0x147] <- cartType
    rom.[0x149] <- ramSizeCode
    rom

let private makeMbc5Cart bankCount =
    makeRom bankCount 0x19uy 0x00uy |> createCartridge

let private makeMbc5CartWithRam bankCount ramSizeCode =
    makeRom bankCount 0x1Auy ramSizeCode |> createCartridge

let private makeMbc5RumbleCart bankCount =
    makeRom bankCount 0x1Cuy 0x00uy |> createCartridge


[<Test>]
let ``MBC5 cartridge has Mbc5 type`` () =
    let cart = makeMbc5Cart 4

    match cart.Mbc with
    | Mbc5 _ -> ()
    | _ -> Assert.Fail "Expected Mbc5"

[<Test>]
let ``MBC5+RAM allocates correct RAM bank count`` () =
    let cart = makeMbc5CartWithRam 4 0x04uy

    Assert.That(cart.RamCount, Is.EqualTo 16)


[<Test>]
let ``Writing 0x0A enables RAM`` () =
    let cart = makeMbc5CartWithRam 2 0x02uy

    handleCartridgeWrite cart 0x0000 0x0Auy

    Assert.That(cart.RamEnabled, Is.True)

[<Test>]
let ``Writing non-0xA disables RAM`` () =
    let cart = makeMbc5CartWithRam 2 0x02uy
    cart.RamEnabled <- true

    handleCartridgeWrite cart 0x0000 0x00uy

    Assert.That(cart.RamEnabled, Is.False)

[<Test>]
let ``RAM enable is ignored when cartridge has no RAM`` () =
    let cart = makeMbc5Cart 2

    handleCartridgeWrite cart 0x0000 0x0Auy

    Assert.That(cart.RamEnabled, Is.False)


[<Test>]
let ``Initial state points switchable bank at bank 1`` () =
    let cart = makeMbc5Cart 8

    Assert.That(cart.RomOffset, Is.EqualTo 0x4000)

[<Test>]
let ``Selecting ROM bank 5 via low register`` () =
    let cart = makeMbc5Cart 8

    handleCartridgeWrite cart 0x2000 0x05uy

    Assert.That(cart.RomOffset, Is.EqualTo(5 * 0x4000))

[<Test>]
let ``Bank 0 is valid for MBC5`` () =
    let cart = makeMbc5Cart 8

    handleCartridgeWrite cart 0x2000 0x00uy

    Assert.That(cart.RomOffset, Is.EqualTo 0)

[<Test>]
let ``High bit combines with low register for 9-bit bank`` () =
    let cart = makeMbc5Cart 512

    handleCartridgeWrite cart 0x2000 0x05uy
    handleCartridgeWrite cart 0x3000 0x01uy

    Assert.That(cart.RomOffset, Is.EqualTo(0x105 * 0x4000))

[<Test>]
let ``High register only uses bit 0`` () =
    let cart = makeMbc5Cart 512

    handleCartridgeWrite cart 0x2000 0x00uy
    handleCartridgeWrite cart 0x3000 0xFFuy

    Assert.That(cart.RomOffset, Is.EqualTo(0x100 * 0x4000))

[<Test>]
let ``ROM bank wraps via ROM size mask`` () =
    let cart = makeMbc5Cart 4

    handleCartridgeWrite cart 0x2000 0x07uy

    Assert.That(cart.RomOffset, Is.EqualTo(3 * 0x4000))

[<Test>]
let ``Selecting RAM bank 3 sets correct offset`` () =
    let cart = makeMbc5CartWithRam 4 0x03uy

    handleCartridgeWrite cart 0x4000 0x03uy

    Assert.That(cart.RamOffset, Is.EqualTo(3 * 0x2000))

[<Test>]
let ``RAM bank register uses 4 bits`` () =
    let cart = makeMbc5CartWithRam 4 0x04uy

    handleCartridgeWrite cart 0x4000 0xFFuy

    Assert.That(cart.RamOffset, Is.EqualTo(0x0F * 0x2000))

[<Test>]
let ``Rumble cartridge uses only 3 bits for RAM bank`` () =
    let cart = makeRom 4 0x1Duy 0x03uy |> createCartridge

    handleCartridgeWrite cart 0x4000 0xFFuy

    Assert.That(cart.RamOffset, Is.EqualTo(0x07 * 0x2000))

[<Test>]
let ``Writes above 0x6000 are no-ops`` () =
    let cart = makeMbc5Cart 8

    handleCartridgeWrite cart 0x2000 0x05uy
    let romOffset = cart.RomOffset

    handleCartridgeWrite cart 0x6000 0xFFuy
    handleCartridgeWrite cart 0x7000 0xFFuy

    Assert.That(cart.RomOffset, Is.EqualTo romOffset)
