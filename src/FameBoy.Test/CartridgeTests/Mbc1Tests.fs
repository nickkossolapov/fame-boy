module FameBoy.Test.Mbc1Tests

open FameBoy.Cartridge
open FameBoy.Cartridge.Mbcs
open NUnit.Framework

let private makeRom (bankCount: int) (cartType: byte) (ramSizeCode: byte) =
    let rom = Array.zeroCreate (bankCount * 0x4000)
    rom.[0x147] <- cartType
    rom.[0x149] <- ramSizeCode
    rom

let private makeMbc1Cart bankCount =
    makeRom bankCount 0x01uy 0x00uy |> createCartridge

let private makeMbc1CartWithRam bankCount ramSizeCode =
    makeRom bankCount 0x02uy ramSizeCode |> createCartridge


[<Test>]
let ``ROM-only cartridge has Mbc None`` () =
    let cart = makeRom 2 0x00uy 0x00uy |> createCartridge

    Assert.That(cart.Mbc, Is.EqualTo Mbc.None)

[<Test>]
let ``MBC1+RAM cartridge allocates correct RAM bank count`` () =
    let cart = makeRom 4 0x03uy 0x03uy |> createCartridge

    Assert.That(cart.RamCount, Is.EqualTo 4)

[<Test>]
let ``ROM bank count derived from ROM size`` () =
    let cart = makeRom 8 0x00uy 0x00uy |> createCartridge

    Assert.That(cart.RomCount, Is.EqualTo 8)

[<TestCase(0x00uy, 1)>]
[<TestCase(0x01uy, 1)>]
[<TestCase(0x02uy, 1)>]
[<TestCase(0x03uy, 4)>]
[<TestCase(0x04uy, 16)>]
[<TestCase(0x05uy, 8)>]
let ``createCartridge parses RAM bank count from header`` (ramSizeCode: byte) (expectedBanks: int) =
    let rom = makeRom 2 0x02uy ramSizeCode
    let cart = createCartridge rom

    Assert.That(cart.RamCount, Is.EqualTo expectedBanks)

[<Test>]
let ``Initial state points switchable bank at bank 1`` () =
    let cart = makeMbc1Cart 4

    Assert.That(cart.RomOffset, Is.EqualTo 0x4000)
    Assert.That(cart.RamEnabled, Is.False)


[<Test>]
let ``Writing 0x0A to RAM enable region enables RAM`` () =
    let cart = makeMbc1CartWithRam 2 0x02uy
    cart.RamEnabled <- false

    handleCartridgeWrite cart 0x0000 0x0Auy

    Assert.That(cart.RamEnabled, Is.True)

[<Test>]
let ``Any value with low nibble not 0xA disables RAM`` () =
    let cart = makeMbc1CartWithRam 2 0x02uy
    cart.RamEnabled <- true

    handleCartridgeWrite cart 0x0000 0x00uy

    Assert.That(cart.RamEnabled, Is.False)

[<Test>]
let ``RAM enable checks only low nibble`` () =
    let cart = makeMbc1CartWithRam 2 0x02uy
    cart.RamEnabled <- false

    handleCartridgeWrite cart 0x0000 0xFAuy

    Assert.That(cart.RamEnabled, Is.True)

[<Test>]
let ``RAM enable is ignored when cartridge has no RAM banks`` () =
    let cart = makeMbc1Cart 2

    handleCartridgeWrite cart 0x0000 0x0Auy

    Assert.That(cart.RamEnabled, Is.False)


[<Test>]
let ``Selecting bank 0 maps to bank 1 instead`` () =
    let cart = makeMbc1Cart 32

    handleCartridgeWrite cart 0x2000 0x03uy
    handleCartridgeWrite cart 0x2000 0x00uy

    Assert.That(cart.RomOffset, Is.EqualTo(1 * 0x4000))

[<Test>]
let ``Selecting bank 3 on a 32-bank cart sets correct offset`` () =
    let cart = makeMbc1Cart 32

    handleCartridgeWrite cart 0x2000 0x03uy

    Assert.That(cart.RomOffset, Is.EqualTo(3 * 0x4000))

[<Test>]
let ``Bank number wraps via ROM size mask`` () =
    let cart = makeMbc1Cart 4

    handleCartridgeWrite cart 0x2000 0x07uy

    Assert.That(cart.RomOffset, Is.EqualTo(3 * 0x4000))

[<Test>]
let ``Only low 5 bits of write are used for bank number`` () =
    let cart = makeMbc1Cart 32

    handleCartridgeWrite cart 0x2000 0xE5uy

    Assert.That(cart.RomOffset, Is.EqualTo(5 * 0x4000))

[<Test>]
let ``Upper bits combine with low register for high bank selection`` () =
    let cart = makeMbc1Cart 128
    handleCartridgeWrite cart 0x2000 0x03uy
    handleCartridgeWrite cart 0x4000 0x02uy
    Assert.That(cart.RomOffset, Is.EqualTo(67 * 0x4000))

[<Test>]
let ``Upper register is masked to 2 bits`` () =
    let cart = makeMbc1Cart 128

    handleCartridgeWrite cart 0x2000 0x01uy
    handleCartridgeWrite cart 0x4000 0xFFuy

    Assert.That(cart.RomOffset, Is.EqualTo(97 * 0x4000))


[<Test>]
let ``Advanced mode switches RAM bank on multi-bank RAM cart`` () =
    let cart = makeMbc1CartWithRam 64 0x03uy

    handleCartridgeWrite cart 0x4000 0x02uy
    handleCartridgeWrite cart 0x6000 0x01uy

    Assert.That(cart.RamOffset, Is.EqualTo(2 * 0x2000))

[<Test>]
let ``Leaving advanced mode resets RAM offset`` () =
    let cart = makeMbc1CartWithRam 128 0x03uy

    handleCartridgeWrite cart 0x4000 0x02uy
    handleCartridgeWrite cart 0x6000 0x01uy
    handleCartridgeWrite cart 0x6000 0x00uy

    Assert.That(cart.RamOffset, Is.EqualTo 0)


[<Test>]
let ``Writes to ROM-only cartridge are no-ops`` () =
    let cart = makeRom 2 0x00uy 0x00uy |> createCartridge
    let romOffset = cart.RomOffset
    let ramEnabled = cart.RamEnabled

    handleCartridgeWrite cart 0x0000 0x0Auy
    handleCartridgeWrite cart 0x2000 0x05uy
    handleCartridgeWrite cart 0x6000 0x01uy

    Assert.That(cart.RomOffset, Is.EqualTo romOffset)
    Assert.That(cart.RamEnabled, Is.EqualTo ramEnabled)

[<TestCase(0x01uy, 33)>]
[<TestCase(0x02uy, 65)>]
[<TestCase(0x03uy, 97)>]
let ``Zero low-register translation applies even when high-register is set`` (upperReg: byte) (expectedBank: int) =
    let cart = makeMbc1Cart 128

    handleCartridgeWrite cart 0x4000 upperReg
    handleCartridgeWrite cart 0x2000 0x00uy

    Assert.That(cart.RomOffset, Is.EqualTo(expectedBank * 0x4000))

[<Test>]
let ``Simple mode upper register does not affect RAM offset`` () =
    let cart = makeMbc1CartWithRam 64 0x03uy

    handleCartridgeWrite cart 0x4000 0x02uy

    Assert.That(cart.RamOffset, Is.EqualTo 0)

[<Test>]
let ``RAM bank number wraps via RAM size mask`` () =
    let cart = makeMbc1CartWithRam 128 0x03uy

    handleCartridgeWrite cart 0x4000 0x03uy
    handleCartridgeWrite cart 0x6000 0x01uy

    Assert.That(cart.RamOffset, Is.EqualTo(3 * 0x2000))
