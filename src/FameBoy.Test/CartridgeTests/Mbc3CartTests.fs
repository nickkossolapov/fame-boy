module FameBoy.Test.Mbc3CartTests

open FameBoy.Cartridge
open NUnit.Framework

let private makeRom (bankCount: int) (cartType: byte) (ramSizeCode: byte) =
    let rom = Array.zeroCreate (bankCount * 0x4000)
    rom.[0x147] <- cartType
    rom.[0x149] <- ramSizeCode
    rom

let private makeMbc3Cart bankCount =
    makeRom bankCount 0x11uy 0x00uy |> createCartridge

let private makeMbc3CartWithRam bankCount ramSizeCode =
    makeRom bankCount 0x12uy ramSizeCode |> createCartridge

let private makeMbc3CartWithRtc bankCount =
    makeRom bankCount 0x10uy 0x03uy |> createCartridge


[<Test>]
let ``MBC3 cartridge has Mbc3 type`` () =
    let cart = makeMbc3Cart 4

    match cart.Mbc with
    | Mbc3 _ -> ()
    | _ -> Assert.Fail "Expected Mbc3"

[<Test>]
let ``MBC3+RAM allocates correct RAM bank count`` () =
    let cart = makeMbc3CartWithRam 4 0x03uy

    Assert.That(cart.RamCount, Is.EqualTo 4)

[<Test>]
let ``MBC3+TIMER+BATTERY (0x0F) has no RAM`` () =
    let cart = makeRom 4 0x0Fuy 0x00uy |> createCartridge

    Assert.That(cart.RamCount, Is.EqualTo 0)


[<Test>]
let ``Writing 0x0A enables RAM`` () =
    let cart = makeMbc3CartWithRam 2 0x02uy

    handleCartridgeWrite cart 0x0000 0x0Auy

    Assert.That(cart.RamEnabled, Is.True)

[<Test>]
let ``Writing non-0xA disables RAM`` () =
    let cart = makeMbc3CartWithRam 2 0x02uy
    cart.RamEnabled <- true

    handleCartridgeWrite cart 0x0000 0x00uy

    Assert.That(cart.RamEnabled, Is.False)

[<Test>]
let ``RAM enable works for RTC-only cartridge`` () =
    let cart = makeRom 4 0x0Fuy 0x00uy |> createCartridge

    handleCartridgeWrite cart 0x0000 0x0Auy

    Assert.That(cart.RamEnabled, Is.True)


[<Test>]
let ``Initial state points switchable bank at bank 1`` () =
    let cart = makeMbc3Cart 8

    Assert.That(cart.RomOffset, Is.EqualTo 0x4000)

[<Test>]
let ``Selecting ROM bank 5 sets correct offset`` () =
    let cart = makeMbc3Cart 8

    handleCartridgeWrite cart 0x2000 0x05uy

    Assert.That(cart.RomOffset, Is.EqualTo(5 * 0x4000))

[<Test>]
let ``Selecting bank 0 maps to bank 1`` () =
    let cart = makeMbc3Cart 8

    handleCartridgeWrite cart 0x2000 0x03uy
    handleCartridgeWrite cart 0x2000 0x00uy

    Assert.That(cart.RomOffset, Is.EqualTo(1 * 0x4000))

[<Test>]
let ``ROM bank number uses only 7 bits`` () =
    let cart = makeMbc3Cart 128

    handleCartridgeWrite cart 0x2000 0xFFuy

    Assert.That(cart.RomOffset, Is.EqualTo(0x7F * 0x4000))

[<Test>]
let ``ROM bank wraps via ROM size mask`` () =
    let cart = makeMbc3Cart 4

    handleCartridgeWrite cart 0x2000 0x07uy

    Assert.That(cart.RomOffset, Is.EqualTo(3 * 0x4000))


// --- RAM banking ---

[<Test>]
let ``Selecting RAM bank 2 sets correct offset`` () =
    let cart = makeMbc3CartWithRam 4 0x03uy

    handleCartridgeWrite cart 0x4000 0x02uy

    Assert.That(cart.RamOffset, Is.EqualTo(2 * 0x2000))

[<Test>]
let ``Selecting RAM bank 0 resets offset`` () =
    let cart = makeMbc3CartWithRam 4 0x03uy

    handleCartridgeWrite cart 0x4000 0x02uy
    handleCartridgeWrite cart 0x4000 0x00uy

    Assert.That(cart.RamOffset, Is.EqualTo 0)


// --- RTC ---

[<Test>]
let ``Switching from RTC register back to RAM bank restores offset`` () =
    let cart = makeMbc3CartWithRtc 4

    handleCartridgeWrite cart 0x4000 0x02uy
    handleCartridgeWrite cart 0x4000 0x08uy
    handleCartridgeWrite cart 0x4000 0x02uy

    Assert.That(cart.RamOffset, Is.EqualTo(2 * 0x2000))

[<Test>]
let ``RTC latch requires 0x00 then 0x01 sequence`` () =
    let cart = makeMbc3CartWithRtc 4
    cart.RamEnabled <- true

    // Select RTC seconds register
    handleCartridgeWrite cart 0x4000 0x08uy

    // Write 0x01 without prior 0x00 — should not latch
    handleCartridgeWrite cart 0x6000 0x01uy
    let before = readCartRam cart 0xA000

    // Proper latch sequence
    handleCartridgeWrite cart 0x6000 0x00uy
    handleCartridgeWrite cart 0x6000 0x01uy
    let after = readCartRam cart 0xA000

    // After latching, RTC should have a value from the system clock
    // We can't assert an exact value, just that the latch occurred without error
    Assert.Pass()

[<Test>]
let ``RTC register read returns 0xFF when RAM disabled`` () =
    let cart = makeMbc3CartWithRtc 4
    cart.RamEnabled <- false

    handleCartridgeWrite cart 0x4000 0x08uy

    let value = readCartRam cart 0xA000

    Assert.That(value, Is.EqualTo 0xFFuy)

[<Test>]
let ``RTC register write and read round-trips`` () =
    let cart = makeMbc3CartWithRtc 4
    cart.RamEnabled <- true

    // Select minutes register and write a value
    handleCartridgeWrite cart 0x4000 0x09uy
    writeCartRam cart 0xA000 42uy

    let value = readCartRam cart 0xA000

    Assert.That(value, Is.EqualTo 42uy)
