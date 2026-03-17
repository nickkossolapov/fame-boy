module FameBoy.Test.JoypadTests

open NUnit.Framework
open FameBoy.Joypad

[<Test>]
let ``toJoypadRegisterValue returns all bits high when neither buttons nor d-pad selected`` () =
    let state =
        { Up = true
          Down = true
          Left = true
          Right = true
          A = true
          B = true
          Start = true
          Select = true }

    let current = 0b00110000uy
    let result = toJoypadRegisterValue state current

    Assert.That(result, Is.EqualTo 0b00111111uy)

[<TestCase(false, false, false, false, 0b00011111uy, TestName = "All buttons pressed")>]
[<TestCase(true, false, false, false, 0b00010111uy, TestName = "Start pressed")>]
[<TestCase(false, true, false, false, 0b00011011uy, TestName = "Select pressed")>]
[<TestCase(false, false, true, false, 0b00011101uy, TestName = "B pressed")>]
[<TestCase(false, false, false, true, 0b00011110uy, TestName = "A pressed")>]
let ``toJoypadRegisterValue reads button state when buttons selected`` (start: bool) (select: bool) (b: bool) (a: bool) (expected: byte) =
    let state =
        { Up = false
          Down = false
          Left = false
          Right = false
          A = a
          B = b
          Start = start
          Select = select }

    let current = 0b00010000uy
    let result = toJoypadRegisterValue state current

    Assert.That(result, Is.EqualTo expected)

[<TestCase(false, false, false, false, 0b00101111uy, TestName = "All d-pad pressed")>]
[<TestCase(false, true, false, false, 0b00100111uy, TestName = "Down pressed")>]
[<TestCase(true, false, false, false, 0b00101011uy, TestName = "Up pressed")>]
[<TestCase(false, false, true, false, 0b00101101uy, TestName = "Left pressed")>]
[<TestCase(false, false, false, true, 0b00101110uy, TestName = "Right pressed")>]
let ``toJoypadRegisterValue reads d-pad state when d-pad selected`` (up: bool) (down: bool) (left: bool) (right: bool) (expected: byte) =
    let state =
        { Up = up
          Down = down
          Left = left
          Right = right
          A = false
          B = false
          Start = false
          Select = false }

    let current = 0b00100000uy
    let result = toJoypadRegisterValue state current

    Assert.That(result, Is.EqualTo expected)

[<Test>]
let ``toJoypadRegisterValue preserves upper nibble`` () =
    let state =
        { Up = false
          Down = false
          Left = false
          Right = false
          A = false
          B = false
          Start = false
          Select = false }

    let current = 0b11100000uy
    let result = toJoypadRegisterValue state current

    Assert.That(result, Is.EqualTo 0b11101111uy)

[<Test>]
let ``toJoypadRegisterValue returns all high when both selectors are low`` () =
    let state =
        { Up = false
          Down = false
          Left = false
          Right = false
          A = false
          B = false
          Start = false
          Select = false }

    let current = 0b11000000uy
    let result = toJoypadRegisterValue state current

    Assert.That(result, Is.EqualTo 0b11001111uy)
