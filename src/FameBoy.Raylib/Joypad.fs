module FameBoy.Raylib.Joypad

open FameBoy.Joypad
open FameBoy.Raylib.RaylibBindings
open Raylib_cs

// QWERTY
// let joypadMapping =
//     {| Up = KeyboardKey.W
//        Down = KeyboardKey.S
//        Left = KeyboardKey.A
//        Right = KeyboardKey.D
//        A = KeyboardKey.J
//        B = KeyboardKey.K
//        Start = KeyboardKey.N
//        Select = KeyboardKey.M |}

// Colemak-dh
let joypadMapping =
    {| Up = KeyboardKey.F
       Down = KeyboardKey.S
       Left = KeyboardKey.R
       Right = KeyboardKey.T
       A = KeyboardKey.N
       B = KeyboardKey.E
       Start = KeyboardKey.H
       Select = KeyboardKey.K |}

let getJoypadState () : JoypadState =
    { Up = isKeyDown joypadMapping.Up
      Down = isKeyDown joypadMapping.Down
      Left = isKeyDown joypadMapping.Left
      Right = isKeyDown joypadMapping.Right
      A = isKeyDown joypadMapping.A
      B = isKeyDown joypadMapping.B
      Start = isKeyDown joypadMapping.Start
      Select = isKeyDown joypadMapping.Select }
