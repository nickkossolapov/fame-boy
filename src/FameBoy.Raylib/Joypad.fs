module FameBoy.Raylib.Joypad

open FameBoy.Joypad
open FameBoy.Raylib.RaylibBindings
open Raylib_cs

// Player 1: WASD + J/K/N/B
let getJoypadState () : JoypadState =
    { Up = isKeyDown KeyboardKey.W
      Down = isKeyDown KeyboardKey.S
      Left = isKeyDown KeyboardKey.A
      Right = isKeyDown KeyboardKey.D
      A = isKeyDown KeyboardKey.K
      B = isKeyDown KeyboardKey.J
      Start = isKeyDown KeyboardKey.N
      Select = isKeyDown KeyboardKey.B }

// Player 2: Arrow keys + Home/PgUp/End/PgDn
let getJoypadStateP2 () : JoypadState =
    { Up = isKeyDown KeyboardKey.Up
      Down = isKeyDown KeyboardKey.Down
      Left = isKeyDown KeyboardKey.Left
      Right = isKeyDown KeyboardKey.Right
      A = isKeyDown KeyboardKey.Home
      B = isKeyDown KeyboardKey.PageUp
      Start = isKeyDown KeyboardKey.End
      Select = isKeyDown KeyboardKey.PageDown }
