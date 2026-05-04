module FameBoy.Joypad

open FameBoy.Interrupts

[<Struct>]
type JoypadState =
    { Up: bool
      Down: bool
      Left: bool
      Right: bool
      A: bool
      B: bool
      Start: bool
      Select: bool }

let inline private toBitFlag isPressed bit = if isPressed then 0uy else bit

let inline toJoypadRegisterValue (state: JoypadState) (current: uint8) =
    // Game Boy is active low for joypad inputs
    let selectButtons = (current &&& 0b100000uy) = 0uy
    let selectDPad = (current &&& 0b10000uy) = 0uy

    let joypadFlags =
        match selectButtons, selectDPad with
        | true, false ->
            toBitFlag state.Start 0b1000uy
            ||| toBitFlag state.Select 0b100uy
            ||| toBitFlag state.B 0b10uy
            ||| toBitFlag state.A 0b1uy
        | false, true ->
            toBitFlag state.Down 0b1000uy
            ||| toBitFlag state.Up 0b100uy
            ||| toBitFlag state.Left 0b10uy
            ||| toBitFlag state.Right 0b1uy
        | _ -> 0b1111uy

    (current &&& 0b11110000uy) ||| joypadFlags

let applyJoypadState (state: JoypadState) prevReg triggerInterrupt =
    let nextReg = toJoypadRegisterValue state prevReg

    let interruptTriggered =
        let mask = 0b1111uy
        let highToLow = (prevReg &&& mask) &&& ~~~(nextReg &&& mask)

        highToLow <> 0uy

    if interruptTriggered then
        triggerInterrupt InterruptType.Joypad

    nextReg
