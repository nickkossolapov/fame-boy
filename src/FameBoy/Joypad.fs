module FameBoy.Joypad

open FameBoy.Hardware
open FameBoy.Memory

type JoypadState = {
    Up: bool
    Down: bool
    Left: bool
    Right: bool
    A: bool
    B: bool
    Start: bool
    Select: bool
}

let inline private toBitFlag isPressed bit =
    if isPressed then 0uy else bit

let toJoypadRegisterValue (state: JoypadState) (current: uint8) =
    // Game Boy is active low for joypad inputs
    let selectButtons =  current &&& 0b10000uy = 0uy
    let selectDPad = current &&& 0b100000uy = 0uy
    
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
    
let applyJoypadState (state: JoypadState) (memory: Memory) =
    let prevReg = memory[Registers.P1]
    let newReg = toJoypadRegisterValue state prevReg
    
    // TODO: handle interrupts
    
    memory[Registers.P1] <- newReg
                
    
    