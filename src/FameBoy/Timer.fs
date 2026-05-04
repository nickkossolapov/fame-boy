module FameBoy.Timer

open FameBoy.Interrupts
open FameBoy.Hardware
open FameBoy.IoController

[<Literal>]
let private dividerFrequency = cpuFrequency / 16384

let inline private getTimerFrequency x =
    match x with
    | 0b00uy -> 256
    | 0b01uy -> 4
    | 0b10uy -> 16
    | _ -> 64

type TimerState =
    { mutable HasTimerOverflowed: bool // Interrupt triggered one M-cycle after overflowing
      mutable DividerCount: int
      mutable TimerCount: int }

let private stepTim (state: TimerState) (io: IoController) =
    let enabled = io.Registers[Io.Tac] &&& 0b100uy <> 0uy
    let frequency = io.Registers[Io.Tac] &&& 0b011uy |> getTimerFrequency

    if enabled then
        state.TimerCount <- state.TimerCount + 1

        if state.TimerCount >= frequency then
            state.TimerCount <- 0

            let newTima = (io.Registers[Io.Tima] + 1uy) &&& 0xFFuy
            io.Registers[Io.Tima] <- newTima

            if newTima = 0uy then
                state.HasTimerOverflowed <- true
                io.TriggerInterrupt InterruptType.Timer

let stepTimers (state: TimerState) (io: IoController) =
    state.DividerCount <- state.DividerCount + 1

    if state.DividerCount = dividerFrequency then
        state.DividerCount <- 0
        io.Registers[Io.Div] <- (io.Registers[Io.Div] + 1uy) &&& 0xFFuy

    if state.HasTimerOverflowed then
        state.HasTimerOverflowed <- false
        io.Registers[Io.Tima] <- io.Registers[Io.Tma]
    else
        stepTim state io

let createTimer () : TimerState =
    { HasTimerOverflowed = false
      DividerCount = 0
      TimerCount = 0 }
