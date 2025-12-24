module FameBoy.Timer

open FameBoy.Cpu.Interrupts
open FameBoy.Hardware
open FameBoy.Memory

[<Literal>]
let private dividerFrequency = cpuFrequency / 16384

let private getTimerFrequency =
    function
    | 0b00uy -> 256
    | 0b01uy -> 4
    | 0b10uy -> 16
    | _ -> 64

type TimerState =
    { mutable HasTimerOverflowed: bool // Interrupt triggered one M-cycle after overflowing
      mutable DividerCount: int
      mutable TimerCount: int }

let private stepTimer (state: TimerState) (memory: Memory) =
    // TODO create an IoManager, so no need to check these on every m-cycle
    let enabled = memory[IoRegisters.Tac] &&& 0b100uy <> 0uy
    let frequency = memory[IoRegisters.Tac] &&& 0b011uy |> getTimerFrequency

    if enabled then
        state.TimerCount <- state.TimerCount + 1

        if state.TimerCount >= frequency then
            state.TimerCount <- 0

            let newTima = memory[IoRegisters.Tima] + 1uy
            memory[IoRegisters.Tima] <- newTima

            if newTima = 0uy then
                state.HasTimerOverflowed <- true
                triggerInterrupt memory InterruptType.Timer

let stepTimers (state: TimerState) (memory: Memory) =
    state.DividerCount <- state.DividerCount + 1

    if state.DividerCount = dividerFrequency then
        state.DividerCount <- 0
        memory.writeIoDirect IoRegisters.Div (memory[IoRegisters.Div] + 1uy)

    if state.HasTimerOverflowed then
        state.HasTimerOverflowed <- false
        memory[IoRegisters.Tima] <- memory[IoRegisters.Tma]
    else
        stepTimer state memory

let createTimer () : TimerState =
    { HasTimerOverflowed = false
      DividerCount = 0
      TimerCount = 0 }
