module FameBoy.Timer

open FameBoy.Cpu.Interrupts
open FameBoy.Hardware
open FameBoy.Memory

[<Literal>]
let private dividerFrequency = cpuFrequency / 16384

let private getTimerFrequency =
    function
    | 0b00uy -> 1024
    | 0b01uy -> 16
    | 0b10uy -> 64
    | _ -> 256

type TimerState =
    { mutable HasOverflowed: bool // Interrupt triggered one M-cycle after overflowing
      mutable DividerCount: int
      mutable TimerCount: int }

let private stepTimer (state: TimerState) (memory: Memory) =
    // TODO create an IoManager, so no need to check these on every m-cycle
    let mem = memory[IoRegisters.Tac]
    let enabled = memory[IoRegisters.Tac] &&& 0b100uy <> 0uy
    let frequency = memory[IoRegisters.Tac] &&& 0b011uy |> getTimerFrequency

    if enabled then
        state.TimerCount <- state.TimerCount + 1

        if state.TimerCount >= frequency then
            state.TimerCount <- 0

            if memory[IoRegisters.Tima] = 0xFFuy then
                state.HasOverflowed <- true

            memory[IoRegisters.Tima] <- memory[IoRegisters.Tima] + 1uy

let stepTimers (state: TimerState) (memory: Memory) =
    if state.DividerCount = dividerFrequency then
        state.DividerCount <- 0
        memory.writeIoDirect IoRegisters.Div (memory[IoRegisters.Div] + 1uy)
    else
        state.DividerCount <- state.DividerCount + 1

    if state.HasOverflowed then
        state.HasOverflowed <- false
        triggerInterrupt memory InterruptType.Timer
        memory[IoRegisters.Tima] <- memory[IoRegisters.Tma]
    else
        stepTimer state memory

let createTimer () : TimerState =
    { HasOverflowed = false
      DividerCount = 0
      TimerCount = 0 }
