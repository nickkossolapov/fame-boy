module FameBoy.Serial

open FameBoy.Cpu.Interrupts
open FameBoy.Hardware
open FameBoy.Memory


let private cyclesPerByte = 128 * 8 // 1024 M-cycles (8192 Hz bit rate, 8 bits)

type SerialState =
    { mutable Counter: int
      mutable IsTransferring: bool }

let createSerial () = { Counter = 0; IsTransferring = false }

let stepSerial (state: SerialState) (memory: Memory) =
    if state.IsTransferring then
        state.Counter <- state.Counter + 1
        
        if state.Counter = cyclesPerByte then
            state.Counter <- 0
            state.IsTransferring <- false

            memory[IoRegisters.Sb] <- 0xFFuy // Not actually transferring data for now, just need the interrupt
            memory[IoRegisters.Sc] <- memory[IoRegisters.Sc] &&& 0b0111_1111uy
            triggerInterrupt memory InterruptType.Serial
    else if (memory[IoRegisters.Sc] &&& 0b1000_0000uy) <> 0uy then
        state.IsTransferring <- true
