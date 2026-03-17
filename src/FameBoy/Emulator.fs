module FameBoy.Emulator

open FameBoy.Cpu.Execute
open FameBoy.Graphics.Ppu
open FameBoy.Joypad
open FameBoy.Memory
open FameBoy.Serial
open FameBoy.Startup
open FameBoy.Timer


let createEmulator bytes getJoypadState =
    let timer = createTimer ()
    let memory = createMemory bytes
    let cpu = createDmgCpu memory
    let ppu = createPpu memory
    let serial = createSerial ()

    let stepper () =
        // TODO Don't apply on every instruction. Modify memory to resolve joypad state on read, and handle interrupts
        applyJoypadState (getJoypadState ()) memory
        let cpuCycles = stepCpu cpu

        for _ in 1..cpuCycles do
            stepTimers timer memory
            stepSerial serial memory

        let ppuSteps = cpuCycles * 4

        for _ in 1..ppuSteps do
            stepPpu ppu

        cpuCycles

    ppu.Framebuffer, memory, stepper
