module FameBoy.Emulator

open FameBoy.Apu
open FameBoy.Cpu.Execute
open FameBoy.Ppu
open FameBoy.IoController
open FameBoy.Joypad
open FameBoy.Memory
open FameBoy.Serial
open FameBoy.Startup
open FameBoy.Timer


let createEmulator bytes ringBufferSize getJoypadState =
    let timer = createTimer ()
    let io = createIoController ()
    let memory = createMemory bytes io
    let cpu = createDmgCpu memory io
    let ppu = createPpu memory io
    let apu = createApu ringBufferSize
    let serial = createSerial ()

    let applyJoypadState (state: JoypadState) = io.JoypadState <- state

    let stepper () =
        let mCycles = stepCpu cpu io

        match io.DmaRequest with
        | ValueSome startPrefix ->
            io.DmaRequest <- ValueNone
            doDmaTransfer memory startPrefix
        | ValueNone -> ()

        for _ in 1..mCycles do
            stepTimers timer io
            stepSerial serial io

        // Rest of Game Boy hardware operates at 4x cycles/s of the CPU
        let tCycles = mCycles * 4

        for _ in 1..tCycles do
            stepPpu ppu
            stepApu apu io

        mCycles

    ppu, apu, stepper, applyJoypadState
