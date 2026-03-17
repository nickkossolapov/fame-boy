module FameBoy.Emulator

open FameBoy.Cpu.Execute
open FameBoy.Graphics.Ppu
open FameBoy.IoController
open FameBoy.Joypad
open FameBoy.Memory
open FameBoy.Serial
open FameBoy.Startup
open FameBoy.Timer


let createEmulator bytes getJoypadState =
    let timer = createTimer ()
    let io = createIoController ()
    let memory = createMemory bytes io
    let cpu = createDmgCpu memory io
    let ppu = createPpu memory io
    let serial = createSerial ()

    let applyJoypadState (state: JoypadState) = io.JoypadState <- state

    let stepper () =
        let cpuCycles = stepCpu cpu io

        match io.DmaRequest with
        | ValueSome startPrefix ->
            io.DmaRequest <- ValueNone
            doDmaTransfer memory startPrefix
        | ValueNone -> ()

        for _ in 1..cpuCycles do
            stepTimers timer io
            stepSerial serial io

        let ppuSteps = cpuCycles * 4

        for _ in 1..ppuSteps do
            stepPpu ppu

        cpuCycles

    ppu, stepper, applyJoypadState
