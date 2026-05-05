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
    let cpu = if io.CgbMode then createCgbCpu memory io else createDmgCpu memory io
    let ppu = createPpu memory io
    let apu = createApu ringBufferSize
    let serial = createSerial ()

    io.ApuRegisters <- apu.Registers

    let applyJoypadState (state: JoypadState) = io.JoypadState <- state

    let mutable lastPpuMode = io.PpuMode
    let mutable normalSpeedDivider = 0

    let stepper () =
        let mCycles = stepCpu cpu io

        match io.DmaRequest with
        | ValueSome startPrefix ->
            io.DmaRequest <- ValueNone
            doDmaTransfer memory startPrefix
        | ValueNone -> ()

        // General-purpose HDMA (transfer all at once when triggered)
        if io.HdmaActive && not io.HdmaHblank then
            doHdmaTransfer memory

        // Timer, serial, and APU all run at normal speed (not affected by double speed).
        // In double speed mode, m-cycles arrive at 2x rate, so step these every other m-cycle.
        if io.DoubleSpeed then
            for i in 1..mCycles do
                if (normalSpeedDivider + i) % 2 = 0 then
                    stepTimers timer io
                    stepSerial serial io
                    stepApu apu
            normalSpeedDivider <- (normalSpeedDivider + mCycles) % 2
        else
            for _ in 1..mCycles do
                stepTimers timer io
                stepSerial serial io
                stepApu apu

        let prevMode = lastPpuMode

        // PPU runs at normal speed: 4 t-cycles per m-cycle in normal, 2 in double speed
        let tCycles = if io.DoubleSpeed then mCycles * 2 else mCycles * 4

        for _ in 1..tCycles do
            stepPpu ppu

        lastPpuMode <- io.PpuMode

        // HBlank HDMA: transfer 16 bytes only on transition into HBlank
        if io.HdmaActive && io.HdmaHblank && io.PpuMode = PpuMode.HBlank && prevMode <> PpuMode.HBlank then
            doHdmaHblankBlock memory

        mCycles

    ppu, apu, serial, io, stepper, applyJoypadState
