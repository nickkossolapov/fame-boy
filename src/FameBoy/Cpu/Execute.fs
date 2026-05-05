module FameBoy.Cpu.Execute

open FameBoy.Cpu.Executors.Arithmetic
open FameBoy.Cpu.Executors.Bitwise
open FameBoy.Cpu.Executors.Control
open FameBoy.Cpu.Executors.Logic
open FameBoy.Cpu.Executors.Load
open FameBoy.Cpu.Instructions
open FameBoy.Interrupts
open FameBoy.Cpu.Opcodes
open FameBoy.Cpu.State
open FameBoy.Cpu.Utils
open FameBoy.Hardware
open FameBoy.IoController

let checkForInterrupt (cpu: Cpu) (io: IoController) =
    if cpu.Ime then
        let enable = io.InterruptEnable
        let flag = io.Registers[Io.If]
        let pending = enable &&& flag &&& 0x1Fuy

        if pending <> 0uy then
            getInterruptForPending pending |> ValueSome
        else
            ValueNone
    else
        ValueNone

let serviceInterrupt (cpu: Cpu) (io: IoController) (interrupt: InterruptType) : int =
    cpu.Ime <- false
    cpu.Halted <- false
    io.ClearInterruptFlag interrupt
    pushToStack cpu cpu.Pc
    cpu.Pc <- getVector interrupt

    5 // Interrupts take 5 m-cycles

let execute (cpu: Cpu) (io: IoController) (instr: DecodedInstruction) =
    cpu.Pc <- (cpu.Pc + uint16 instr.Length) &&& 0xFFFFus

    if cpu.EnableImeNextInstr then
        cpu.Ime <- true
        cpu.EnableImeNextInstr <- false

    let condTaken =
        match instr.Instruction with
        | Halt ->
            cpu.Halted <- true
            false
        | Stop ->
            // In CGB mode, STOP with KEY1 bit 0 set triggers a speed switch
            if io.CgbMode && (io.Registers[Io.Key1] &&& 0x01uy <> 0uy) then
                io.DoubleSpeed <- not io.DoubleSpeed
                io.Registers[Io.Key1] <- io.Registers[Io.Key1] &&& 0xFEuy // Clear prepare bit
            else
                cpu.Halted <- true // Halt until button press (like HALT but deeper)
            false
        | Di ->
            cpu.Ime <- false
            cpu.EnableImeNextInstr <- false
            false
        | Ei ->
            cpu.EnableImeNextInstr <- true
            false
        | Nop -> false
        | Arithmetic i ->
            executeArithmetic cpu i
            false
        | Bitwise i ->
            executeBitwise cpu i
            false
        | Control i ->
            let taken = isCondTaken cpu i
            executeControl cpu i
            taken
        | Load i ->
            executeLoad cpu i
            false
        | Logic i ->
            executeLogic cpu i
            false
        | Unknown -> false

    let interruptCycles =
        match checkForInterrupt cpu io with
        | ValueSome i -> serviceInterrupt cpu io i
        | ValueNone -> 0

    let instructionCycles =
        match instr.MCycles with
        | Fixed c -> c
        | Conditional cc -> if condTaken then cc.Met else cc.NotMet

    interruptCycles + instructionCycles

let stepCpu (cpu: Cpu) (io: IoController) =
    if cpu.Halted then
        if ((io.InterruptEnable &&& io.Registers[Io.If]) &&& 0x1Fuy) <> 0uy then
            cpu.Halted <- false

            match checkForInterrupt cpu io with
            | ValueSome i -> serviceInterrupt cpu io i
            | ValueNone -> fetchAndDecode cpu.Memory cpu.Pc |> execute cpu io
        else
            1
    else
        fetchAndDecode cpu.Memory cpu.Pc |> execute cpu io
