module FameBoy.Cpu.Execute

open FameBoy.Cpu.Executors.Arithmetic
open FameBoy.Cpu.Executors.Bitwise
open FameBoy.Cpu.Executors.Control
open FameBoy.Cpu.Executors.Logic
open FameBoy.Cpu.Executors.Load
open FameBoy.Cpu.Instructions
open FameBoy.Cpu.Interrupts
open FameBoy.Cpu.Opcodes
open FameBoy.Cpu.State
open FameBoy.Hardware

let execute (cpu: Cpu) (instr: DecodedInstruction) =
    cpu.Pc <- (cpu.Pc + uint16 instr.Length) &&& 0xFFFFus

    if cpu.EnableImeNextInstr then
        cpu.Ime <- true
        cpu.EnableImeNextInstr <- false

    let condTaken =
        match instr.Instruction with
        | Halt ->
            cpu.Halted <- true
            false
        | Stop -> false // NOP - Not used in any games
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

    let cycles =
        match checkForInterrupt cpu with
        | ValueSome i -> serviceInterrupt cpu i
        | ValueNone ->
            match instr.MCycles with
            | Fixed c -> c
            | Conditional cc -> if condTaken then cc.Met else cc.NotMet

    if cpu.EnableImeNextInstr then
        cpu.Ime <- true
        cpu.EnableImeNextInstr <- false

    cycles

let stepCpu (cpu: Cpu) =
    let isHalted = cpu.Halted && (cpu.Memory[IoRegisters.Ie] &&& cpu.Memory[IoRegisters.If]) = 0uy

    if isHalted then
        1
    else
        fetchAndDecode cpu.Memory cpu.Pc |> execute cpu
