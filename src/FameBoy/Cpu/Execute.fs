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
    cpu.Pc <- cpu.Pc + uint16 instr.Length

    match checkForInterrupt cpu with
    | Some i -> serviceInterrupt cpu i
    | None ->
        let condTaken =
            match instr.Instruction with
            | Halt ->
                cpu.Halted <- true
                false
            | Stop -> false // NOP - Not used in any games
            | Di ->
                cpu.Ime <- false
                cpu.EnableImeNextCycle <- false
                false
            | Ei ->
                cpu.EnableImeNextCycle <- true
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

        match instr.MCycles with
        | Fixed c -> c
        | Conditional cc -> if condTaken then cc.Met else cc.NotMet

let stepCpu (cpu: Cpu) =
    let isHalted = cpu.Halted && (cpu.Memory[IoRegisters.Ie] &&& cpu.Memory[IoRegisters.If]) = 0uy

    if isHalted then
        1
    else
        fetchAndDecode cpu.Memory cpu.Pc |> execute cpu
