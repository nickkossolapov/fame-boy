module FameBoy.Cpu.Execute

open FameBoy.Cpu.Executors.Bitwise
open FameBoy.Cpu.Executors.Control
open FameBoy.Cpu.Executors.Logic
open FameBoy.Cpu.Executors.Load
open FameBoy.Cpu.Instructions
open FameBoy.Cpu.State

let execute (cpu: Cpu) (instr: DecodedInstruction) =
    cpu.Pc <- cpu.Pc + instr.Length

    match instr.Instruction with
    | Bitwise i -> executeBit cpu i
    | Control i -> executeControl cpu i
    | Load i -> executeLoad cpu i
    | Logic i -> executeLogic cpu i
    | Unknown _ -> ()
