module FameBoy.Cpu.Execute

open FameBoy.Cpu.Instructions
open FameBoy.Cpu.State
open FameBoy.State.Executors.Load

let execute (cpu: Cpu) (instr: DecodedInstruction) =
    cpu.Pc <- cpu.Pc + instr.Length

    match instr.Instruction with
    | Load i -> executeLoad cpu i
    | Unknown -> ()
