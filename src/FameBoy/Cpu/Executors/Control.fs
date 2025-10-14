module FameBoy.Cpu.Executors.Control

open FameBoy.Cpu.Instructions
open FameBoy.Cpu.State

let private evaluateCondition (cpu: Cpu) (condition: Condition) =
    match condition with
    | Condition.Zero -> cpu.getFlag Flag.Zero
    | Condition.NotZero -> not (cpu.getFlag Flag.Zero)
    | Condition.Carry -> cpu.getFlag Flag.Carry
    | Condition.NoCarry -> not (cpu.getFlag Flag.Carry)


let executeControl (cpu: Cpu) (instr: ControlInstr) =
    match instr with
    | JrCond (condition, b) ->
        let shouldJump = evaluateCondition cpu condition

        if shouldJump then
            cpu.Pc <- cpu.Pc + int b
