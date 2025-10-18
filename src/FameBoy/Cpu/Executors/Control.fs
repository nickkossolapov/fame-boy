module FameBoy.Cpu.Executors.Control

open FameBoy.Cpu.Instructions
open FameBoy.Cpu.State
open FameBoy.Cpu.Utils

let private evaluateCondition (cpu: Cpu) (condition: Condition) =
    match condition with
    | Condition.Zero -> cpu.getFlag Flag.Zero
    | Condition.NotZero -> not (cpu.getFlag Flag.Zero)
    | Condition.Carry -> cpu.getFlag Flag.Carry
    | Condition.NoCarry -> not (cpu.getFlag Flag.Carry)

let executeControl (cpu: Cpu) (instr: ControlInstr) =
    let met c = evaluateCondition cpu c

    let ret () =
        cpu.Pc <- getWordFromMemory cpu.Memory cpu.Sp
        cpu.Sp <- cpu.Sp + 2us

    match instr with
    | Jp w -> cpu.Pc <- w
    | JpHL -> cpu.Pc <- cpu.Registers.HL
    | JpCond (condition, w) ->
        if met condition then
            cpu.Pc <- w
    | Jr b -> cpu.Pc <- uint16 (int16 cpu.Pc + int16 b)
    | JrCond (condition, b) ->
        if met condition then
            cpu.Pc <- uint16 (int16 cpu.Pc + int16 b)
    | Call w ->
        pushToStack cpu cpu.Pc
        cpu.Pc <- w
    | CallCond (condition, w) ->
        if met condition then
            pushToStack cpu cpu.Pc
            cpu.Pc <- w
    | Ret -> ret ()
    | RetCond condition ->
        if met condition then
            ret ()
    | Reti ->
        cpu.Ime <- true
        ret ()
    | Rst b ->
        pushToStack cpu cpu.Pc
        cpu.Pc <- uint16 b
