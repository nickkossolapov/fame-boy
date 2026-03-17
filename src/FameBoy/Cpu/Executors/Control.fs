module FameBoy.Cpu.Executors.Control

open FameBoy.Cpu.Instructions
open FameBoy.Cpu.Instructions.LoadTypes
open FameBoy.Cpu.State
open FameBoy.Cpu.State.Flags
open FameBoy.Cpu.Utils

let private evaluateCondition (cpu: Cpu) (condition: Condition) =
    match condition with
    | Condition.Zero -> isZero cpu.Flags
    | Condition.NotZero -> not (isZero cpu.Flags)
    | Condition.Carry -> isCarry cpu.Flags
    | Condition.NoCarry -> not (isCarry cpu.Flags)

let isCondTaken (cpu: Cpu) =
    function
    | CallCond(c, _)
    | JpCond(c, _)
    | JrCond(c, _)
    | RetCond c -> evaluateCondition cpu c
    | _ -> false

let executeControl (cpu: Cpu) (instr: ControlInstr) =
    let met c = evaluateCondition cpu c

    let ret () =
        cpu.Pc <- getWordFromMemory cpu.Memory cpu.Sp
        cpu.Sp <- (cpu.Sp + 2us) &&& 0xFFFFus

    match instr with
    | Jp s -> cpu.Pc <- s
    | JpHL -> cpu.Pc <- cpu.Registers.HL
    | JpCond(condition, w) ->
        if met condition then
            cpu.Pc <- w
    | Jr b -> cpu.Pc <- uint16 (int16 cpu.Pc + int16 b)
    | JrCond(condition, b) ->
        if met condition then
            cpu.Pc <- uint16 (int16 cpu.Pc + int16 b)
    | Call s ->
        pushToStack cpu cpu.Pc
        cpu.Pc <- s
    | CallCond(condition, w) ->
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
