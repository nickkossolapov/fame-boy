module FameBoy.Cpu.Executors.Logic

open FameBoy.Cpu.Instructions
open FameBoy.Cpu.State

let turnAToBcd (cpu: Cpu) =
    let a = cpu.Registers.A
    let mutable correction = 0x00uy
    let mutable carry = cpu.getFlag Carry

    if cpu.getFlag Subtract then
        if cpu.getFlag HalfCarry then
            correction <- correction ||| 0x06uy

        if carry then
            correction <- correction ||| 0x60uy

        cpu.Registers.A <- a - correction
    else
        if a > 0x99uy || carry then
            correction <- correction ||| 0x60uy
            carry <- true

        if (a &&& 0x0Fuy) > 0x09uy || cpu.getFlag HalfCarry then
            correction <- correction ||| 0x06uy

        cpu.Registers.A <- a + correction

    cpu.setFlags [ Zero, cpu.Registers.A = 0uy; HalfCarry, false; Carry, carry ]

let executeLogic (cpu: Cpu) (instr: LogicInstr) =
    match instr with
    | And bs ->
        let value = bs.GetFrom cpu
        let result = (cpu.Registers.A &&& value)

        cpu.Registers.A <- result
        cpu.setFlags [ Zero, result = 0uy; Subtract, false; HalfCarry, true; Carry, false ]
    | Or bs ->
        let value = bs.GetFrom cpu
        let result = (cpu.Registers.A ||| value)

        cpu.Registers.A <- result
        cpu.setFlags [ Zero, result = 0uy; Subtract, false; HalfCarry, false; Carry, false ]
    | Xor bs ->
        let value = bs.GetFrom cpu
        let result = (cpu.Registers.A ^^^ value)

        cpu.Registers.A <- result
        cpu.setFlags [ Zero, result = 0uy; Subtract, false; HalfCarry, false; Carry, false ]
    | Ccf -> cpu.setFlags [ Subtract, false; HalfCarry, false; Carry, not (cpu.getFlag Carry) ]
    | Scf -> cpu.setFlags [ Subtract, false; HalfCarry, false; Carry, true ]
    | Daa -> turnAToBcd cpu
    | Cpl ->
        cpu.Registers.A <- ~~~cpu.Registers.A
        cpu.setFlags [ Subtract, true; HalfCarry, true ]
