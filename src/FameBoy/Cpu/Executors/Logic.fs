module FameBoy.Cpu.Executors.Logic

open FameBoy.Cpu.Instructions
open FameBoy.Cpu.State
open FameBoy.Cpu.State.Flags

let turnAToBcd (cpu: Cpu) =
    let a = cpu.Registers.A
    let mutable correction = 0x00uy
    let mutable c = isCarry cpu.Flags

    if isSub cpu.Flags then
        if isHalf cpu.Flags then
            correction <- correction ||| 0x06uy

        if c then
            correction <- correction ||| 0x60uy

        cpu.Registers.A <- (a - correction) &&& 0xFFuy
    else
        if a > 0x99uy || c then
            correction <- correction ||| 0x60uy
            c <- true

        if (a &&& 0x0Fuy) > 0x09uy || isHalf cpu.Flags then
            correction <- correction ||| 0x06uy

        cpu.Registers.A <- (a + correction) &&& 0xFFuy

    cpu.Flags <- cpu.Flags |> setZ (cpu.Registers.A = 0uy) |> setH false |> setC c

let executeLogic (cpu: Cpu) (instr: LogicInstr) =
    match instr with
    | And source ->
        let value = source.GetFrom cpu
        let result = (cpu.Registers.A &&& value)

        cpu.Registers.A <- result
        cpu.Flags <- cpu.Flags |> setZ (result = 0uy) |> setN false |> setH true |> setC false
    | Or source ->
        let value = source.GetFrom cpu
        let result = (cpu.Registers.A ||| value)

        cpu.Registers.A <- result
        cpu.Flags <- cpu.Flags |> setZ (result = 0uy) |> setN false |> setH false |> setC false
    | Xor source ->
        let value = source.GetFrom cpu
        let result = (cpu.Registers.A ^^^ value)

        cpu.Registers.A <- result
        cpu.Flags <- cpu.Flags |> setZ (result = 0uy) |> setN false |> setH false |> setC false
    | Ccf -> cpu.Flags <- cpu.Flags |> setN false |> setH false |> setC (not (isCarry cpu.Flags))
    | Scf -> cpu.Flags <- cpu.Flags |> setN false |> setH false |> setC true
    | Daa -> turnAToBcd cpu
    | Cpl ->
        cpu.Registers.A <- ~~~cpu.Registers.A &&& 0xFFuy
        cpu.Flags <- cpu.Flags |> setN true |> setH true
