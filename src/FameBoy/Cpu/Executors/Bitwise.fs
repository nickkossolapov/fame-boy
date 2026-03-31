module FameBoy.Cpu.Executors.Bitwise

open FameBoy.Cpu.Instructions
open FameBoy.Cpu.Instructions.Operand
open FameBoy.Cpu.State
open FameBoy.Cpu.State.Flags

let private shiftLeft (edge: bool) (value: uint8) =
    let e = if edge then 1uy else 0uy
    let c = (value &&& 0x80uy) <> 0uy

    ((value <<< 1) + e) &&& 0xFFuy, c

let private rotateLeftCircular (value: uint8) =
    shiftLeft ((value &&& 0x80uy) <> 0uy) value

let private shiftRight (edge: bool) (value: uint8) =
    let e = if edge then 0x80uy else 0uy
    let c = (value &&& 0x1uy) <> 0uy

    ((value >>> 1) + e) &&& 0xFFuy, c

let private rotateRightCircular (value: uint8) =
    shiftRight ((value &&& 0x1uy) <> 0uy) value

let private shiftA (cpu: Cpu) moveFunc =
    let shifted, c = moveFunc cpu.Registers.A

    cpu.Registers.A <- shifted
    cpu.Flags <- cpu.Flags |> setZ false |> setN false |> setH false |> setC c

let private shiftBytes (target: Target) (cpu: Cpu) moveFunc =
    let shifted, c = moveFunc (target.GetFrom cpu)

    target.SetTo cpu shifted
    cpu.Flags <- cpu.Flags |> setZ (shifted = 0uy) |> setN false |> setH false |> setC c

let executeBitwise (cpu: Cpu) (instr: BitwiseInstr) =
    match instr with
    | Rlca -> shiftA cpu rotateLeftCircular
    | Rrca -> shiftA cpu rotateRightCircular
    | Rla -> shiftA cpu (shiftLeft (isCarry cpu.Flags))
    | Rra -> shiftA cpu (shiftRight (isCarry cpu.Flags))
    | Rlc target -> shiftBytes target cpu rotateLeftCircular
    | Rrc target -> shiftBytes target cpu rotateRightCircular
    | Rl target -> shiftBytes target cpu (shiftLeft (isCarry cpu.Flags))
    | Rr target -> shiftBytes target cpu (shiftRight (isCarry cpu.Flags))
    | Sla target -> shiftBytes target cpu (shiftLeft false)
    | Sra target ->
        let msb = ((target.GetFrom cpu &&& 0x80uy) <> 0uy)

        shiftBytes target cpu (shiftRight msb)
    | Srl target -> shiftBytes target cpu (shiftRight false)
    | Bit(u3, target) ->
        let value = target.GetFrom cpu
        let bitIsZero = ((value >>> (int u3)) &&& 1uy) = 0uy

        cpu.Flags <- cpu.Flags |> setZ bitIsZero |> setN false |> setH true
    | Swap target ->
        let value = target.GetFrom cpu
        let swapped = (((value <<< 4) &&& 0xF0uy) + (value >>> 4)) &&& 0xFFuy

        target.SetTo cpu swapped
        cpu.Flags <- cpu.Flags |> setZ (swapped = 0uy) |> setN false |> setH false |> setC false
    | Res(u3, target) ->
        let mask = ~~~(1uy <<< (int u3)) &&& 0xFFuy
        let res = (target.GetFrom cpu) &&& mask

        target.SetTo cpu res
    | Set(u3, target) ->
        let mask = (1uy <<< (int u3)) &&& 0xFFuy
        let res = (target.GetFrom cpu) ||| mask

        target.SetTo cpu res
