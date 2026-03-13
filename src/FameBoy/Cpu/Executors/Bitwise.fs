module FameBoy.Cpu.Executors.Bitwise

open FameBoy.Cpu.Instructions
open FameBoy.Cpu.Instructions.ByteSource
open FameBoy.Cpu.State

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
    let shifted, carry = moveFunc cpu.Registers.A

    cpu.Registers.A <- shifted
    cpu.setFlags [ Zero, false; Subtract, false; HalfCarry, false; Carry, carry ]

let private shiftBytes (write: Write) (cpu: Cpu) moveFunc =
    let shifted, carry = moveFunc (write.GetFrom cpu)

    write.SetTo cpu shifted
    cpu.setFlags [ Zero, shifted = 0uy; Subtract, false; HalfCarry, false; Carry, carry ]

let executeBitwise (cpu: Cpu) (instr: BitwiseInstr) =
    match instr with
    | Rlca -> shiftA cpu rotateLeftCircular
    | Rrca -> shiftA cpu rotateRightCircular
    | Rla -> shiftA cpu (shiftLeft (cpu.getFlag Carry))
    | Rra -> shiftA cpu (shiftRight (cpu.getFlag Carry))
    | Rlc w -> shiftBytes w cpu rotateLeftCircular
    | Rrc w -> shiftBytes w cpu rotateRightCircular
    | Rl w -> shiftBytes w cpu (shiftLeft (cpu.getFlag Carry))
    | Rr w -> shiftBytes w cpu (shiftRight (cpu.getFlag Carry))
    | Sla w -> shiftBytes w cpu (shiftLeft false)
    | Sra w ->
        let msb = ((w.GetFrom cpu &&& 0x80uy) <> 0uy)

        shiftBytes w cpu (shiftRight msb)
    | Srl w -> shiftBytes w cpu (shiftRight false)
    | Bit (u3, w) ->
        let value = w.GetFrom cpu
        let bitIsZero = ((value >>> (int u3)) &&& 1uy) = 0uy

        cpu.setFlags [ Zero, bitIsZero; Subtract, false; HalfCarry, true ]
    | Swap w ->
        let value = w.GetFrom cpu
        let swapped = (((value <<< 4) &&& 0xF0uy) + (value >>> 4)) &&& 0xFFuy

        w.SetTo cpu swapped
        cpu.setFlags [ Zero, swapped = 0uy; Subtract, false; HalfCarry, false; Carry, false ]
    | Res (u3, w) ->
        let mask = ~~~(1uy <<< (int u3)) &&& 0xFFuy
        let res = (w.GetFrom cpu) &&& mask

        w.SetTo cpu res
    | Set (u3, w) ->
        let mask = (1uy <<< (int u3)) &&& 0xFFuy
        let res = (w.GetFrom cpu) ||| mask

        w.SetTo cpu res
