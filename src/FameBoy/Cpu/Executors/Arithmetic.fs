module FameBoy.Cpu.Executors.Arithmetic

open FameBoy.Cpu.Instructions
open FameBoy.Cpu.State

let executeArithmetic (cpu: Cpu) (instr: ArithmeticInstr) =
    match instr with
    | Add bs ->
        let x, y = int cpu.Registers.A, int (bs.GetFrom cpu)
        let res = x + y
        let halfCarry = (x &&& 0xF) + (y &&& 0xF) > 0xF
        let carry = res > 0xFF

        cpu.Registers.A <- uint8 res

        cpu.setFlags [ Zero, uint8 res = 0uy; Subtract, false; HalfCarry, halfCarry; Carry, carry ]
    | Adc bs ->
        let x, y = int cpu.Registers.A, int (bs.GetFrom cpu)
        let c = if cpu.getFlag Carry then 1 else 0
        let res = x + y + c
        let halfCarry = (x &&& 0xF) + (y &&& 0xF) + c > 0xF
        let carry = res > 0xFF

        cpu.Registers.A <- uint8 res

        cpu.setFlags [ Zero, uint8 res = 0uy; Subtract, false; HalfCarry, halfCarry; Carry, carry ]
    | Sub bs ->
        let x, y = int cpu.Registers.A, int (bs.GetFrom cpu)
        let res = x - y
        let halfCarry = (x &&& 0xF) < (y &&& 0xF)
        let carry = res < 0

        cpu.Registers.A <- uint8 res

        cpu.setFlags [ Zero, uint8 res = 0uy; Subtract, true; HalfCarry, halfCarry; Carry, carry ]
    | Sbc bs ->
        let x, y = int cpu.Registers.A, int (bs.GetFrom cpu)
        let c = if cpu.getFlag Carry then 1 else 0
        let res = x - y - c
        let halfCarry = (x &&& 0xF) < (y &&& 0xF) + c
        let carry = res < 0

        cpu.Registers.A <- uint8 res

        cpu.setFlags [ Zero, uint8 res = 0uy; Subtract, true; HalfCarry, halfCarry; Carry, carry ]
    | Cp bs ->
        let x, y = int cpu.Registers.A, int (bs.GetFrom cpu)
        let res = x - y
        let halfCarry = (x &&& 0xF) < (y &&& 0xF)
        let carry = res < 0

        cpu.setFlags [ Zero, uint8 res = 0uy; Subtract, true; HalfCarry, halfCarry; Carry, carry ]
    | Inc ref ->
        let value = ref.GetFrom cpu
        let res = value + 1uy
        let halfCarry = (value &&& 0xFuy) = 0xFuy

        ref.SetTo cpu res

        cpu.setFlags [ Zero, res = 0uy; Subtract, false; HalfCarry, halfCarry ]
    | Dec ref ->
        let value = ref.GetFrom cpu
        let res = value - 1uy
        let halfCarry = (value &&& 0xFuy) = 0x0uy

        ref.SetTo cpu res

        cpu.setFlags [ Zero, res = 0uy; Subtract, true; HalfCarry, halfCarry ]
    | IncReg16 reg -> (reg.GetFrom cpu) + 1us |> reg.SetTo cpu
    | DecReg16 reg -> (reg.GetFrom cpu) - 1us |> reg.SetTo cpu
    | AddHL reg ->
        let x, y = int cpu.Registers.HL, int (reg.GetFrom cpu)
        let res = x + y
        let halfCarry = (x &&& 0xFFF) + (y &&& 0xFFF) > 0xFFF
        let carry = res > 0xFFFF

        cpu.Registers.HL <- uint16 res

        cpu.setFlags [ Subtract, false; HalfCarry, halfCarry; Carry, carry ]
    | AddSPe s ->
        let sp, e = cpu.Sp, uint16 s
        let res = sp + e

        let halfCarry = ((sp &&& 0x0Fus) + (e &&& 0x0Fus)) &&& 0x10us <> 0us
        let carry = ((sp &&& 0xFFus) + (e &&& 0xFFus)) &&& 0x100us <> 0us

        cpu.Sp <- uint16 res

        cpu.setFlags [ Zero, false; Subtract, false; HalfCarry, halfCarry; Carry, carry ]
