module FameBoy.Cpu.Executors.Arithmetic

open FameBoy.Cpu.Instructions
open FameBoy.Cpu.State

let executeArithmetic (cpu: Cpu) (instr: ArithmeticInstr) =
    match instr with
    | Add bs ->
        let x, y = int cpu.Registers.A, int (bs.GetFrom cpu)
        let res = x + y

        cpu.Registers.A <- uint8 res

        cpu.setFlags
            [ Zero, uint8 res = 0uy
              Subtract, false
              HalfCarry, (x &&& 0xF + y &&& 0xF > 0xF)
              Carry, res > 0xFF ]
    | Adc bs ->
        let x, y = int cpu.Registers.A, int (bs.GetFrom cpu)
        let c = if (cpu.getFlag Carry) then 1 else 0
        let res = x + y + c

        cpu.Registers.A <- uint8 res

        cpu.setFlags
            [ Zero, uint8 res = 0uy
              Subtract, false
              HalfCarry, (x &&& 0xF + y &&& 0xF + c > 0xF)
              Carry, res > 0xFF ]
    | Sub bs ->
        let x, y = int cpu.Registers.A, int (bs.GetFrom cpu)
        let res = x - y

        cpu.Registers.A <- uint8 res

        cpu.setFlags
            [ Zero, uint8 res = 0uy
              Subtract, true
              HalfCarry, (x &&& 0xF) < (y &&& 0xF)
              Carry, res < 0 ]
    | Sbc bs ->
        let x, y = int cpu.Registers.A, int (bs.GetFrom cpu)
        let c = if (cpu.getFlag Carry) then 1 else 0
        let res = x - y - c

        cpu.Registers.A <- uint8 res

        cpu.setFlags
            [ Zero, uint8 res = 0uy
              Subtract, true
              HalfCarry, (x &&& 0xF) < (y &&& 0xF) + c
              Carry, res < 0 ]
    | Cp bs ->
        let x, y = int cpu.Registers.A, int (bs.GetFrom cpu)
        let res = x - y

        cpu.setFlags
            [ Zero, uint8 res = 0uy
              Subtract, true
              HalfCarry, (x &&& 0xF) < (y &&& 0xF)
              Carry, res < 0 ]
    | IncReg8 reg ->
        let value = reg.GetFrom cpu
        let res = value + 1uy

        reg.SetTo cpu res

        cpu.setFlags [ Zero, res = 0uy; Subtract, false; HalfCarry, (value &&& 0xFuy) = 0xFuy ]
    | IncAtHL ->
        let value = cpu.Memory[cpu.Registers.HL]
        let res = value + 1uy

        cpu.Memory[cpu.Registers.HL] <- res

        cpu.setFlags [ Zero, res = 0uy; Subtract, false; HalfCarry, (value &&& 0xFuy) = 0xFuy ]
    | DecReg8 reg ->
        let value = reg.GetFrom cpu
        let res = value - 1uy

        reg.SetTo cpu res

        cpu.setFlags [ Zero, res = 0uy; Subtract, true; HalfCarry, (value &&& 0xFuy) = 0x0uy ]
    | DecAtHL ->
        let value = cpu.Memory[cpu.Registers.HL]
        let res = value - 1uy

        cpu.Memory[cpu.Registers.HL] <- res

        cpu.setFlags [ Zero, res = 0uy; Subtract, false; HalfCarry, (value &&& 0xFuy) = 0xFuy ]
    | IncReg16 reg -> (reg.GetFrom cpu) + 1us |> reg.SetTo cpu
    | DecReg16 reg -> (reg.GetFrom cpu) - 1us |> reg.SetTo cpu
    | AddHL reg ->
        let x, y = int cpu.Registers.HL, int (reg.GetFrom cpu)
        let res = x + y

        cpu.Registers.HL <- uint16 res

        cpu.setFlags
            [ Subtract, false
              HalfCarry, (x &&& 0xFFF + y &&& 0xFFF > 0xF)
              Carry, res > 0xFFFF ]
    | AddSPe s ->
        let x, y = int cpu.Sp, int s
        let res = x + y

        cpu.Sp <- uint16 res

        cpu.setFlags [ Subtract, false; HalfCarry, (x &&& 0xF + y &&& 0xF > 0xF); Carry, res > 0xFF ]
