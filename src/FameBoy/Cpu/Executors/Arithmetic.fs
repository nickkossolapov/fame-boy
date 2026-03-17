module FameBoy.Cpu.Executors.Arithmetic

open FameBoy.Cpu.Instructions
open FameBoy.Cpu.State
open FameBoy.Cpu.State.Flags

let executeArithmetic (cpu: Cpu) (instr: ArithmeticInstr) =
    match instr with
    | Add bs ->
        let x, y = int cpu.Registers.A, int (bs.GetFrom cpu)
        let res = x + y
        let hc = (x &&& 0xF) + (y &&& 0xF) > 0xF
        let c = res > 0xFF

        cpu.Registers.A <- uint8 res
        cpu.Flags <- cpu.Flags |> setZ (uint8 res = 0uy) |> setN false |> setH hc |> setC c
    | Adc bs ->
        let x, y = int cpu.Registers.A, int (bs.GetFrom cpu)
        let c = if isCarry cpu.Flags then 1 else 0
        let res = x + y + c
        let h = (x &&& 0xF) + (y &&& 0xF) + c > 0xF
        let newC = res > 0xFF

        cpu.Registers.A <- uint8 res
        cpu.Flags <- cpu.Flags |> setZ (uint8 res = 0uy) |> setN false |> setH h |> setC newC
    | Sub bs ->
        let x, y = int cpu.Registers.A, int (bs.GetFrom cpu)
        let res = x - y
        let h = (x &&& 0xF) < (y &&& 0xF)
        let c = res < 0

        cpu.Registers.A <- uint8 res
        cpu.Flags <- cpu.Flags |> setZ (uint8 res = 0uy) |> setN true |> setH h |> setC c
    | Sbc bs ->
        let x, y = int cpu.Registers.A, int (bs.GetFrom cpu)
        let c = if isCarry cpu.Flags then 1 else 0
        let res = x - y - c
        let h = (x &&& 0xF) < (y &&& 0xF) + c
        let newC = res < 0

        cpu.Registers.A <- uint8 res
        cpu.Flags <- cpu.Flags |> setZ (uint8 res = 0uy) |> setN true |> setH h |> setC newC
    | Cp bs ->
        let x, y = int cpu.Registers.A, int (bs.GetFrom cpu)
        let res = x - y
        let h = (x &&& 0xF) < (y &&& 0xF)
        let c = res < 0

        cpu.Flags <- cpu.Flags |> setZ (uint8 res = 0uy) |> setN true |> setH h |> setC c
    | Inc ref ->
        let value = ref.GetFrom cpu
        let res = (value + 1uy) &&& 0xFFuy
        let h = (value &&& 0xFuy) = 0xFuy

        ref.SetTo cpu res
        cpu.Flags <- cpu.Flags |> setZ (res = 0uy) |> setN false |> setH h
    | Dec ref ->
        let value = ref.GetFrom cpu
        let res = (value - 1uy) &&& 0xFFuy
        let h = (value &&& 0xFuy) = 0x0uy

        ref.SetTo cpu res
        cpu.Flags <- cpu.Flags |> setZ (res = 0uy) |> setN true |> setH h
    | IncReg16 reg -> ((reg.GetFrom cpu) + 1us) &&& 0xFFFFus |> reg.SetTo cpu
    | DecReg16 reg -> ((reg.GetFrom cpu) - 1us) &&& 0xFFFFus |> reg.SetTo cpu
    | AddHL reg ->
        let x, y = int cpu.Registers.HL, int (reg.GetFrom cpu)
        let res = x + y
        let h = (x &&& 0xFFF) + (y &&& 0xFFF) > 0xFFF
        let c = res > 0xFFFF

        cpu.Registers.HL <- uint16 res
        cpu.Flags <- cpu.Flags |> setN false |> setH h |> setC c
    | AddSPe s ->
        let sp, e = cpu.Sp, uint16 s
        let res = (sp + e) &&& 0xFFFFus

        let h = ((sp &&& 0x0Fus) + (e &&& 0x0Fus)) &&& 0x10us <> 0us
        let c = ((sp &&& 0xFFus) + (e &&& 0xFFus)) &&& 0x100us <> 0us

        cpu.Sp <- res
        cpu.Flags <- cpu.Flags |> setZ false |> setN false |> setH h |> setC c
