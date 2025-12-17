module FameBoy.Cpu.Interrupts

open FameBoy.Cpu.State
open FameBoy.Cpu.Utils
open FameBoy.Hardware
open FameBoy.Memory

[<RequireQualifiedAccess>]
type InterruptType =
    | VBlank
    | LcdStat
    | Timer
    | Serial
    | Joypad

module private Helpers =
    let VBlankVector = 0x0040us
    let LcdStatVector = 0x0048us
    let TimerVector = 0x0050us
    let SerialVector = 0x0058us
    let JoypadVector = 0x0060us

    let InterruptList =
        seq {
            InterruptType.VBlank
            InterruptType.LcdStat
            InterruptType.Timer
            InterruptType.Serial
            InterruptType.Joypad
        }

    let getBitMask =
        function
        | InterruptType.VBlank -> 0b00000001uy
        | InterruptType.LcdStat -> 0b00000010uy
        | InterruptType.Timer -> 0b00000100uy
        | InterruptType.Serial -> 0b00001000uy
        | InterruptType.Joypad -> 0b00010000uy

    let getVector =
        function
        | InterruptType.VBlank -> VBlankVector
        | InterruptType.LcdStat -> LcdStatVector
        | InterruptType.Timer -> TimerVector
        | InterruptType.Serial -> SerialVector
        | InterruptType.Joypad -> JoypadVector

    let clearInterruptFlag (memory: Memory) (flag: InterruptType) =
        memory[IoRegisters.If] <- memory[IoRegisters.If] &&& ~~~(getBitMask flag)

open Helpers

let triggerInterrupt (memory: Memory) (t: InterruptType) =
    let mask = getBitMask t

    memory[IoRegisters.If] <- memory[IoRegisters.If] ||| mask

let checkForInterrupt (cpu: Cpu) : InterruptType option =
    if cpu.Ime then
        let enable = cpu.Memory[IoRegisters.Ie]
        let flag = cpu.Memory[IoRegisters.If]

        InterruptList |> Seq.tryFind (fun i -> enable &&& flag &&& getBitMask i <> 0uy)
    else
        None

let serviceInterrupt (cpu: Cpu) (interrupt: InterruptType) : int =
    cpu.Ime <- false
    cpu.Halted <- false
    clearInterruptFlag cpu.Memory interrupt
    pushToStack cpu cpu.Pc
    cpu.Pc <- getVector interrupt

    5 // Interrupts take 5 m-cycles
