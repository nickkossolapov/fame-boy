open System.IO
open FameBoy.Cpu.Execute
open FameBoy.Cpu.Opcodes
open FameBoy.Cpu.State

let readRom path = File.ReadAllBytes path

let bytes = readRom "D:/gb/bootroms/dmg_boot.bin"

let cpu = createCpu bytes

let instruction = fetchAndDecode cpu.Memory cpu.Pc

execute cpu instruction

printfn "Done"
