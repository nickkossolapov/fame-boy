open System.IO
open FameBoy.Cpu.Execute
open FameBoy.Cpu.Opcodes
open FameBoy.Cpu.State

let readRom path = File.ReadAllBytes path

let bytes = readRom "D:/gb/bootroms/dmg_boot.bin"

let cpu = createCpu bytes


for _ in 0..100 do
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

printfn "Done"
