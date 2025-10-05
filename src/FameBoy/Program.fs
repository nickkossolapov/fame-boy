open System.IO

let readRom path = File.ReadAllBytes path

let bytes = readRom "D:/gb/bootroms/dmg_boot.bin"

for b in bytes do
    printfn $"`%02X{b}:{(System.Convert.ToString(b, 2).PadLeft(8, '0'))}` - \n"
