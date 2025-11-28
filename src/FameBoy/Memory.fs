module FameBoy.Memory

let private memorySize = 0x10000

type Memory(arr: uint8 array) =
    member _.Item
        with get (i: uint16) = arr[int i]
        and set (i: uint16) (v: uint8) = arr[int i] <- v

    member _.Array = arr

let createMemory (rom: uint8 array) =
    let memory = Array.zeroCreate memorySize

    Array.blit rom 0 memory 0 rom.Length
    Array.fill memory 0x8000 0x2000 0uy

    Memory memory
