module FameBoy.Memory

let private memorySize = 65535us

type Memory(arr: uint8 array) =
    member _.Item
        with get (i: uint16) = arr[int i]
        and set (i: uint16) (v: uint8) = arr[int i] <- v

    member _.Array = arr

let createMemory (bootRom: uint8 array) =
    let memory = Array.zeroCreate (int memorySize)

    Array.blit bootRom 0 memory 0 bootRom.Length

    Memory memory
