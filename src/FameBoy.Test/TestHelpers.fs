module FameBoy.Test.TestHelpers

open FameBoy.Cartridge
open FameBoy.Cpu.State
open FameBoy.IoController
open FameBoy.Memory


let createTestMemory (arr: uint8 array) : Memory =
    let memory = Array.zeroCreate 0x10000
    Array.blit arr 0 memory 0 arr.Length

    let io = createIoController ()
    let cartridge = createCartridge memory

    { VideoRam = memory[0x8000..0x9FFF]
      WorkRam = memory[0xC000..0xDFFF]
      OamRam = memory[0xFE00..0xFE9F]
      IoController = io
      HighRam = memory[0xFF80..0xFFFE]
      Cartridge = cartridge }

let createTestCpu arr =
    let memory = createTestMemory arr
    let cpu = createCpu memory

    cpu, memory.IoController
