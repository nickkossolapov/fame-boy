module FameBoy.Test.InstructionTests.BitwiseTests

open FameBoy.Cpu.Execute
open FameBoy.Cpu.Instructions
open FameBoy.Cpu.Opcodes
open FameBoy.Cpu.State
open FameBoy.Cpu.State.Flags
open FameBoy.Memory
open NUnit.Framework

let twoBitPrefix = 0xCBuy

let setCpuFlags (z: bool, n: bool, h: bool, c: bool) (cpu: Cpu) =
    cpu.Flags <- cpu.Flags |> setZ z |> setN n |> setH h |> setC c

let verifyCpuFlags (z: bool, n: bool, h: bool, c: bool) (cpu: Cpu) =
    Assert.That(isZero cpu.Flags, Is.EqualTo z)
    Assert.That(isSub cpu.Flags, Is.EqualTo n)
    Assert.That(isHalf cpu.Flags, Is.EqualTo h)
    Assert.That(isCarry cpu.Flags, Is.EqualTo c)

type BitwiseTestData =
    { Description: string
      Opcode: uint8
      InputFlags: bool * bool * bool * bool // ZNHC
      InputData: uint8
      ExpectedLength: int
      ExpectedMCycles: MCycles
      ExpectedData: uint8
      ExpectedFlags: bool * bool * bool * bool } // ZNHC

let accumulatorTestData =
    [| { Description = "Rotate left circular accumulator - rlca"
         Opcode = 0x07uy
         InputFlags = (true, true, true, false)
         InputData = 0b10101010uy
         ExpectedLength = 1
         ExpectedMCycles = Fixed 1
         ExpectedData = 0b01010101uy
         ExpectedFlags = (false, false, false, true) }
       { Description = "Rotate right circular accumulator - rrca"
         Opcode = 0x0Fuy
         InputFlags = (true, true, true, false)
         InputData = 0b10101011uy
         ExpectedLength = 1
         ExpectedMCycles = Fixed 1
         ExpectedData = 0b11010101uy
         ExpectedFlags = (false, false, false, true) }
       { Description = "Rotate left accumulator - rla"
         Opcode = 0x17uy
         InputFlags = (true, true, true, true)
         InputData = 0b10101010uy
         ExpectedLength = 1
         ExpectedMCycles = Fixed 1
         ExpectedData = 0b01010101uy
         ExpectedFlags = (false, false, false, true) }
       { Description = "Rotate right accumulator - rra"
         Opcode = 0x1Fuy
         InputFlags = (true, true, true, true)
         InputData = 0b10101011uy
         ExpectedLength = 1
         ExpectedMCycles = Fixed 1
         ExpectedData = 0b11010101uy
         ExpectedFlags = (false, false, false, true) } |]
    |> Array.map (fun d -> TestCaseData(d).SetName d.Description)

[<Test>]
[<TestCaseSource(nameof accumulatorTestData)>]
let ``Test rotate accumulator instructions`` (data: BitwiseTestData) =
    // Setup
    let cpu = createCpu (createTestMemory [||])

    cpu.Pc <- 0x100us
    cpu.Memory.Cartridge.Rom[0x100] <- data.Opcode
    cpu.Registers.A <- data.InputData
    setCpuFlags data.InputFlags cpu

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr |> ignore

    // Evaluate
    Assert.That(instr.Length, Is.EqualTo data.ExpectedLength)
    Assert.That(instr.MCycles, Is.EqualTo data.ExpectedMCycles)

    Assert.That(cpu.Registers.A, Is.EqualTo data.ExpectedData)
    verifyCpuFlags data.ExpectedFlags cpu

let registerDirectTestData =
    [| { Description = "Rotate left circular - rlc b"
         Opcode = 0x00uy
         InputFlags = (false, true, true, false)
         InputData = 0b10101010uy
         ExpectedLength = 2
         ExpectedMCycles = Fixed 2
         ExpectedData = 0b01010101uy
         ExpectedFlags = (false, false, false, true) }
       { Description = "Rotate left circular sets zero flag - rlc b"
         Opcode = 0x00uy
         InputFlags = (false, true, true, false)
         InputData = 0b00000000uy
         ExpectedLength = 2
         ExpectedMCycles = Fixed 2
         ExpectedData = 0b00000000uy
         ExpectedFlags = (true, false, false, false) }
       { Description = "Rotate left - rl b"
         Opcode = 0x10uy
         InputFlags = (false, true, true, true)
         InputData = 0b10101010uy
         ExpectedLength = 2
         ExpectedMCycles = Fixed 2
         ExpectedData = 0b01010101uy
         ExpectedFlags = (false, false, false, true) }
       { Description = "Rotate left sets zero flag - rl b"
         Opcode = 0x10uy
         InputFlags = (false, true, true, false)
         InputData = 0b10000000uy
         ExpectedLength = 2
         ExpectedMCycles = Fixed 2
         ExpectedData = 0b00000000uy
         ExpectedFlags = (true, false, false, true) }
       { Description = "Shift left arithmetic - sla b"
         Opcode = 0x20uy
         InputFlags = (false, true, true, false)
         InputData = 0b11010110uy
         ExpectedLength = 2
         ExpectedMCycles = Fixed 2
         ExpectedData = 0b10101100uy
         ExpectedFlags = (false, false, false, true) }
       { Description = "Shift left arithmetic sets zero flag - sla b"
         Opcode = 0x20uy
         InputFlags = (false, true, true, false)
         InputData = 0b10000000uy
         ExpectedLength = 2
         ExpectedMCycles = Fixed 2
         ExpectedData = 0b00000000uy
         ExpectedFlags = (true, false, false, true) }
       { Description = "Shift right arithmetic - sra b"
         Opcode = 0x28uy
         InputFlags = (false, true, true, false)
         InputData = 0b11010111uy
         ExpectedLength = 2
         ExpectedMCycles = Fixed 2
         ExpectedData = 0b11101011uy
         ExpectedFlags = (false, false, false, true) }
       { Description = "Shift right arithmetic sets zero flag - sra b"
         Opcode = 0x28uy
         InputFlags = (false, true, true, false)
         InputData = 0b00000001uy
         ExpectedLength = 2
         ExpectedMCycles = Fixed 2
         ExpectedData = 0b00000000uy
         ExpectedFlags = (true, false, false, true) }
       { Description = "Swap nibbles - swap b"
         Opcode = 0x30uy
         InputFlags = (false, true, true, false)
         InputData = 0b11010010uy
         ExpectedLength = 2
         ExpectedMCycles = Fixed 2
         ExpectedData = 0b00101101uy
         ExpectedFlags = (false, false, false, false) }
       { Description = "Swap nibbles sets zero flag - swap b"
         Opcode = 0x30uy
         InputFlags = (false, true, true, false)
         InputData = 0b00000000uy
         ExpectedLength = 2
         ExpectedMCycles = Fixed 2
         ExpectedData = 0b00000000uy
         ExpectedFlags = (true, false, false, false) }
       { Description = "Shift right logical - srl b"
         Opcode = 0x38uy
         InputFlags = (false, true, true, false)
         InputData = 0b11010111uy
         ExpectedLength = 2
         ExpectedMCycles = Fixed 2
         ExpectedData = 0b01101011uy
         ExpectedFlags = (false, false, false, true) }
       { Description = "Shift right logical sets zero flag - srl b"
         Opcode = 0x38uy
         InputFlags = (false, true, true, false)
         InputData = 0b00000001uy
         ExpectedLength = 2
         ExpectedMCycles = Fixed 2
         ExpectedData = 0b00000000uy
         ExpectedFlags = (true, false, false, true) }
       { Description = "Reset bit - res 0,b"
         Opcode = 0x80uy
         InputFlags = (true, true, true, true)
         InputData = 0b11111111uy
         ExpectedLength = 2
         ExpectedMCycles = Fixed 2
         ExpectedData = 0b11111110uy
         ExpectedFlags = (true, true, true, true) } |]
    |> Array.map (fun d -> TestCaseData(d).SetName d.Description)

[<Test>]
[<TestCaseSource(nameof registerDirectTestData)>]
let ``Test bitwise register (direct) instructions`` (data: BitwiseTestData) =
    // Setup
    let cpu = createCpu (createTestMemory [||])

    cpu.Pc <- 0x100us
    cpu.Memory.Cartridge.Rom[0x100] <- twoBitPrefix
    cpu.Memory.Cartridge.Rom[0x101] <- data.Opcode
    cpu.Registers.B <- data.InputData
    setCpuFlags data.InputFlags cpu

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr |> ignore

    // Evaluate
    Assert.That(instr.Length, Is.EqualTo data.ExpectedLength)
    Assert.That(instr.MCycles, Is.EqualTo data.ExpectedMCycles)

    Assert.That(cpu.Registers.B, Is.EqualTo data.ExpectedData)
    verifyCpuFlags data.ExpectedFlags cpu

let hlIndirectTestData =
    [| { Description = "Rotate right circular - rrc (hl)"
         Opcode = 0x0Euy
         InputFlags = (false, true, true, false)
         InputData = 0b10101011uy
         ExpectedLength = 2
         ExpectedMCycles = Fixed 4
         ExpectedData = 0b11010101uy
         ExpectedFlags = (false, false, false, true) }
       { Description = "Rotate right circular sets zero flag - rrc (hl)"
         Opcode = 0x0Euy
         InputFlags = (false, true, true, false)
         InputData = 0b00000000uy
         ExpectedLength = 2
         ExpectedMCycles = Fixed 4
         ExpectedData = 0b00000000uy
         ExpectedFlags = (true, false, false, false) }
       { Description = "Rotate right - rr (hl)"
         Opcode = 0x1Euy
         InputFlags = (false, true, true, true)
         InputData = 0b10101011uy
         ExpectedLength = 2
         ExpectedMCycles = Fixed 4
         ExpectedData = 0b11010101uy
         ExpectedFlags = (false, false, false, true) }
       { Description = "Rotate right sets zero flag - rr (hl)"
         Opcode = 0x1Euy
         InputFlags = (false, true, true, false)
         InputData = 0b00000001uy
         ExpectedLength = 2
         ExpectedMCycles = Fixed 4
         ExpectedData = 0b00000000uy
         ExpectedFlags = (true, false, false, true) }
       { Description = "Shift right arithmetic - sra (hl)"
         Opcode = 0x2Euy
         InputFlags = (false, true, true, false)
         InputData = 0b10101011uy
         ExpectedLength = 2
         ExpectedMCycles = Fixed 4
         ExpectedData = 0b11010101uy
         ExpectedFlags = (false, false, false, true) }
       { Description = "Shift right arithmetic sets zero flag - sra (hl)"
         Opcode = 0x2Euy
         InputFlags = (false, true, true, false)
         InputData = 0b00000001uy
         ExpectedLength = 2
         ExpectedMCycles = Fixed 4
         ExpectedData = 0b00000000uy
         ExpectedFlags = (true, false, false, true) }
       { Description = "Swap nibbles - swap (hl)"
         Opcode = 0x36uy
         InputFlags = (false, true, true, false)
         InputData = 0b11010010uy
         ExpectedLength = 2
         ExpectedMCycles = Fixed 4
         ExpectedData = 0b00101101uy
         ExpectedFlags = (false, false, false, false) }
       { Description = "Swap nibbles sets zero flag - swap (hl)"
         Opcode = 0x36uy
         InputFlags = (false, true, true, false)
         InputData = 0b00000000uy
         ExpectedLength = 2
         ExpectedMCycles = Fixed 4
         ExpectedData = 0b00000000uy
         ExpectedFlags = (true, false, false, false) }
       { Description = "Set bit - set 7,(hl)"
         Opcode = 0xFEuy
         InputFlags = (true, true, true, true)
         InputData = 0b00000000uy
         ExpectedLength = 2
         ExpectedMCycles = Fixed 4
         ExpectedData = 0b10000000uy
         ExpectedFlags = (true, true, true, true) } |]
    |> Array.map (fun d -> TestCaseData(d).SetName d.Description)

[<Test>]
[<TestCaseSource(nameof hlIndirectTestData)>]
let ``Test bitwise HL (indirect) instructions`` (data: BitwiseTestData) =
    // Setup
    let cpu = createCpu (createTestMemory [||])

    cpu.Pc <- 0x100us
    cpu.Memory.Cartridge.Rom[0x100] <- twoBitPrefix
    cpu.Memory.Cartridge.Rom[0x101] <- data.Opcode
    cpu.Registers.HL <- 0xC000us
    cpu.Memory[0xC000us] <- data.InputData
    setCpuFlags data.InputFlags cpu

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr |> ignore

    // Evaluate
    Assert.That(instr.Length, Is.EqualTo data.ExpectedLength)
    Assert.That(instr.MCycles, Is.EqualTo data.ExpectedMCycles)

    Assert.That(cpu.Memory[0xC000us], Is.EqualTo data.ExpectedData)
    verifyCpuFlags data.ExpectedFlags cpu

[<Test>]
let ``Test bit 7 of H register - bit 7,h`` () =
    // Setup
    let opcode = 0x7Cuy
    let cpu = createCpu (createTestMemory [||])

    cpu.Pc <- 0x100us
    cpu.Memory.Cartridge.Rom[0x100] <- twoBitPrefix
    cpu.Memory.Cartridge.Rom[0x101] <- opcode
    cpu.Registers.H <- 0b10000000uy
    cpu.Flags <- cpu.Flags |> setZ true |> setN true |> setH false

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr |> ignore

    // Evaluate
    Assert.That(instr.Length, Is.EqualTo 2)
    Assert.That(instr.MCycles, Is.EqualTo(Fixed 2))

    Assert.That(isZero cpu.Flags, Is.False)
    Assert.That(isSub cpu.Flags, Is.False)
    Assert.That(isHalf cpu.Flags, Is.True)

[<Test>]
let ``Test bit 7 of H register, bit not set - bit 7,h`` () =
    // Setup
    let opcode = 0x7Cuy
    let cpu = createCpu (createTestMemory [||])

    cpu.Pc <- 0x100us
    cpu.Memory.Cartridge.Rom[0x100] <- twoBitPrefix
    cpu.Memory.Cartridge.Rom[0x101] <- opcode
    cpu.Registers.H <- 0b00000000uy
    cpu.Flags <- cpu.Flags |> setZ false |> setN true |> setH false

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr |> ignore

    // Evaluate
    Assert.That(instr.Length, Is.EqualTo 2)
    Assert.That(instr.MCycles, Is.EqualTo(Fixed 2))

    Assert.That(isZero cpu.Flags, Is.True)
    Assert.That(isSub cpu.Flags, Is.False)
    Assert.That(isHalf cpu.Flags, Is.True)
