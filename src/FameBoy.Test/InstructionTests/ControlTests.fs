module FameBoy.Test.InstructionTests.ControlTests

open FameBoy.Cpu.Execute
open FameBoy.Cpu.Opcodes
open FameBoy.Cpu.State
open FameBoy.Cpu.Instructions
open FameBoy.Memory
open NUnit.Framework


[<Test>]
let ``Jump to address - jp nn`` () =
    // Setup
    let opcode = 0xC3uy
    let cpu = createCpu (createMemory [||])

    cpu.Pc <- 0x100us
    cpu.Memory[0x100us] <- opcode
    cpu.Memory[0x101us] <- 0x34uy // LSB(nn)
    cpu.Memory[0x102us] <- 0x12uy // MSB(nn)

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 3)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 4))

    Assert.That (cpu.Pc, Is.EqualTo 0x1234us)

[<Test>]
let ``Jump to address in HL - jp hl`` () =
    // Setup
    let opcode = 0xE9uy
    let cpu = createCpu (createMemory [||])

    cpu.Pc <- 0x100us
    cpu.Registers.HL <- 0x1234us
    cpu.Memory[0x100us] <- opcode

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 1)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 1))

    Assert.That (cpu.Pc, Is.EqualTo 0x1234us)
    
[<Test>]
let ``Jump if carry, taken - jp c,a16`` () =
    // Setup
    let opcode = 0xDAuy
    let cpu = createCpu (createMemory [||])

    cpu.Pc <- 0x100us
    cpu.Memory[0x100us] <- opcode
    cpu.Memory[0x101us] <- 0x34uy // LSB(nn)
    cpu.Memory[0x102us] <- 0x12uy // MSB(nn)
    cpu.setFlag Flag.Carry true // C flag set, so jump is taken

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 3)
    Assert.That (instr.MCycles, Is.EqualTo (Conditional { Met = 4; NotMet = 3 }))

    Assert.That (cpu.Pc, Is.EqualTo 0x1234us)

[<Test>]
let ``Jump if carry, not taken - jp c,a16`` () =
    // Setup
    let opcode = 0xDAuy
    let cpu = createCpu (createMemory [||])

    cpu.Pc <- 0x100us
    cpu.Memory[0x100us] <- opcode
    cpu.Memory[0x101us] <- 0x34uy // LSB(nn)
    cpu.Memory[0x102us] <- 0x12uy // MSB(nn)
    cpu.setFlag Flag.Carry false // C flag not set, so jump is not taken

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 3)
    Assert.That (instr.MCycles, Is.EqualTo (Conditional { Met = 4; NotMet = 3 }))

    Assert.That (cpu.Pc, Is.EqualTo 0x103us) // PC just advances past instruction

[<Test>]
let ``Jump relative, positive - jr s8`` () =
    // Setup
    let opcode = 0x18uy
    let cpu = createCpu (createMemory [||])

    cpu.Pc <- 0x100us
    cpu.Memory[0x100us] <- opcode
    cpu.Memory[0x101us] <- 0x20uy // s8 = +32

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 2)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 3))

    Assert.That (cpu.Pc, Is.EqualTo 0x122us) // 0x100 + 2 + 32

[<Test>]
let ``Jump relative, negative - jr s8`` () =
    // Setup
    let opcode = 0x18uy
    let cpu = createCpu (createMemory [||])

    cpu.Pc <- 0x100us
    cpu.Memory[0x100us] <- opcode
    cpu.Memory[0x101us] <- 0xFBuy // s8 = -5

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 2)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 3))

    Assert.That (cpu.Pc, Is.EqualTo 0xFDus) // 0x100 + 2 - 5

[<Test>]
let ``Jump relative if not zero, taken - jr nz,s8`` () =
    // Setup
    let opcode = 0x20uy
    let cpu = createCpu (createMemory [||])

    cpu.Pc <- 0x100us
    cpu.Memory[0x100us] <- opcode
    cpu.Memory[0x101us] <- 0x05uy // s8 = +5
    cpu.setFlag Flag.Zero false // Z flag not set, so jump is taken

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 2)
    Assert.That (instr.MCycles, Is.EqualTo (Conditional { Met = 3; NotMet = 2 }))

    Assert.That (cpu.Pc, Is.EqualTo 0x107) // 0x100 + 2 + 5

[<Test>]
let ``Jump relative if not zero, not taken - jr nz,s8`` () =
    // Setup
    let opcode = 0x20uy
    let cpu = createCpu (createMemory [||])
    
    cpu.Pc <- 0x100us
    cpu.Memory[0x100us] <- opcode
    cpu.Memory[0x101us] <- 0x05uy // s8 = +5
    cpu.setFlag Flag.Zero true // Z flag set, so jump is not taken

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 2)
    Assert.That (instr.MCycles, Is.EqualTo (Conditional { Met = 3; NotMet = 2 }))

    Assert.That (cpu.Pc, Is.EqualTo 0x102) // PC just advances past instruction


[<Test>]
let ``Call function - call nn`` () =
    // Setup
    let opcode = 0xCDuy
    let cpu = createCpu (createMemory [||])
    
    cpu.Pc <- 0x100us
    cpu.Sp <- 0xFFFEus
    cpu.Memory[0x100us] <- opcode
    cpu.Memory[0x101us] <- 0x34uy // LSB(nn)
    cpu.Memory[0x102us] <- 0x12uy // MSB(nn)

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 3)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 6))

    Assert.That (cpu.Pc, Is.EqualTo 0x1234us)
    Assert.That (cpu.Sp, Is.EqualTo 0xFFFCus)
    Assert.That (cpu.Memory[0xFFFDus], Is.EqualTo 0x01uy) // MSB of 0x103
    Assert.That (cpu.Memory[0xFFFCus], Is.EqualTo 0x03uy) // LSB of 0x103

[<Test>]
let ``Call if no carry, taken - call nc,a16`` () =
    // Setup
    let opcode = 0xD4uy
    let cpu = createCpu (createMemory [||])

    cpu.Pc <- 0x100us
    cpu.Sp <- 0xFFFEus
    cpu.Memory[0x100us] <- opcode
    cpu.Memory[0x101us] <- 0x34uy // LSB(nn)
    cpu.Memory[0x102us] <- 0x12uy // MSB(nn)
    cpu.setFlag Flag.Carry false // C flag not set, so call is taken

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 3)
    Assert.That (instr.MCycles, Is.EqualTo (Conditional { Met = 6; NotMet = 3 }))

    Assert.That (cpu.Pc, Is.EqualTo 0x1234us)
    Assert.That (cpu.Sp, Is.EqualTo 0xFFFCus)
    Assert.That (cpu.Memory[0xFFFDus], Is.EqualTo 0x01uy) // MSB of 0x103
    Assert.That (cpu.Memory[0xFFFCus], Is.EqualTo 0x03uy) // LSB of 0x103

[<Test>]
let ``Call if no carry, not taken - call nc,a16`` () =
    // Setup
    let opcode = 0xD4uy
    let cpu = createCpu (createMemory [||])

    cpu.Pc <- 0x100us
    cpu.Sp <- 0xFFFEus
    cpu.Memory[0x100us] <- opcode
    cpu.Memory[0x101us] <- 0x34uy // LSB(nn)
    cpu.Memory[0x102us] <- 0x12uy // MSB(nn)
    cpu.setFlag Flag.Carry true // C flag set, so call is not taken

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 3)
    Assert.That (instr.MCycles, Is.EqualTo (Conditional { Met = 6; NotMet = 3 }))

    Assert.That (cpu.Pc, Is.EqualTo 0x103us) // PC just advances past instruction
    Assert.That (cpu.Sp, Is.EqualTo 0xFFFEus) // SP is unchanged

[<Test>]
let ``Return from function - ret`` () =
    // Setup
    let opcode = 0xC9uy
    let cpu = createCpu (createMemory [||])

    cpu.Pc <- 0x100us
    cpu.Sp <- 0xFFFCus
    cpu.Memory[0x100us] <- opcode
    cpu.Memory[0xFFFCus] <- 0x34uy // LSB of return address
    cpu.Memory[0xFFFDus] <- 0x12uy // MSB of return address

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 1)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 4))

    Assert.That (cpu.Pc, Is.EqualTo 0x1234us)
    Assert.That (cpu.Sp, Is.EqualTo 0xFFFEus)

[<Test>]
let ``Return if zero, taken - ret z`` () =
    // Setup
    let opcode = 0xC8uy
    let cpu = createCpu (createMemory [||])

    cpu.Pc <- 0x100us
    cpu.Sp <- 0xFFFCus
    cpu.Memory[0x100us] <- opcode
    cpu.Memory[0xFFFCus] <- 0x34uy // LSB of return address
    cpu.Memory[0xFFFDus] <- 0x12uy // MSB of return address
    cpu.setFlag Flag.Zero true // Z flag set, so return is taken

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 1)
    Assert.That (instr.MCycles, Is.EqualTo (Conditional { Met = 5; NotMet = 2 }))

    Assert.That (cpu.Pc, Is.EqualTo 0x1234us)
    Assert.That (cpu.Sp, Is.EqualTo 0xFFFEus)

[<Test>]
let ``Return if zero, not taken - ret z`` () =
    // Setup
    let opcode = 0xC8uy
    let cpu = createCpu (createMemory [||])

    cpu.Pc <- 0x100us
    cpu.Sp <- 0xFFFCus
    cpu.Memory[0x100us] <- opcode
    cpu.Memory[0xFFFCus] <- 0x34uy // LSB of return address
    cpu.Memory[0xFFFDus] <- 0x12uy // MSB of return address
    cpu.setFlag Flag.Zero false // Z flag not set, so return is not taken

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 1)
    Assert.That (instr.MCycles, Is.EqualTo (Conditional { Met = 5; NotMet = 2 }))

    Assert.That (cpu.Pc, Is.EqualTo 0x101us) // PC just advances past instruction
    Assert.That (cpu.Sp, Is.EqualTo 0xFFFCus) // SP is unchanged

[<Test>]
let ``Return from interrupt - reti`` () =
    // Setup
    let opcode = 0xD9uy
    let cpu = createCpu (createMemory [||])

    cpu.Pc <- 0x100us
    cpu.Sp <- 0xFFFCus
    cpu.Ime <- false
    cpu.Memory[0x100us] <- opcode
    cpu.Memory[0xFFFCus] <- 0x34uy // LSB of return address
    cpu.Memory[0xFFFDus] <- 0x12uy // MSB of return address

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 1)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 4))

    Assert.That (cpu.Pc, Is.EqualTo 0x1234us)
    Assert.That (cpu.Sp, Is.EqualTo 0xFFFEus)
    Assert.That (cpu.Ime, Is.True)

[<Test>]
let ``Restart to 0x18 - rst 0x18`` () =
    // Setup
    let opcode = 0xDFuy
    let cpu = createCpu (createMemory [||])

    cpu.Pc <- 0x100us
    cpu.Sp <- 0xFFFEus
    cpu.Memory[0x100us] <- opcode

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 1)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 4))

    Assert.That (cpu.Pc, Is.EqualTo 0x0018us)
    Assert.That (cpu.Sp, Is.EqualTo 0xFFFCus)
    Assert.That (cpu.Memory[0xFFFDus], Is.EqualTo 0x01uy) // MSB of 0x101
    Assert.That (cpu.Memory[0xFFFCus], Is.EqualTo 0x01uy) // LSB of 0x101

