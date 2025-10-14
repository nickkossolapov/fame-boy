# filepath: d:\personal\fame-boy\src\FameBoy.Test\Prompts\InstructionTests.md

Your goal is to create unit tests for a Game Boy emulator in F# using NUnit. The test will have the general body like:

``` fsharp
[<Test>]
let ``Load 16-bit register - ld sp,n16`` () = 
    // Setup
    let opcode = 0x31uy
    let cpu = createCpu [||]

    cpu.Pc <- 0x100
    cpu.Memory[0x100] <- opcode
    cpu.Memory[0x101] <- 0xFFuy
    cpu.Memory[0x102] <- 0xFEuy

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That (instr.Length, Is.EqualTo 3)
    Assert.That (instr.MCycles, Is.EqualTo (Fixed 3))

    Assert.That (cpu.Sp, Is.EqualTo 0xFEFFus)
    // other checks
```

* You will be given an example opcode and a reference implementation in pseudocode.
* The requested opcode is inserted at 0x100.
* Any data needed for the test may be inserted into any location in memory or any of the registers.
    * A through F can be accessed like `cpu.Registers.A`.
    * The double width registers can be accessed with `cpu.Registers.BC`, `cpu.Registers.DE`, `cpu.Registers.HL`
    * They can also be written with `cpu.Registers.setBC`, `cpu.Registers.setDE`, `cpu.Registers.setHL`
    * PC and SP can be accessed with `cpu.Pc` and `cpu.Sp`.
    * Additionally, the flags can also be read and written like `cpu.getFlag Flag.Zero` and `cpu.setFlag Flag.Zero true`.
* Test instructions that span multiple bytes.
* Test behavior at memory boundaries (e.g., 0xFFFF).
* Use descriptive names for tests in the format `` `[description] - [assembly]` ``,
  e.g., `` `Load 16-bit register - ld sp,n16` ``.
* Ensure to validate all side effects, including flags, memory, and registers.
* Ensure to validate the length and machine cycles (`MCycles`) of the instruction.
* The implementation of the CPU should be treated as a black box.
    * Only create the tests and do not attempt to fix code.
    * Do not look at the code implementation, create the test case only based on the provided pseudocode.
* Write multiple tests for any conditional instructions, testing both the met and not met cases.
