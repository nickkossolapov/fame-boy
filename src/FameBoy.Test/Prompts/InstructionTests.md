# filepath: d:\personal\fame-boy\src\FameBoy.Test\Prompts\InstructionTests.md

Your goal is to create unit tests for a Game Boy emulator in F# using NUnit. The test will have the general body:

``` cs
[Test]
public void TestExampleOpcode()
{
    // Setup
    let opcode = <opcode>
    let length = 3
    let cpu = createCpu [||]

    cpu.Pc <- 0x100
    cpu.Memory[0x100] <- 0x21 
    cpu.Memory[0x101] <- 0x34 // Upper half
    cpu.Memory[0x102] <- 0x12 // Lower half

    // Execute
    let instr = fetchAndDecode cpu.Memory cpu.Pc
    execute cpu instr

    // Evaluate
    Assert.That(cpu.Pc, Is.EqualTo(0x100 + length))
    Assert.That(cpu.Sp, Is.EqualTo(0xFFFEus)) 
    // other checks
}
```

* You will be given an example opcode and a reference implementation in pseudocode.
* The requested opcode is inserted at 0x100.
* Any data needed for the test may be inserted into memory or any of the registers like `cpu.Registers.A <- value`.
* The CPU state is mutable at the time of writing.
* Test instructions that span multiple bytes.
* Test behavior at memory boundaries (e.g., 0xFFFF).
* Include edge cases such as invalid opcodes or unaligned memory accesses.
* Use descriptive names for tests in the format `` `Test [description] - [assembly]` ``,
  e.g., `` `Test Load 16-bit register - ld sp,n16` ``, where type is the
* Ensure to validate all side effects, including flags, memory, and registers.
* Ensure to validate the length of the instruction.
* Only create the tests and do not attempt to fix code. The implementation of the CPU should be treated as a black box.
* Outside of `Setup`, `Execute`, and `Evaluate`, do not include any other comments like `Upper half` or `Lower half`
