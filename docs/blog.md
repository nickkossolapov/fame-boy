# Building a Game Boy emulator in F\#

I've been working as a software engineer for over 8 years at this point, and admittedly I've never understood how computers actually work. So I figured a good way to actually learn how they work would be to try and emulate one. I spent hundreds of hours as a kid catching Pokémon, and so the Game Boy was the perfect candidate: real hardware, relatively simple in scope, and something with a personal connection.

Instead of jumping straight into it, I first did [From NAND to Tetris](https://www.nand2tetris.org/). It was a great course, and it made me really understand the fundamentals of computers, like registers, memory, and the ALU. Then to get used to building an emulator, I built a CHIP-8 emulator in F#: [Fip-8](https://github.com/nickkossolapov/fip-8).

A few months later, and after many nights of going to bed at 2 AM even though I told myself I'd only work on it for an hour or two, I have a working Game Boy emulator: Fame Boy.

![gif of the emulator playing pokemon](./assets/pokemon.gif)

## How it works

I knew I wanted to have the emulator working on both web and desktop, so I focused on having a simple interface between the emulator core, and whatever frontend is running it.

The interface between the frontends and core is essentially just two arrays and two functions:
- `framebuffer` - 144 \* 160 length array of shades (white, light, dark, black).
- `audiobuffer` - a ring audio buffer at sample rate of 32768 Hz with read and write heads.
- `stepEmulator()` - a function that goes through 1 CPU instruction and returns the number of cycles taken.
- `getJoypadState(state)` - a callback used by the emulator whenever the CPU tries to read the joypad hardware register.

![Architecture diagram](./assets/architecture.svg)

I tried to model Fame Boy in similar way to the actual hardware of the Game Boy.

The [CPU](../src/FameBoy/Cpu/), like the real Sharp LR35902 in the Game Boy, knows nothing about the hardware except a memory map. It's also the most F#-ish part of my codebase, leaning heavily into functional domain modelling.

[Memory.fs](../src/FameBoy/Memory.fs) holds most of the RAM used in the Game Boy, and acts as the memory map and bus between the CPU, IO Controller, and cartridge. It also shares a reference to the same VRAM and OAM RAM arrays with the PPU for performance. 

[IoController.fs](./src/FameBoy/IoController.fs) emerged when I found myself adding too much logic to Memory.fs. While a singular IO controller doesn't exist in the Game Boy hardware, handling all the hardware registers through it simplified and added safety to the interfaces for the individual components. 

The `stepper` function in [Emulator.fs](../src/FameBoy/Emulator.fs) is the glue that brings the whole emulator together, composing all the components' individual step functions:

```fsharp
let stepper () =
	// Execute a single instruction
	// Each instruction uses a different amount of cycles
	let mCycles = stepCpu cpu io
	  
	for _ in 1..mCycles do  
	    stepTimers timer io  
	    stepSerial serial io
	    // The APU technically runs at 4x CPU-cycles, but can be batched
	    stepApu apu  
	  
	// The PPU operates at 4x CPU-cycles, but can't be batched
	let tCycles = mCycles * 4  
	  
	for _ in 1..tCycles do  
	    stepPpu ppu  
	
	// Return cycles taken so the frontend run the emulator at the right speed
	mCycles 
```

While the real hardware components all run in parallel based on a central master oscillator, my emulator is single threaded and so they have to run in sequence. The stepper function centralises the execution and ensures that all the components are synchronised.

Lastly, for the emulator to be playable it needs to run at the correct number of cycles per second, around 17476 CPU-cycles per 60 FPS frame. The frontends use the audio sampling rate to drive the emulator when the sound is on, and the frame rate to drive the emulator when it's muted. More on this later in the chapter about sound.

## Emulating the CPU and F\#

First of all, I'd like to apologise to the functional programming purists. While [my CHIP-8 emulator](https://github.com/nickkossolapov/fip-8)is completely pure (no `mutable` members and all arrays are copied for none of that side-effect nonsense), Fame Boy uses mutability liberally. The Game Boy runs *a lot* faster than the CHIP-8, and copying 16+ kB of memory a million times every second didn't seem like the smart thing to do.

TODO Testing, overall structure, and using F# types (setFlag etc)

## The other components

### PPU

The Game Boy doesn't have a GPU, it has PPU, picture processing unit. Although in my mind it actually stands for pixel processing unit, because while building I spent more time focused on the individual pixels than any sort of picture.

This is the part that surprised me when it came to blogs from other people who made Game Boy emulators. Many blogs focused on the CPU, with only a few paragraphs for the PPU. Maybe it's because I was fresh off of From Nand to Tetris and the CHIP-8 emulator, the CPU felt straight forward, while the PPU took a lot longer to understand. 

TODO more details on building the PPU, debug maps, scanline etc

### Joypad

TODO kept on breaking the joypad because games read it twice for all of the inputs, so caching was hard

## Taking it to the web with Fable

TODO WebAssembly Register uint8 clamping, and simplicity

## Trying to improve performance

TODO

## It's 2026, so a bit on AI

No software is free from the influence of AI these days, even learning projects. In general I strive to be transparent about my AI usage, and so I wanted to comment on how I used it and my experience with it in a purely learning project.

TODO How I used AI, the timing winter, and some optimising at the end

## Sound is hard

TODO 
- When starting audio, decided pretty quickly to not use timer clock to drive the emulator (foreshadowing). Audio driven vs frame driven.
- Windows had really weird issue with 800 buffer size not working (even though it worked when doing framerate driven clock).

## Overall experience 

TODO

![Screenshot of browser version|300](screenshot.png)
