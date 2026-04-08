# Fame Boy

[![CI](https://github.com/nickkossolapov/fame-boy/actions/workflows/ci.yml/badge.svg)](https://github.com/nickkossolapov/fame-boy/actions/workflows/ci.yml)

A Game Boy (DMG) emulator written in F#. Try it out in the browser [here](https://nickkossolapov.github.io/fame-boy/)!

![pokemon demo](./assets/pokemon.gif) ![zelda demo](./assets/zelda.gif)

### Features

- Supports most of the popular Game Boy games with sound (incl. Tetris, Pokémon, Mario, Zelda, and more!).
- Runs [in the browser](https://nickkossolapov.github.io/fame-boy/) with a touch-friendly fully responsive design built with [Fable](https://fable.io/).
- Cross-platform too, it runs natively on Windows, macOS, and Linux (and others) with [Raylib](https://www.raylib.com/).
- Zero-dependency [F# core](./src/FameBoy) with robust typing and built with functional programming in mind.

### About

This started as a way for me to learn more about computer hardware. The original Game Boy felt like the perfect target: well-defined in
scope while still complex enough to learn about a real system, and chock-full of nostalgia.

Along the way, I mostly optimised for readability, idiomatic F#, and having fun in the process, rather than chasing perfect hardware
accuracy or maximum performance.

I did use AI for a lot of the test cases, some code review, occasionally helping me understand bits of the hardware (looking at you, APU), 
and a particularly unhinged performance optimization session at the end. But most of the code was still (unfortunately?) meticulously 
crafted by me.

### Limitations

There are still a few gaps with the real hardware that I may or may not get to (but would like to).

- No Game Boy Color support.
- Limited emulator configuration (no fast-forward, key remapping, or custom palettes).
- Missing battery saves (SRAM) and save states.
- Hardware inaccuracies (e.g. CPU instruction-level rather than M-cycle-level timing, scanline-based rendering rather than pixel FIFOs, and missing a few hardware features/bugs).

### Repo structure

- `FameBoy` - Core emulator library (CPU, PPU, memory, cartridges, IO)
- `FameBoy.Raylib` - Native desktop frontend using Raylib
- `FameBoy.Web` - Browser frontend using Fable and Vite
- `FameBoy.Test` - Unit and integration tests
- `FameBoy.Benchmark`, `FameBoy.Benchmark.Web` - Performance benchmarking projects

## Getting Started

### Prerequisites

- [.NET 10 SDK](https://dotnet.microsoft.com/)
- [Node.js](https://nodejs.org/) (for the web projects only)

### Running it locally

#### Desktop

``` sh
dotnet run --project src/FameBoy.Raylib -- <rom-file-path> [scale]
```

`scale` is an optional positive integer that controls the window size multiplier (default: 4).

#### Web

``` sh
cd src/FameBoy.Web
npm install
npm run dev
```

This starts both Fable and Vite in watch mode.

### Testing

The unit tests cover most of the core emulator, and the [integration tests](./src/FameBoy.Test/IntegrationTests.fs) run the
emulator with the [dmg-acid2](https://github.com/mattcurrie/dmg-acid2) and [Blargg cpu_instrs](https://github.com/retrio/gb-test-roms)
test ROMs, then compare the PPU's framebuffer with a known correct framebuffer.

To run the tests:

``` sh
dotnet test
```

### Benchmarks

#### Native

The benchmark includes a few ROMs to run the emulator with in headless mode with [BenchmarkDotNet](https://benchmarkdotnet.org/).

``` sh
./benchmark.ps1
```

Or directly:

``` sh
cd src/FameBoy.Benchmark
dotnet run -c release
```

#### Web

There is also a basic Node.js benchmarking project using the same test ROMs and structure as the native benchmarking ﻿to better estimate
browser performance.

``` sh
./benchmark-web.ps1
```

Or directly:

``` sh
cd src/FameBoy.Benchmark.Web
npm run bench
```

### Controls

| Game Boy | Key           |
|----------|---------------|
| D-pad    | W / A / S / D |
| A        | K             |
| B        | J             |
| Start    | N             |
| Select   | B             |

The web version also supports mouse/touch.

## License

The Fame Boy source code is licensed under the [MIT License](./LICENSE).

This project redistributes an unmodified copy of [Tobu Tobu Girl DX](https://github.com/SimonLarsen/tobutobugirl-dx) by Simon Larsen,
included under its original MIT/CC-BY licensing terms and is not covered by the above license.
