![Fame Boy icon](./assets/icon.png)

# Fame Boy

[![CI](https://github.com/nickkossolapov/fame-boy/actions/workflows/ci.yml/badge.svg)](https://github.com/nickkossolapov/fame-boy/actions/workflows/ci.yml)

A Game Boy (DMG) emulator written in F#. This is a development exercise for me, so I prioritised readability and idiomatic F# over
performance, but still with a few compromises for performance. It's quite far from being feature-complete, and may never reach there.

It runs natively on Linux, macOS, and Windows with [Raylib](https://www.raylib.com/) and in the browser with [Fable](https://fable.io/).

Try it out [here](https://nickkossolapov.github.io/fame-boy/)!

## Prerequisites

- [.NET 10 SDK](https://dotnet.microsoft.com/)
- [Node.js](https://nodejs.org/) (for the web project only)

## Getting Started

### Run local debug build

#### Desktop

``` bash
dotnet run --project src/FameBoy.Raylib -- <rom-file> [scale]
```

`scale` is an optional positive integer that controls the window size multiplier (default: 4).

#### Web

``` bash
cd src/FameBoy.Web
npm install
```

And then start Fable and serve the app with [Vite](https://vite.dev/):

``` bash
npm run dev
```

### Create release builds

#### Desktop

``` bash
dotnet build .\src\FameBoy.Raylib\FameBoy.Raylib.fsproj -c release 
```

#### Web

``` bash
npm run build
```

### Run tests

Covers most of the core emulator (except PPU rendering).

```bash
dotnet test
```

### Run benchmarks

The benchmark includes a few ROMs to run the emulator with in headless mode with [BenchmarkDotNet](https://benchmarkdotnet.org/).

``` powershell
./benchmark.ps1
```

Or directly:

``` bash
dotnet run --project src/FameBoy.Benchmark -c Release
```

## Controls

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

This project redistributes an unmodified copy of [Tobu Tobu Girl DX](https://github.com/SimonLarsen/tobutobugirl) by Simon Larsen,
included under its original MIT/CC-BY licensing terms and is not covered by the above license.

