module FameBoy.Apu

open FameBoy.Hardware

[<Literal>]
let samplingRate = 48000 // hz

[<Literal>]
// let bufferSize = 2048 // samples
let bufferSize = 4096 // samples

[<Literal>]
let ringBufferSize = 3 * bufferSize


let tCyclesPerSample = (float32 (cpuFrequency * 4)) / float32 samplingRate

type Apu =
    { RingBuffer: float32 array
      mutable ReadHead: int
      mutable WriteHead: int
      mutable Timer: int
      mutable Accumulator: float32
      mutable TestFrequency: float32 }

let createApu () =
    { RingBuffer = Array.zeroCreate ringBufferSize
      ReadHead = 0
      WriteHead = 0
      Timer = 0
      Accumulator = 0f
      TestFrequency = 200f }

module private SineWave =
    // TODO send to Raylib https://www.raylib.com/examples/audio/loader.html?name=audio_raw_stream
    let sineFrequency = 200f // hz

    let at fr apuTime : float32 =
        let t = float32 apuTime / (float32 (cpuFrequency * 4))

        0.5f * sin (6.28318f * t * fr)

let stepApu (state: Apu) =
    state.Timer <- state.Timer + 1
    state.Accumulator <- state.Accumulator + 1f

    if state.Accumulator > tCyclesPerSample then
        state.Accumulator <- state.Accumulator - tCyclesPerSample

        if state.WriteHead - state.ReadHead < ringBufferSize then
            let i = state.WriteHead % ringBufferSize
            state.RingBuffer[i] <- SineWave.at state.TestFrequency state.Timer
            state.WriteHead <- state.WriteHead + 1
