module FameBoy.Apu

open FameBoy.Hardware
open FameBoy.IoController


module private Constants =
    // Native rate = 4,194,304 / 128 = 32,768 Hz - exact, no need to deal with float in APU state
    [<Literal>]
    let tCyclesPerSample = 128

    [<Literal>]
    let nativeSampleRate = 32768 // From above

    [<Literal>]
    // let ringBufferSize = 2048 // TODO experiment with buffer size once I get real audio working
    let ringBufferSize = 4096

    [<Literal>]
    let ringBufferModulo = ringBufferSize - 1

    [<Literal>]
    let drcGain = 0.02

open Constants

type Apu =
    { RingBuffer: float32 array
      mutable WriteHead: int
      mutable ReadHead: int
      mutable Timer: int
      mutable Counter: int
      mutable TestFrequency: float }

let createApu () =
    { RingBuffer = Array.zeroCreate ringBufferSize
      WriteHead = ringBufferSize / 2
      ReadHead = 0
      Timer = 0
      Counter = 0
      TestFrequency = 200.0 }

module private SineWave =
    let at fr apuTime : float32 =
        let t = float apuTime / (float (cpuFrequency * 4))

        float32 (0.3 * sin (6.28318 * t * fr))

let stepApu (state: Apu) (io: IoController) =
    state.Timer <- state.Timer + 1
    state.Counter <- state.Counter + 1

    if state.Counter >= tCyclesPerSample then
        state.Counter <- 0

        let i = state.WriteHead &&& ringBufferModulo
        state.RingBuffer[i] <- SineWave.at state.TestFrequency state.Timer
        state.WriteHead <- state.WriteHead + 1


// Based on Dynamic Rate Control for Retro Game Emulators by Hans-Kristian Arntzen - https://github.com/libretro/docs/blob/master/archive/ratecontrol.pdf
let private calculateAdjustmentRatio (currentFill: int) : float =
    let fillRatio = (float (2 * currentFill - ringBufferSize) / float ringBufferSize)

    1.0 + fillRatio * drcGain

// TODO Experiment once I have working audio
// This will always resample the entire buffer range, meaning there can be a pitch drop beyond the DRC limit, but it reduces popping
let readResampledBuffer (state: Apu) (destination: float32 array) (outputSampleRate: int) =
    let adjustmentRatio = calculateAdjustmentRatio (state.WriteHead - state.ReadHead)
    let samplingRatio = float nativeSampleRate / float outputSampleRate
    let numApuSamples = int (adjustmentRatio * samplingRatio * float destination.Length)

    // Clamp to available samples, leaving one extra for interpolation lookahead
    let available = state.WriteHead - state.ReadHead
    let samplesToConsume = max 0 (min numApuSamples (available - 1))

    if samplesToConsume > 0 && destination.Length > 0 then
        let step = float samplesToConsume / float destination.Length

        for i = 0 to destination.Length - 1 do
            let pos = float i * step
            let index = int pos
            let frac = float32 (pos - float index)

            let s0 = state.RingBuffer[(state.ReadHead + index) &&& ringBufferModulo]
            let s1 = state.RingBuffer[(state.ReadHead + index + 1) &&& ringBufferModulo]

            destination[i] <- s0 + frac * (s1 - s0)

        state.ReadHead <- state.ReadHead + samplesToConsume
