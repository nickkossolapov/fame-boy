module FameBoy.Apu

open System
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

[<Struct>]
type Direction =
    | Increasing
    | Decreasing

type Envelope =
    { mutable Volume: int
      mutable Direction: Direction
      mutable Pace: int
      mutable Timer: int }

type PulseChannel =
    { mutable DutyCycle: int
      mutable Frequency: int
      mutable LengthEnabled: bool
      mutable Timer: int
      mutable DutyStep: int
      mutable Enabled: bool
      Envelope: Envelope }

type Apu =
    { RingBuffer: float32 array
      Channel2: PulseChannel
      mutable WriteHead: int
      mutable ReadHead: int
      mutable Timer: int
      mutable Counter: int
      mutable SequencerStep: int }

let createApu () =
    let channel2 =
        { DutyCycle = 0
          Frequency = 0
          LengthEnabled = false
          Timer = 0
          DutyStep = 0
          Enabled = false
          Envelope =
            { Volume = 0
              Direction = Decreasing
              Pace = 0
              Timer = 0 } }

    { RingBuffer = Array.zeroCreate ringBufferSize
      Channel2 = channel2
      WriteHead = ringBufferSize / 2
      ReadHead = 0
      Timer = 0
      Counter = 0
      SequencerStep = 0 }

module private Helpers =
    let inline dac digital = (float32 digital / 7.5f) - 1.0f

    let stepEnvelope (env: Envelope) =
        if env.Pace > 0 then
            env.Timer <- env.Timer - 1

            if env.Timer <= 0 then
                env.Timer <- env.Pace

                let newVolume =
                    match env.Direction with
                    | Decreasing -> env.Volume - 1
                    | Increasing -> env.Volume + 1

                env.Volume <- Math.Clamp(newVolume, 0, 15)

open Helpers

module private Channel2 =
    // Each bit represents one step of the 8-step duty cycle
    // 0 = HIGH (output volume), 1 = LOW (output zero)
    let dutyCycles = [| 0b1111_1110; 0b0111_1110; 0b0111_1000; 0b1000_0001 |]

    let trigger (ch: PulseChannel) (io: IoController) =
        let nr21 = int io.Registers[Io.Nr21]
        let nr22 = int io.Registers[Io.Nr22]
        let nr23 = int io.Registers[Io.Nr23]
        let nr24 = int io.Registers[Io.Nr24]

        ch.DutyCycle <- (nr21 >>> 6) &&& 0b11
        ch.Frequency <- nr23 ||| ((nr24 &&& 0b0111) <<< 8)
        ch.LengthEnabled <- (nr24 &&& 0b0100_0000) <> 0
        ch.Timer <- (2048 - ch.Frequency) * 4
        ch.DutyStep <- 0
        ch.Enabled <- (nr22 &&& 0b1111_1000) <> 0

        ch.Envelope.Volume <- (nr22 >>> 4) &&& 0b1111
        ch.Envelope.Direction <- if (nr22 >>> 3) &&& 1 = 0 then Decreasing else Increasing
        ch.Envelope.Pace <- nr22 &&& 0b0111
        ch.Envelope.Timer <- ch.Envelope.Pace

        io.Registers[Io.Nr24] <- io.Registers[Io.Nr24] &&& 0b0111_1111uy

    let step (ch: PulseChannel) =
        ch.Timer <- ch.Timer - 1

        if ch.Timer <= 0 then
            ch.Timer <- (2048 - ch.Frequency) * 4
            ch.DutyStep <- (ch.DutyStep + 1) &&& 7

    let output (ch: PulseChannel) : float32 =
        if not ch.Enabled then
            0.0f
        else
            let pattern = dutyCycles[ch.DutyCycle]
            let bit = (pattern >>> ch.DutyStep) &&& 1
            let digital = if bit = 0 then ch.Envelope.Volume else 0

            dac digital

let stepSequencer (state: Apu) =
    if state.SequencerStep &&& 1 = 0 then
        () // TODO tick length counter
    else if state.SequencerStep = 7 then
        stepEnvelope state.Channel2.Envelope

    state.SequencerStep <- (state.SequencerStep + 1) &&& 7

let stepApu (state: Apu) (io: IoController) =
    if io.Registers[Io.Nr24] &&& 0b1000_0000uy <> 0uy then
        Channel2.trigger state.Channel2 io

    if state.Timer &&& 8191 = 0 then
        stepSequencer state

    Channel2.step state.Channel2

    state.Timer <- state.Timer + 1
    state.Counter <- state.Counter + 1

    if state.Counter >= tCyclesPerSample then
        state.Counter <- 0

        let i = state.WriteHead &&& ringBufferModulo
        state.RingBuffer[i] <- Channel2.output state.Channel2
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
