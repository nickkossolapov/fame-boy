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

type Sweep =
    { mutable Step: int
      mutable Direction: Direction
      mutable Pace: int
      mutable Timer: int }

type Length =
    { mutable Counter: int
      mutable Enabled: bool }

type PulseChannel =
    { mutable DutyCycle: int
      mutable Frequency: int
      mutable Timer: int
      mutable DutyStep: int
      mutable Enabled: bool
      Length: Length
      Envelope: Envelope }

type SweepChannel = { Pulse: PulseChannel; Sweep: Sweep }

type Apu =
    { RingBuffer: float32 array
      Channel1: SweepChannel
      Channel2: PulseChannel
      mutable WriteHead: int
      mutable ReadHead: int
      mutable Timer: int
      mutable Counter: int
      mutable SequencerStep: int }

let createApu () =
    let createLength () = { Counter = 0; Enabled = false }

    let createEnvelope () =
        { Volume = 0
          Direction = Decreasing
          Pace = 0
          Timer = 0 }

    let createPulse () =
        { DutyCycle = 0
          Frequency = 0
          Timer = 0
          DutyStep = 0
          Enabled = false
          Length = createLength ()
          Envelope = createEnvelope () }

    let createSweep () =
        { Step = 0
          Direction = Increasing
          Pace = 0
          Timer = 0 }

    { RingBuffer = Array.zeroCreate ringBufferSize
      Channel1 =
        { Pulse = createPulse ()
          Sweep = createSweep () }
      Channel2 = createPulse ()
      WriteHead = ringBufferSize / 2
      ReadHead = 0
      Timer = 0
      Counter = 0
      SequencerStep = 0 }

module private Shared =
    // Each bit represents one step of the 8-step duty cycle
    // 0 = HIGH (output volume), 1 = LOW (output zero)
    let dutyCycles = [| 0b1111_1110; 0b0111_1110; 0b0111_1000; 0b1000_0001 |]

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

    let stepLength (len: Length) =
        if len.Enabled && len.Counter > 0 then
            len.Counter <- len.Counter - 1

            len.Counter = 0
        else
            false

open Shared

module private PulseChannel =
    let trigger (ch: PulseChannel) (io: IoController) =
        let nr21 = int io.Registers[Io.Nr21]
        let nr22 = int io.Registers[Io.Nr22]
        let nr23 = int io.Registers[Io.Nr23]
        let nr24 = int io.Registers[Io.Nr24]

        ch.DutyCycle <- (nr21 >>> 6) &&& 0b11
        ch.Frequency <- nr23 ||| ((nr24 &&& 0b0111) <<< 8)
        ch.Timer <- (2048 - ch.Frequency) * 4
        ch.DutyStep <- 0
        ch.Enabled <- (nr22 &&& 0b1111_1000) <> 0

        ch.Length.Counter <- 64
        ch.Length.Enabled <- (nr24 &&& 0b0100_0000) <> 0

        ch.Envelope.Volume <- (nr22 >>> 4) &&& 0b1111
        ch.Envelope.Direction <- if (nr22 >>> 3) &&& 1 = 0 then Decreasing else Increasing
        ch.Envelope.Pace <- nr22 &&& 0b0111
        ch.Envelope.Timer <- ch.Envelope.Pace

        io.Registers[Io.Nr24] <- io.Registers[Io.Nr24] &&& 0b0111_1111uy

    let step (ch: PulseChannel) =
        if ch.Enabled then
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

module private SweepChannel =
    let trigger (ch: SweepChannel) (io: IoController) =
        let nr10 = int io.Registers[Io.Nr10]
        let nr11 = int io.Registers[Io.Nr11]
        let nr12 = int io.Registers[Io.Nr12]
        let nr13 = int io.Registers[Io.Nr13]
        let nr14 = int io.Registers[Io.Nr14]

        ch.Pulse.DutyCycle <- (nr11 >>> 6) &&& 0b0011
        ch.Pulse.Frequency <- nr13 ||| ((nr14 &&& 0b0111) <<< 8)
        ch.Pulse.Timer <- (2048 - ch.Pulse.Frequency) * 4
        ch.Pulse.DutyStep <- 0
        ch.Pulse.Enabled <- (nr12 &&& 0b1111_1000) <> 0

        ch.Sweep.Step <- nr10 &&& 0b0111
        ch.Sweep.Direction <- if (nr10 >>> 3) &&& 1 = 0 then Increasing else Decreasing
        ch.Sweep.Pace <- (nr10 >>> 4) &&& 0b0111
        ch.Sweep.Timer <- ch.Sweep.Pace

        ch.Pulse.Length.Counter <- 64
        ch.Pulse.Length.Enabled <- (nr14 &&& 0b0100_0000) <> 0

        ch.Pulse.Envelope.Volume <- (nr12 >>> 4) &&& 0b1111
        ch.Pulse.Envelope.Direction <- if (nr12 >>> 3) &&& 1 = 0 then Decreasing else Increasing
        ch.Pulse.Envelope.Pace <- nr12 &&& 0b0111
        ch.Pulse.Envelope.Timer <- ch.Pulse.Envelope.Pace

        io.Registers[Io.Nr14] <- io.Registers[Io.Nr14] &&& 0b0111_1111uy

    let step (ch: SweepChannel) = PulseChannel.step ch.Pulse

    let output (ch: SweepChannel) : float32 = PulseChannel.output ch.Pulse

    let private calcNewFreq (ch: SweepChannel) =
        let delta = ch.Pulse.Frequency >>> ch.Sweep.Step

        match ch.Sweep.Direction with
        | Increasing -> ch.Pulse.Frequency + delta
        | Decreasing -> ch.Pulse.Frequency - delta

    let private overflowCheck ch =
        let newFreq = calcNewFreq ch

        if newFreq > 2047 then
            ch.Pulse.Enabled <- false

    let stepSweep (ch: SweepChannel) =
        let sweep = ch.Sweep

        // Overflow check always runs
        overflowCheck ch

        if sweep.Pace > 0 then
            sweep.Timer <- sweep.Timer - 1

            if sweep.Timer <= 0 then
                sweep.Timer <- sweep.Pace

                let newFreq = calcNewFreq ch

                if newFreq <= 2047 && newFreq >= 0 then
                    ch.Pulse.Frequency <- newFreq

let stepSequencer (state: Apu) =
    if state.SequencerStep &&& 1 = 0 then
        if state.SequencerStep &&& 0b0010 <> 0 then // Sweep only ticks on 2 and 6
            SweepChannel.stepSweep state.Channel1

        if stepLength state.Channel1.Pulse.Length then
            state.Channel1.Pulse.Enabled <- false

        if stepLength state.Channel2.Length then
            state.Channel2.Enabled <- false
    else if state.SequencerStep = 7 then
        stepEnvelope state.Channel1.Pulse.Envelope
        stepEnvelope state.Channel2.Envelope

    state.SequencerStep <- (state.SequencerStep + 1) &&& 7

let private simpleMix ch1 ch2 =
    (PulseChannel.output ch1.Pulse + PulseChannel.output ch2) / 2f


let stepApu (state: Apu) (io: IoController) =
    if io.Registers[Io.Nr14] &&& 0b1000_0000uy <> 0uy then
        SweepChannel.trigger state.Channel1 io

    if io.Registers[Io.Nr24] &&& 0b1000_0000uy <> 0uy then
        PulseChannel.trigger state.Channel2 io

    if state.Timer &&& 8191 = 0 then
        stepSequencer state

    SweepChannel.step state.Channel1
    PulseChannel.step state.Channel2

    state.Timer <- state.Timer + 1
    state.Counter <- state.Counter + 1

    if state.Counter >= tCyclesPerSample then
        state.Counter <- 0

        let i = state.WriteHead &&& ringBufferModulo
        state.RingBuffer[i] <- simpleMix state.Channel1 state.Channel2
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
