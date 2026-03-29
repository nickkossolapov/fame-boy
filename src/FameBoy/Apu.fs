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

type WaveChannel =
    { mutable RamIndex: int
      mutable Timer: int
      mutable Period: int
      mutable OutputLevel: int
      mutable Enabled: bool
      Length: Length }

type NoiseChannel =
    { mutable WideMode: bool // Wide is 15-bit LFSR, narrow is 7-bit LFSR
      mutable Lfsr: int
      mutable Timer: int
      mutable Period: int
      mutable Enabled: bool
      Length: Length
      Envelope: Envelope }

type HighPassFilter =
    { mutable LastIn: float32
      mutable LastOut: float32
      Alpha: float32 }

type LowPassFilter =
    { mutable LastOut: float32
      Alpha: float32 }

type Apu =
    { RingBuffer: float32 array
      RingBufferMask: int
      Channel1: SweepChannel
      Channel2: PulseChannel
      Channel3: WaveChannel
      Channel4: NoiseChannel
      HighPass: HighPassFilter
      LowPass: LowPassFilter
      mutable WriteHead: int
      mutable ReadHead: int
      mutable Timer: int
      mutable Counter: int
      mutable SequencerStep: int }

let createApu bufferSize =
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

    let createHighPass () =
        { LastIn = 0.0f
          LastOut = 0.0f
          Alpha = 0.996f }

    let createLowPass () = { LastOut = 0.0f; Alpha = 0.8f }

    { RingBuffer = Array.zeroCreate bufferSize
      RingBufferMask = bufferSize - 1
      Channel1 =
        { Pulse = createPulse ()
          Sweep = createSweep () }
      Channel2 = createPulse ()
      Channel3 =
        { RamIndex = 0
          Timer = 0
          Period = 0
          OutputLevel = 0
          Enabled = false
          Length = createLength () }
      Channel4 =
        { WideMode = true
          Lfsr = 0x7FFF
          Timer = 0
          Period = 0
          Enabled = false
          Length = createLength ()
          Envelope = createEnvelope () }
      HighPass = createHighPass ()
      LowPass = createLowPass ()
      WriteHead = bufferSize / 2
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

    let inline stepLength (ch: ^a when ^a: (member Length: Length) and ^a: (member Enabled: bool)) =
        if ch.Length.Enabled && ch.Length.Counter > 0 then
            ch.Length.Counter <- ch.Length.Counter - 1

            ch.Enabled && ch.Length.Counter <> 0
        else
            ch.Enabled

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

    let output (ch: PulseChannel) =
        if not ch.Enabled then
            0
        else
            let pattern = dutyCycles[ch.DutyCycle]
            let bit = (pattern >>> ch.DutyStep) &&& 1

            if bit = 0 then ch.Envelope.Volume else 0

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
        ch.Pulse.Enabled <- (nr12 &&& 0b1111_1000) <> 0

        ch.Sweep.Step <- nr10 &&& 0b0111
        ch.Sweep.Direction <- if (nr10 >>> 3) &&& 1 = 0 then Increasing else Decreasing
        ch.Sweep.Pace <- (nr10 >>> 4) &&& 0b0111
        ch.Sweep.Timer <- if ch.Sweep.Pace > 0 then ch.Sweep.Pace else 8

        ch.Pulse.Length.Counter <- 64
        ch.Pulse.Length.Enabled <- (nr14 &&& 0b0100_0000) <> 0

        ch.Pulse.Envelope.Volume <- (nr12 >>> 4) &&& 0b1111
        ch.Pulse.Envelope.Direction <- if (nr12 >>> 3) &&& 1 = 0 then Decreasing else Increasing
        ch.Pulse.Envelope.Pace <- nr12 &&& 0b0111
        ch.Pulse.Envelope.Timer <- ch.Pulse.Envelope.Pace

        io.Registers[Io.Nr14] <- io.Registers[Io.Nr14] &&& 0b0111_1111uy

    let step (ch: SweepChannel) = PulseChannel.step ch.Pulse

    let output (ch: SweepChannel) = PulseChannel.output ch.Pulse

    let private calcNewFreq (ch: SweepChannel) =
        let delta = ch.Pulse.Frequency >>> ch.Sweep.Step

        match ch.Sweep.Direction with
        | Increasing -> ch.Pulse.Frequency + delta
        | Decreasing -> ch.Pulse.Frequency - delta

    let private overflowCheck ch =
        let newFreq = calcNewFreq ch

        if newFreq > 2047 then
            ch.Pulse.Enabled <- false

    let stepSweep (ch: SweepChannel) (io: IoController) =
        let sweep = ch.Sweep

        sweep.Timer <- sweep.Timer - 1

        if sweep.Timer <= 0 then
            sweep.Timer <- if sweep.Pace > 0 then sweep.Pace else 8

            if sweep.Pace > 0 then
                let newFreq = calcNewFreq ch

                if newFreq > 2047 then
                    ch.Pulse.Enabled <- false
                elif newFreq >= 0 && sweep.Step > 0 then
                    ch.Pulse.Frequency <- newFreq
                    io.Registers[Io.Nr13] <- byte (newFreq &&& 0xFF)
                    io.Registers[Io.Nr14] <- (io.Registers[Io.Nr14] &&& 0b1111_1000uy) ||| byte ((newFreq >>> 8) &&& 0b0111)

                    if calcNewFreq ch > 2047 then
                        ch.Pulse.Enabled <- false

module private WaveChannel =
    let trigger (ch: WaveChannel) (io: IoController) =
        let nr30 = int io.Registers[Io.Nr30]
        let nr32 = int io.Registers[Io.Nr32]
        let nr33 = int io.Registers[Io.Nr33]
        let nr34 = int io.Registers[Io.Nr34]

        let frequency = nr33 ||| ((nr34 &&& 0b0111) <<< 8)

        ch.RamIndex <- 0
        ch.Period <- (2048 - frequency) * 2
        ch.Timer <- ch.Period
        ch.OutputLevel <- (nr32 &&& 0b0110_0000) >>> 5
        ch.Enabled <- (nr30 &&& 0b1000_0000) <> 0

        ch.Length.Counter <- 64 - int io.Registers[Io.Nr31]
        ch.Length.Enabled <- (nr34 &&& 0b0100_0000) <> 0

        io.Registers[Io.Nr34] <- io.Registers[Io.Nr34] &&& 0b0111_1111uy

    let step (ch: WaveChannel) =
        if ch.Enabled then
            ch.Timer <- ch.Timer - 1

            if ch.Timer <= 0 then
                ch.Timer <- ch.Period
                ch.RamIndex <- (ch.RamIndex + 1) &&& 0x1F

    let output (ch: WaveChannel) (io: IoController) =
        if not ch.Enabled || ch.OutputLevel = 0 then
            0
        else
            let byte = int io.Registers[Io.WaveRam + ch.RamIndex / 2]

            let nibble =
                if (ch.RamIndex % 2) = 0 then
                    (byte &&& 0b1111_0000) >>> 4
                else
                    (byte &&& 0b1111)

            nibble >>> (ch.OutputLevel - 1)

module private NoiseChannel =
    let private divisors = [| 8; 16; 32; 48; 64; 80; 96; 112 |]

    let private getPeriod nr43 =
        let divider = nr43 &&& 0b0111
        let shift = (nr43 >>> 4) &&& 0b1111

        divisors[divider] <<< shift

    let trigger (ch: NoiseChannel) (io: IoController) =
        let nr41 = int io.Registers[Io.Nr41]
        let nr42 = int io.Registers[Io.Nr42]
        let nr43 = (int io.Registers[Io.Nr43])
        let nr44 = int io.Registers[Io.Nr44]

        ch.Period <- getPeriod nr43
        ch.Timer <- ch.Period
        ch.Lfsr <- 0x7FFF
        ch.WideMode <- not (nr43 &&& 0b1000 <> 0)
        ch.Enabled <- (nr42 &&& 0b1111_1000) <> 0

        ch.Length.Counter <- 64 - (nr41 &&& 0b0011_1111)
        ch.Length.Enabled <- (nr44 &&& 0b0100_0000) <> 0

        ch.Envelope.Volume <- (nr42 >>> 4) &&& 0b1111
        ch.Envelope.Direction <- if (nr42 >>> 3) &&& 1 = 0 then Decreasing else Increasing
        ch.Envelope.Pace <- nr42 &&& 0b0111
        ch.Envelope.Timer <- ch.Envelope.Pace

        io.Registers[Io.Nr44] <- io.Registers[Io.Nr44] &&& 0b0111_1111uy

    let private stepLfsr value wideMode =
        let b0 = value &&& 1
        let b1 = (value >>> 1) &&& 1
        let feedback = b0 ^^^ b1

        let shifted = value >>> 1

        let res = (shifted &&& ~~~(1 <<< 14)) ||| (feedback <<< 14)

        if not wideMode then
            (res &&& ~~~(1 <<< 6)) ||| (feedback <<< 6)
        else
            res

    let step (ch: NoiseChannel) =
        if ch.Enabled then
            ch.Timer <- ch.Timer - 1

            if ch.Timer <= 0 then
                ch.Timer <- ch.Period
                ch.Lfsr <- stepLfsr ch.Lfsr ch.WideMode

    let output (ch: NoiseChannel) : float32 =
        if not ch.Enabled then
            dac 0
        else
            let bit = ch.Lfsr &&& 1
            let digital = if bit = 0 then ch.Envelope.Volume else 0

            dac digital

module private SoundProcessing =
    let stepHighPass (f: HighPassFilter) (input: float32) =
        let output = f.Alpha * (f.LastOut + input - f.LastIn)
        f.LastIn <- input
        f.LastOut <- output

        output

    let stepLowPass (f: LowPassFilter) (input: float32) =
        f.LastOut <- f.Alpha * input + (1.0f - f.Alpha) * f.LastOut
        f.LastOut

    let getMixedSample (state: Apu) (io: IoController) =
        let s1 = SweepChannel.output state.Channel1 |> dac
        let s2 = PulseChannel.output state.Channel2 |> dac
        let s3 = WaveChannel.output state.Channel3 io |> dac
        let s4 = NoiseChannel.output state.Channel4 |> dac

        let nr50 = int io.Registers[Io.Nr50]
        let nr51 = int io.Registers[Io.Nr51]

        let leftVol = float32 ((nr50 >>> 4) &&& 0b0111) / 7f
        let rightVol = float32 (nr50 &&& 0b0111) / 7f

        let left =
            (if nr51 &&& 0b0001_0000 <> 0 then s1 else 0f)
            + (if nr51 &&& 0b0010_0000 <> 0 then s2 else 0f)
            + (if nr51 &&& 0b0100_0000 <> 0 then s3 else 0f)
            + (if nr51 &&& 0b1000_0000 <> 0 then s4 else 0f)

        let right =
            (if nr51 &&& 0b0000_0001 <> 0 then s1 else 0f)
            + (if nr51 &&& 0b0000_0010 <> 0 then s2 else 0f)
            + (if nr51 &&& 0b0000_0100 <> 0 then s3 else 0f)
            + (if nr51 &&& 0b0000_1000 <> 0 then s4 else 0f)

        // Merge to mono - I'm not going to implement stereo for now, as it doesn't work will with headphones
        // I might revisit later and blend L+R, but for now I'm keeping the implementation simple
        (left * leftVol + right * rightVol) / 8f

open SoundProcessing

module private Apu =
    let stepSequencer (state: Apu) (io: IoController) =
        match state.SequencerStep with
        | 0
        | 2
        | 4
        | 6 ->
            // Length clocks at 256 Hz
            state.Channel1.Pulse.Enabled <- stepLength state.Channel1.Pulse
            state.Channel2.Enabled <- stepLength state.Channel2
            state.Channel3.Enabled <- stepLength state.Channel3
            state.Channel4.Enabled <- stepLength state.Channel4

            // Sweep clocks at 128 Hz (only on 2 and 6)
            if state.SequencerStep = 2 || state.SequencerStep = 6 then
                SweepChannel.stepSweep state.Channel1 io

        | 7 ->
            // Envelope clocks at 64 Hz
            stepEnvelope state.Channel1.Pulse.Envelope
            stepEnvelope state.Channel2.Envelope
            stepEnvelope state.Channel4.Envelope
        | _ -> ()

        state.SequencerStep <- (state.SequencerStep + 1) &&& 7


    let step (state: Apu) (io: IoController) =
        if io.Registers[Io.Nr14] &&& 0b1000_0000uy <> 0uy then
            SweepChannel.trigger state.Channel1 io

        if io.Registers[Io.Nr24] &&& 0b1000_0000uy <> 0uy then
            PulseChannel.trigger state.Channel2 io

        if io.Registers[Io.Nr34] &&& 0b1000_0000uy <> 0uy then
            WaveChannel.trigger state.Channel3 io

        if io.Registers[Io.Nr44] &&& 0b1000_0000uy <> 0uy then
            NoiseChannel.trigger state.Channel4 io

        state.Channel1.Pulse.Frequency <- int io.Registers[Io.Nr13] ||| ((int io.Registers[Io.Nr14] &&& 0b0111) <<< 8)
        state.Channel2.Frequency <- int io.Registers[Io.Nr23] ||| ((int io.Registers[Io.Nr24] &&& 0b0111) <<< 8)

        if state.Timer &&& 8191 = 0 then
            stepSequencer state io

        SweepChannel.step state.Channel1
        PulseChannel.step state.Channel2
        WaveChannel.step state.Channel3
        NoiseChannel.step state.Channel4

        state.Timer <- state.Timer + 1
        state.Counter <- state.Counter + 1

        if state.Counter >= tCyclesPerSample then
            state.Counter <- 0

            let rawSample = getMixedSample state io

            let filteredSample =
                rawSample |> stepHighPass state.HighPass |> stepLowPass state.LowPass

            let i = state.WriteHead &&& state.RingBufferMask
            state.RingBuffer[i] <- filteredSample
            state.WriteHead <- state.WriteHead + 1

    let getMasterControl state =
        let ch1 = if state.Channel1.Pulse.Enabled then 0b0001uy else 0uy
        let ch2 = if state.Channel2.Enabled then 0b0010uy else 0uy
        let ch3 = if state.Channel3.Enabled then 0b0100uy else 0uy
        let ch4 = if state.Channel4.Enabled then 0b1000uy else 0uy

        ch1 ||| ch2 ||| ch3 ||| ch4

let stepApu (state: Apu) (io: IoController) =
    if io.Registers[Io.Nr52] &&& 0b1000_0000uy <> 0uy then
        Apu.step state io

        io.Registers[Io.Nr52] <- 0b1000_0000uy ||| Apu.getMasterControl state

// Based on Dynamic Rate Control for Retro Game Emulators by Hans-Kristian Arntzen - https://github.com/libretro/docs/blob/master/archive/ratecontrol.pdf
let private calculateAdjustmentRatio (ringBufferSize: int) (currentFill: int) : float =
    let fillRatio = (float (2 * currentFill - ringBufferSize) / float ringBufferSize)

    1.0 + fillRatio * drcGain


// This will always resample the entire buffer range, meaning there can be a pitch drop beyond the DRC limit, but it reduces popping
// I should play around
let readResampledBuffer (state: Apu) (destination: float32 array) (outputSampleRate: int) =
    let available = state.WriteHead - state.ReadHead
    let adjustmentRatio = calculateAdjustmentRatio state.RingBuffer.Length available
    let samplingRatio = float nativeSampleRate / float outputSampleRate
    let numApuSamples = int (adjustmentRatio * samplingRatio * float destination.Length)
    let samplesToConsume = max 0 (min numApuSamples (available - 1)) // Clamp to available samples, leaving one extra for interpolation lookahead

    if samplesToConsume > 0 && destination.Length > 0 then
        let step = float samplesToConsume / float destination.Length

        for i = 0 to destination.Length - 1 do
            let pos = float i * step
            let index = int pos
            let frac = float32 (pos - float index)

            let s0 = state.RingBuffer[(state.ReadHead + index) &&& state.RingBufferMask]
            let s1 = state.RingBuffer[(state.ReadHead + index + 1) &&& state.RingBufferMask]

            destination[i] <- s0 + frac * (s1 - s0)

        state.ReadHead <- state.ReadHead + samplesToConsume
