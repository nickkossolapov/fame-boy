module FameBoy.Web.Audio

open FameBoy.Apu
open FameBoy.Web.JsBindings

[<Literal>]
let private samplingRate = 48000

[<Literal>]
let private defaultVolume = 0.4

// How far ahead to schedule audio buffers, in seconds
[<Literal>]
let private audioLeadTime = 0.035

let private bufferSize = samplingRate / 120
let private resampleBuffer = Array.zeroCreate<float32> bufferSize

type private AudioState =
    { mutable Ctx: IAudioContext option
      mutable Gain: IGainNode option
      mutable NextPlayTime: float
      mutable UserMuted: bool
      mutable Suppressed: bool
      mutable Initialized: bool }

type private FrameTracker =
    { History: bool array
      mutable Index: int
      mutable BadCount: int }

let private state =
    { Ctx = None
      Gain = None
      NextPlayTime = 0.0
      UserMuted = false
      Suppressed = false
      Initialized = false }

let private frameTracker =
    { History = Array.create 20 false
      Index = 0
      BadCount = 0 }

let private applyGain () =
    match state.Gain with
    | Some gain ->
        let shouldMute = state.UserMuted || state.Suppressed
        setGainValue gain (if shouldMute then 0.0 else defaultVolume)
    | None -> ()

let isUserMuted () = state.UserMuted

let ensureInitialized () =
    if not state.Initialized then
        state.Initialized <- true
        let ctx = createAudioContext samplingRate
        let gain = createGain ctx
        setGainValue gain defaultVolume
        connectGainTo gain ctx.destination
        state.Ctx <- Some ctx
        state.Gain <- Some gain

let resetPlayback () =
    state.NextPlayTime <- 0.0

let toggleMute () =
    state.UserMuted <- not state.UserMuted
    applyGain ()
    state.UserMuted

// Sliding window mute. If browser is out of focus performance drops a lot, so audio isn't great
let reportFrameTime (dt: float) =
    let windowSize = frameTracker.History.Length
    let isBad = dt > 25.0
    let wasBad = frameTracker.History[frameTracker.Index]

    frameTracker.History[frameTracker.Index] <- isBad
    frameTracker.Index <- (frameTracker.Index + 1) % windowSize

    if isBad && not wasBad then
        frameTracker.BadCount <- frameTracker.BadCount + 1
    elif not isBad && wasBad then
        frameTracker.BadCount <- frameTracker.BadCount - 1

    let shouldSuppress = frameTracker.BadCount >= 4

    if shouldSuppress <> state.Suppressed then
        state.Suppressed <- shouldSuppress
        applyGain ()

let tryQueueAudio (apu: Apu) stepEmulator fpsDriven =
    match state.Ctx, state.Gain with
    | Some ctx, Some gain ->
        let now = ctx.currentTime

        if state.NextPlayTime < now then
            state.NextPlayTime <- now

        let mutable keepScheduling = true

        while keepScheduling && state.NextPlayTime - now < audioLeadTime do
            if not fpsDriven then
                while samplesAvailable apu < nativeSamplesNeeded apu bufferSize samplingRate do
                    stepEmulator () |> ignore
            elif samplesAvailable apu < nativeSamplesNeeded apu bufferSize samplingRate then
                keepScheduling <- false

            if keepScheduling then
                readResampledBuffer apu resampleBuffer samplingRate

                let buffer = createBuffer ctx 1 bufferSize samplingRate
                let channelData = getChannelData buffer 0

                for i = 0 to bufferSize - 1 do
                    channelData[i] <- resampleBuffer[i]

                let source = createBufferSource ctx
                setBuffer source buffer
                connectSourceTo source gain
                startSource source state.NextPlayTime
                state.NextPlayTime <- state.NextPlayTime + buffer.duration
    | _ -> ()
