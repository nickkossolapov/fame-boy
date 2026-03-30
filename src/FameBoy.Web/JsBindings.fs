module FameBoy.Web.JsBindings

open Fable.Core

type IResponse =
    abstract arrayBuffer: unit -> JS.Promise<JS.ArrayBuffer>

[<Global>]
let fetch (url: string) : JS.Promise<IResponse> = jsNative

type IAudioContext =
    abstract currentTime: float
    abstract destination: obj

type IGainNode = interface end
type IAudioBuffer = abstract duration: float
type IBufferSource = interface end

[<Emit("new AudioContext({sampleRate: $0})")>]
let createAudioContext (sampleRate: int) : IAudioContext = jsNative

[<Emit("$0.createGain()")>]
let createGain (ctx: IAudioContext) : IGainNode = jsNative

[<Emit("$0.gain.value = $1")>]
let setGainValue (gain: IGainNode) (value: float) : unit = jsNative

[<Emit("$0.connect($1)")>]
let connectGainTo (gain: IGainNode) (dest: obj) : unit = jsNative

[<Emit("$0.connect($1)")>]
let connectSourceTo (source: IBufferSource) (gain: IGainNode) : unit = jsNative

[<Emit("$0.createBuffer($1, $2, $3)")>]
let createBuffer (ctx: IAudioContext) (channels: int) (length: int) (sampleRate: int) : IAudioBuffer = jsNative

[<Emit("$0.getChannelData($1)")>]
let getChannelData (buffer: IAudioBuffer) (channel: int) : float32 array = jsNative

[<Emit("$0.createBufferSource()")>]
let createBufferSource (ctx: IAudioContext) : IBufferSource = jsNative

[<Emit("$0.buffer = $1")>]
let setBuffer (source: IBufferSource) (buffer: IAudioBuffer) : unit = jsNative

[<Emit("$0.start($1)")>]
let startSource (source: IBufferSource) (time: float) : unit = jsNative
