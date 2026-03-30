module FameBoy.Web.JsBindings

open Fable.Core


[<Emit("new AudioContext({sampleRate: $0})")>]
let createAudioContext (_: int) : obj = jsNative

[<Emit("$0.createGain()")>]
let createGain (ctx: obj) : obj = jsNative

[<Emit("$0.gain.value = $1")>]
let setGainValue (gain: obj) (value: float) : unit = jsNative

[<Emit("$0.connect($1)")>]
let connectNode (source: obj) (dest: obj) : unit = jsNative

[<Emit("$0.destination")>]
let destination (ctx: obj) : obj = jsNative

[<Emit("$0.createBuffer($1, $2, $3)")>]
let createBuffer (ctx: obj) (channels: int) (length: int) (sampleRate: int) : obj = jsNative

[<Emit("$0.getChannelData($1)")>]
let getChannelData (buffer: obj) (channel: int) : float32 array = jsNative

[<Emit("$0.duration")>]
let bufferDuration (buffer: obj) : float = jsNative

[<Emit("$0.createBufferSource()")>]
let createBufferSource (ctx: obj) : obj = jsNative

[<Emit("$0.buffer = $1")>]
let setBuffer (source: obj) (buffer: obj) : unit = jsNative

[<Emit("$0.start($1)")>]
let startSource (source: obj) (time: float) : unit = jsNative

[<Emit("$0.currentTime")>]
let currentTime (ctx: obj) : float = jsNative
