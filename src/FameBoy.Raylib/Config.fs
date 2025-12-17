[<RequireQualifiedAccess>]
module FameBoy.Raylib.Config

open FameBoy.Hardware

[<Literal>]
let scale = 2

[<Literal>]
let enableDebugView = true

[<Literal>]
let width =
    if enableDebugView then
        Screen.width + 256 + 1
    else
        Screen.width

[<Literal>]
let height = if enableDebugView then 256 + 96 + 1 else Screen.height
