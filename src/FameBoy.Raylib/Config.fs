[<RequireQualifiedAccess>]
module FameBoy.Raylib.Config

open System.Diagnostics
open FameBoy.Hardware

let mutable scale = 4

let mutable fullscreen = false

let mutable linkMode = false

let enableDebugView = Debugger.IsAttached

let width =
    if enableDebugView then
        Screen.width + 256 + 1
    else
        Screen.width

let height = if enableDebugView then 256 + 96 + 1 else Screen.height
