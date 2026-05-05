module FameBoy.Interrupts


[<RequireQualifiedAccess; Struct>]
type InterruptType =
    | VBlank
    | LcdStat
    | Timer
    | Serial
    | Joypad

[<Literal>]
let private VBlankVector = 0x0040us

[<Literal>]
let private LcdStatVector = 0x0048us

[<Literal>]
let private TimerVector = 0x0050us

[<Literal>]
let private SerialVector = 0x0058us

[<Literal>]
let private JoypadVector = 0x0060us


let getVector =
    function
    | InterruptType.VBlank -> VBlankVector
    | InterruptType.LcdStat -> LcdStatVector
    | InterruptType.Timer -> TimerVector
    | InterruptType.Serial -> SerialVector
    | InterruptType.Joypad -> JoypadVector

let getInterruptMask =
    function
    | InterruptType.VBlank -> 0b00000001uy
    | InterruptType.LcdStat -> 0b00000010uy
    | InterruptType.Timer -> 0b00000100uy
    | InterruptType.Serial -> 0b00001000uy
    | InterruptType.Joypad -> 0b00010000uy

let getInterruptForPending pending =
    if pending &&& 0x01uy <> 0uy then InterruptType.VBlank
    elif pending &&& 0x02uy <> 0uy then InterruptType.LcdStat
    elif pending &&& 0x04uy <> 0uy then InterruptType.Timer
    elif pending &&& 0x08uy <> 0uy then InterruptType.Serial
    else InterruptType.Joypad
