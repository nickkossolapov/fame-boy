module FameBoy.Hardware

[<RequireQualifiedAccess>]
module Screen =
    [<Literal>]
    let height = 144

    [<Literal>]
    let width = 160

[<Literal>]
let cpuFrequency = 1048576 // M-Cycles

[<Literal>]
let CartRomBankSize = 0x4000

[<Literal>]
let CartRamBankSize = 0x2000


/// Offsets, all IO registers' address will start with 0xFF
[<RequireQualifiedAccess>]
module Io =
    [<Literal>]
    let IoMemoryOffset = 0xFF00

    [<Literal>]
    let Joyp = 0x00

    [<Literal>]
    let Sb = 0x01

    [<Literal>]
    let Sc = 0x02

    [<Literal>]
    let Div = 0x04

    [<Literal>]
    let Tima = 0x05

    [<Literal>]
    let Tma = 0x06

    [<Literal>]
    let Tac = 0x07

    [<Literal>]
    let If = 0x0F

    [<Literal>]
    let Nr10 = 0x10

    [<Literal>]
    let Nr11 = 0x11

    [<Literal>]
    let Nr12 = 0x12

    [<Literal>]
    let Nr13 = 0x13

    [<Literal>]
    let Nr14 = 0x14

    [<Literal>]
    let Nr21 = 0x16

    [<Literal>]
    let Nr22 = 0x17

    [<Literal>]
    let Nr23 = 0x18

    [<Literal>]
    let Nr24 = 0x19

    [<Literal>]
    let Nr30 = 0x1A

    [<Literal>]
    let Nr31 = 0x1B

    [<Literal>]
    let Nr32 = 0x1C

    [<Literal>]
    let Nr33 = 0x1D

    [<Literal>]
    let Nr34 = 0x1E

    [<Literal>]
    let Nr41 = 0x20

    [<Literal>]
    let Nr42 = 0x21

    [<Literal>]
    let Nr43 = 0x22

    [<Literal>]
    let Nr44 = 0x23

    [<Literal>]
    let Nr50 = 0x24

    [<Literal>]
    let Nr51 = 0x25

    [<Literal>]
    let Nr52 = 0x26

    [<Literal>]
    let Lcdc = 0x40

    [<Literal>]
    let Stat = 0x41

    [<Literal>]
    let Scy = 0x42

    [<Literal>]
    let Scx = 0x43

    [<Literal>]
    let Ly = 0x44

    [<Literal>]
    let Lyc = 0x45

    [<Literal>]
    let Dma = 0x46

    [<Literal>]
    let Bgp = 0x47

    [<Literal>]
    let Obp0 = 0x48

    [<Literal>]
    let Obp1 = 0x49

    [<Literal>]
    let Wy = 0x4A

    [<Literal>]
    let Wx = 0x4B

    [<Literal>]
    let Key0 = 0x4C

    [<Literal>]
    let Key1 = 0x4D

    [<Literal>]
    let Vbk = 0x4F

    [<Literal>]
    let Bank = 0x50

    [<Literal>]
    let Hdma1 = 0x51

    [<Literal>]
    let Hdma2 = 0x52

    [<Literal>]
    let Hdma3 = 0x53

    [<Literal>]
    let Hdma4 = 0x54

    [<Literal>]
    let Hdma5 = 0x55

    [<Literal>]
    let Rp = 0x56

    [<Literal>]
    let Bcps = 0x68

    [<Literal>]
    let Bcpd = 0x69

    [<Literal>]
    let Ocps = 0x6A

    [<Literal>]
    let Ocpd = 0x6B

    [<Literal>]
    let Svbk = 0x70
