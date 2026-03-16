module FameBoy.Hardware

[<RequireQualifiedAccess>]
module Screen =
    [<Literal>]
    let height = 144

    [<Literal>]
    let width = 160

[<Literal>]
let cpuFrequency = 1048576 // M-Cycles

[<RequireQualifiedAccess>]
module IoRegisters =
    [<Literal>]
    let Joyp = 0xFF00us

    [<Literal>]
    let Sb = 0xFF01us

    [<Literal>]
    let Sc = 0xFF02us

    [<Literal>]
    let Div = 0xFF04us

    [<Literal>]
    let Tima = 0xFF05us

    [<Literal>]
    let Tma = 0xFF06us

    [<Literal>]
    let Tac = 0xFF07us

    [<Literal>]
    let If = 0xFF0Fus

    [<Literal>]
    let Nr10 = 0xFF10us

    [<Literal>]
    let Nr11 = 0xFF11us

    [<Literal>]
    let Nr12 = 0xFF12us

    [<Literal>]
    let Nr13 = 0xFF13us

    [<Literal>]
    let Nr14 = 0xFF14us

    [<Literal>]
    let Nr21 = 0xFF16us

    [<Literal>]
    let Nr22 = 0xFF17us

    [<Literal>]
    let Nr23 = 0xFF18us

    [<Literal>]
    let Nr24 = 0xFF19us

    [<Literal>]
    let Nr30 = 0xFF1Aus

    [<Literal>]
    let Nr31 = 0xFF1Bus

    [<Literal>]
    let Nr32 = 0xFF1Cus

    [<Literal>]
    let Nr33 = 0xFF1Dus

    [<Literal>]
    let Nr34 = 0xFF1Eus

    [<Literal>]
    let Nr41 = 0xFF20us

    [<Literal>]
    let Nr42 = 0xFF21us

    [<Literal>]
    let Nr43 = 0xFF22us

    [<Literal>]
    let Nr44 = 0xFF23us

    [<Literal>]
    let Nr50 = 0xFF24us

    [<Literal>]
    let Nr51 = 0xFF25us

    [<Literal>]
    let Nr52 = 0xFF26us

    [<Literal>]
    let Lcdc = 0xFF40us

    [<Literal>]
    let Stat = 0xFF41us

    [<Literal>]
    let Scy = 0xFF42us

    [<Literal>]
    let Scx = 0xFF43us

    [<Literal>]
    let Ly = 0xFF44us

    [<Literal>]
    let Lyc = 0xFF45us

    [<Literal>]
    let Dma = 0xFF46us

    [<Literal>]
    let Bgp = 0xFF47us

    [<Literal>]
    let Obp0 = 0xFF48us

    [<Literal>]
    let Obp1 = 0xFF49us

    [<Literal>]
    let Wy = 0xFF4Aus

    [<Literal>]
    let Wx = 0xFF4Bus

    [<Literal>]
    let Key0 = 0xFF4Cus

    [<Literal>]
    let Key1 = 0xFF4Dus

    [<Literal>]
    let Vbk = 0xFF4Fus

    [<Literal>]
    let Bank = 0xFF50us

    [<Literal>]
    let Hdma1 = 0xFF51us

    [<Literal>]
    let Hdma2 = 0xFF52us

    [<Literal>]
    let Hdma3 = 0xFF53us

    [<Literal>]
    let Hdma4 = 0xFF54us

    [<Literal>]
    let Hdma5 = 0xFF55us

    [<Literal>]
    let Rp = 0xFF56us

    [<Literal>]
    let Bcps = 0xFF68us

    [<Literal>]
    let Bcpd = 0xFF69us

    [<Literal>]
    let Ocps = 0xFF6Aus

    [<Literal>]
    let Ocpd = 0xFF6Bus

    [<Literal>]
    let Svbk = 0xFF70us

    [<Literal>]
    let Ie = 0xFFFFus


/// Used to directly access IO Registers instead of memory[addr] when it has special behaviour with CPU, e.g. read-only bits in byte
[<RequireQualifiedAccess>]
module IoRegisterOffsets =
    [<Literal>]
    let private memoryOffset = 0xFF00

    [<Literal>]
    let Joyp = 0xFF00 - memoryOffset

    [<Literal>]
    let Div = 0xFF04 - memoryOffset

    [<Literal>]
    let Stat = 0xFF41 - memoryOffset

    [<Literal>]
    let Ly = 0xFF44 - memoryOffset

    [<Literal>]
    let Dma = 0xFF46 - memoryOffset
