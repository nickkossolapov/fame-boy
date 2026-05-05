module FameBoy.Serial

open FameBoy.Interrupts
open FameBoy.Hardware
open FameBoy.IoController

let private cyclesPerByte = 128 * 8 // 1024 M-cycles (8192 Hz bit rate, 8 bits)

type SerialState =
    { mutable Counter: int
      mutable IsTransferring: bool
      /// Incoming byte from the link partner (0xFF if no partner)
      mutable IncomingByte: uint8
      /// Whether a linked partner has provided data for the current transfer
      mutable HasIncoming: bool
      /// Outgoing byte to send to the link partner
      mutable OutgoingByte: uint8
      /// Whether this device has initiated a transfer (master mode)
      mutable TransferInitiated: bool }

let createSerial () =
    { Counter = 0
      IsTransferring = false
      IncomingByte = 0xFFuy
      HasIncoming = false
      OutgoingByte = 0xFFuy
      TransferInitiated = false }

let stepSerial (state: SerialState) (io: IoController) =
    let sc = io.Registers[Io.Sc]
    let isMaster = sc &&& 0x01uy <> 0uy
    let transferRequested = sc &&& 0x80uy <> 0uy

    if state.IsTransferring then
        if isMaster then
            // Master drives the clock
            state.Counter <- state.Counter + 1

            if state.Counter = cyclesPerByte then
                state.Counter <- 0
                state.IsTransferring <- false

                // Exchange: receive incoming byte
                io.Registers[Io.Sb] <- state.IncomingByte
                state.HasIncoming <- false
                io.Registers[Io.Sc] <- sc &&& 0b0111_1111uy
                io.TriggerInterrupt InterruptType.Serial
        else
            // Slave: waiting for master to clock us - check if incoming data arrived
            if state.HasIncoming then
                state.IsTransferring <- false
                state.HasIncoming <- false

                io.Registers[Io.Sb] <- state.IncomingByte
                io.Registers[Io.Sc] <- sc &&& 0b0111_1111uy
                io.TriggerInterrupt InterruptType.Serial
    elif transferRequested then
        state.IsTransferring <- true
        state.Counter <- 0
        state.OutgoingByte <- io.Registers[Io.Sb]
        state.TransferInitiated <- true

/// Link cable simulation: connects two serial states together
/// Call this after stepping both emulators to exchange data
let exchangeSerial (serial1: SerialState) (io1: IoController) (serial2: SerialState) (io2: IoController) =
    let isMaster1 = serial1.TransferInitiated && serial1.IsTransferring && (io1.Registers[Io.Sc] &&& 0x01uy <> 0uy)
    let isMaster2 = serial2.TransferInitiated && serial2.IsTransferring && (io2.Registers[Io.Sc] &&& 0x01uy <> 0uy)

    // If both claim master, device 1 wins
    if isMaster1 then
        serial1.IncomingByte <- io2.Registers[Io.Sb]
        serial1.HasIncoming <- true
        serial2.IncomingByte <- serial1.OutgoingByte
        serial2.HasIncoming <- true
        serial1.TransferInitiated <- false
    elif isMaster2 then
        serial2.IncomingByte <- io1.Registers[Io.Sb]
        serial2.HasIncoming <- true
        serial1.IncomingByte <- serial2.OutgoingByte
        serial1.HasIncoming <- true
        serial2.TransferInitiated <- false
