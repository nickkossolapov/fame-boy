using System;
using System.Runtime.InteropServices.JavaScript;
using FameBoy;
using FameBoy.Graphics;
using Microsoft.FSharp.Core;

Console.WriteLine("Hello, Browser!");

internal class EmulatorContainer(Ppu.Shade[] framebuffer, Func<int> step)
{
    private readonly int[] _outputFramebuffer = new int[framebuffer.Length];

    public void Step()
    {
        step();
    }

    public int[] GetOutputFramebuffer()
    {
        for (var i = 0; i < framebuffer.Length; i++)
        {
            var (r, g, b) = framebuffer[i] switch
            {
                Ppu.Shade.White => (186, 218, 85),
                Ppu.Shade.Light => (130, 153, 59),
                Ppu.Shade.Dark => (74, 87, 34),
                Ppu.Shade.Black => (19, 22, 8),
                _ => (0, 0, 0)
            };

            const int a = 255 << 24;

            _outputFramebuffer[i] =
                a |
                (b << 16) |
                (g << 8) |
                r;
        }

        return _outputFramebuffer;
    }
}

internal partial class EmulatorInterop
{
    private static readonly Joypad.JoypadState _joypad = new(false, false, false, false, false, false, false, false);
    private static EmulatorContainer? _emulator;

    [JSExport]
    internal static void Init(byte[] bytes)
    {
        var (framebuffer, _, step) = Emulator.createEmulator(bytes, FuncConvert.FromFunc(() => _joypad));

        _emulator = new EmulatorContainer(framebuffer, () => step.Invoke(null));
        Console.Write($"Read {bytes.Length} bytes");
    }

    [JSExport]
    internal static int[] Step(int steps)
    {
        for (var i = 0; i < steps; i++) _emulator?.Step();

        return _emulator?.GetOutputFramebuffer() ?? [];
    }

    // [JSImport("emulator.getJoypadState", "main.js")]
    // internal static partial byte GetJoypadState(); // bool[] is not supported by .NET-JS interop
}