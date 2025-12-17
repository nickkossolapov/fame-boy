module FameBoy.Raylib.Utils

open System.Diagnostics

module RateLimiting =
    let rateLimitFunc (time: int) (func: 'a -> unit) =
        let stopwatch = Stopwatch ()
        let mutable lastPrint = 0L

        fun p ->
            if not stopwatch.IsRunning then
                stopwatch.Start ()

            let now = stopwatch.ElapsedMilliseconds

            if now - lastPrint >= time then
                lastPrint <- now
                func p
